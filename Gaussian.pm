=head1 NAME

PDL::Gaussian -- Gaussian distributions.

=head1 DESCRIPTION

This package provides a set of standard routines to handle 
sets gaussian distributions.

A new set of gaussians is initialized by 

	$a = new PDL::Gaussian(xdims,gdims);

Where I<xdims> is a reference to an array containing the 
dimensions in the space the gaussian
is in and I<gdimslist> is a reference to an array containing 
the dimensionality of the gaussian space. For example, after

	$a = new PDL::Gaussian([2],[3,4]);
	$b = new PDL::Gaussian([],[]);

The variable  $a contains set of 12 (=3*4) 2-Dimensional gaussians
and $b is the simplest form: one 1D gaussian.
Currently, I<xdims> may containe either zero or one dimensions 
due to limitations of PDL::PP.

To set the distribution parameters, you can use the routines

	$a->set_covariance($cv);		# covariance matrices
	$a->set_icovariance($icv);		# inverse covariance matrices
	$a->set_mu($mu);			# centers

The dimensions of $cv and $icv must be (@xdims,@xdims,@gdims) and
the dimensions of $mu must be (@xdims,@gdims).

To get a string representation of the gaussians (most useful for
debugging) use the routine
	
	$string = $a->asstr();

It is possible to calculate the probability or logarithm of probability
of each of the distributions at some points.

	$a->calc_value($x,$p);
	$a->calc_lnvalue($x,$p);

Here, $x must have dimensions (ndims,...) and $p must have dimensions
(gdimslist, ...) where the elipsis represents the same dimensions in 
both variables. It is usually advisable to work with the logarithms
of probabilities to avoid numerical problems.

It is possible to generate the parameters for the gaussians from data.
The function

	$a->fromweighteddata($data,$wt,$small_covariance);

where data is of dimensions (ndims,npoints) and wt is of dimensions
(npoints,gdimslist), analyzes the data statistically and gives
a corresponding gaussian distribution. The parameter $small_covariance
is the smallest allowed covariance in any direction: if one or more of 
the eigenvalues of the covariance matrix are smaller than this, they
are automatically set to $small_covariance to avoid singularities.

=head1 BUGS

DOESN'T REALLY WORK YET

Stupid interface.

Limitation to 0 or 1 x-dimensions is questionable (although 
it's hard to imagine a case when more is needed).

=head1 AUTHOR

Copyright (C) 1996 Tuomas J. Lukka (lukka@fas.harvard.edu)

=cut

package PDL::Gaussian;


use PDL::OO ;
use PDL::Slatec;
use PDL::NBasic;
use PDL::Thread;
use PDL::Experiment;
use PDL::Misc;
use PDL::Primitive;
use Carp;

sub new {
	my($type,$ndims,$nfuncs) = @_;
	if($#{$ndims} > 0) {
		confess("PDL::Gaussian can only have one dimension dimensionality\n");
	}
	my $ndims1 = ($#ndims==0 ? $ndims : [1]);
	bless {
		Mu => (PDL->zeroes (@$ndims1,@$nfuncs)->double),
		ICV => (PDL->zeroes (@$ndims1,@$ndims1,@$nfuncs)->double),
		CV => (PDL->zeroes (@$ndims1,@$ndims1,@$nfuncs)->double),
		lnPrefactor=> (PDL->zeroes(@$nfuncs)->double),
		EigVec => (PDL->zeroes (@$ndims1,@$ndims1,@$nfuncs)->double),
		EigVal => (PDL->zeroes (@$ndims1,@$nfuncs)->double),
		NDims => $ndims,
		NFuncs => $nfuncs,
	},$type;
}

sub dummize { my($this,$pdl,$ind) = @_;
	if($#{$this->{NDims}} >= 0) {
		return $pdl;
	} else {
		confess ("Dummization (0 dims) gaussian not implemented");
		my $new =  $pdl->_shallowcopy();
		$new->dummy($ind);
		return $new;
	}
}

sub dummize2 {my($this,$pdl,$ind) = @_;
	return $this->dummize($this->dummize($pdl,$ind),$ind+1);
}

sub asstr {
	my($this) = @_;
	return join '', 
	  "Gaussian: NDims = $this->{NDims}[0], NFuncs = ",
	  	(join ',',@{$this->{NFuncs}}),
	  "\nCoVar: $this->{CV}, ICoVar: $this->{IVC},
pref: $this->{lnPrefactor}, Eigvec: $this->{EigVec}, eigval: $this->{EigVal}
";
}

sub set_covariance {
	my($this,$cv) = @_;
	similar_assign($cv,$this->dummize2($this->{CV},0));
	$this->xxx_covariance();
}

sub set_icovariance {
	my($this,$cv) = @_;
	similar_assign($cv,$this->dummize2($this->{ICV},0));
	$this->xxx_icovariance();
}

sub set_mu {
	my($this,$mu) = @_;
	similar_assign($mu,$this->dummize($this->{Mu},0));
}

sub xxx_covariance {
	my($this)=@_;
## invert - this should be encapsulated
#	my_biop($this->{CV},$this->{ICV},"NOTHING");
#	my $rcond = float pdl 0; my $z = float zeroes $this->{Ndims};
#	my $info = long zeroes(1,@{$this->{NFuncs}});
#	poco($this->{ICV}, $rcond, $z, $info);
## Should check info for luck at inversion
#	my $job = long pdl 11; # Determinant + inverse
#	my $det = float zeroes(2,@{$this->{NFuncs}});
#	podi($this->{ICV},$job,$det);
## Now, 
	$this->_eigs($this->{CV});
	$this->_pref();
# Calc.
	$this->_otrans(1);
}

sub xxx_icovariance {
	my($this) = @_;
	$this->_eigs($this->{ICV});
	$this->{EigVal} **= -1;
	$this->_pref();
	$this->_otrans(0);
}

sub _eigs {
	my($this,$mat) = @_;
	my $fvone = (PDL->zeroes(@{$this->{NDims}}))->float;
	my $fvtwo = (PDL->zeroes(@{$this->{NDims}}))->float;
	my $ierr = (PDL->zeroes(@{$this->{NFuncs}}))->long;
	my $tmp = $mat->float; # Copy, since is destroyed.
	my $tmpval = $this->{EigVal}->float;
	my $tmpvec = $this->{EigVec}->float;
	print "FVONEDIMS\n"; $fvone->printdims();
	rs($tmp, $tmpval, $tmpvec, $ierr, $fvone, $fvtwo, 1);
	$this->{EigVal} = $tmpval->double;
	$this->{EigVec} = $tmpvec->double;
	print "OEIGS\n";
}

sub _otrans {
	my($this,$inv) = @_;
	my $tmp = PDL-> zeroes( @{$this->{CV}{Dims}})->double;
	my_ufunc2($this->{EigVec},$tmp,"NOTHING"); # CV = Eigvec
	my_biop1($tmp->thread(0),$this->{EigVal},$tmp->thread(0),
		($inv?"/":"*")); # * eigval
	print "OTRANS: Tmp = $tmp, inv = $inv\n";
	inner($this->{EigVec}->thread(0,-1),$tmp->thread(-1,0),
		$this->{($inv?"ICV":"CV")}->thread(0,1));
}

# Calculate prefactor.
sub _pref {
	my($this) = @_;
	my $tmp = (log($this->{EigVal}));
	sumover($tmp,$this->{lnPrefactor});
	$this->{lnPrefactor} *= -0.5;
	$this->{lnPrefactor} -= 0.5 * $this->{NDims} * log (2*3.14);
}

# (nvars) => (@xdims)
sub calc_value ($$$) {
	my($this,$x,$p) = @_;
	$this->calc_lnvalue($this->dummize($x,0),$p);
	exp(inplace $p);
}

# (nvars,foo) => (xdims,foo)
sub calc_lnvalue ($$$) {
	my($this,$xorig,$p) = @_;
	my $x = $this->dummize($xorig,0);
	my $muxed = (PDL->zeroes(@{$this->{NDims}},@{$p->{Dims}}))->double;

	print "MUXED1: $muxed\n";

	my $arg11 = $this->{Mu}->thread(1..$#{$this->{NFuncs}}+1);
	my $arg12 = $muxed->thread(1..$#{$this->{NFuncs}}+1);

#	my_biop1($x,$this->{Mu}->thread(1..$#{$this->{NFuncs}}+1),
#		$muxed->thread(1..$#{$this->{NFuncs}}+1),"-");

	my_biop1($x, $arg11, $arg12, "-");

	print "TOINNER2\n";
	print "MUXED: $muxed\n";
	print "TOINNER2\n";
	my $arg1 = ($muxed->thread(1..$#{$this->{NFuncs}}+1));
	print "TOINNER3\n";
	my $arg2 = ($this->{ICV}->thread(2..$#{$this->{ICV}{Dims}}));
	print "TOINNER4\n";
	my $arg3 = ($p->thread(0..$#{$this->{NFuncs}}));
	print "TOINNER5\n";
#	inner2(($muxed->thread(1..$#{$this->{NFuncs}}+1))
#		,($this->{ICV}->thread(2..$#{$this->{ICV}{Dims}})),
#		($muxed->thread(1..$#{$this->{NFuncs}}+1))
#		   ($p->thread(0..$#{$this->{NFuncs}})));
	inner2($arg1,$arg2,$arg1,$arg3);
	print "FROMINNER2\n";
	$p /= -2;
	print "TON3\n";
	my_biop1($p,$this->{lnPrefactor},$p,"+");
	print "OUTON3\n";
}

# Again, (nvars,newndims,foo) => (newndims,newndims,@xdims,foo)
# This doesn't make sense when dummized
sub calc_lccovariance {
	my($this,$vec,$var) = @_;
	my $tmp = 
	calc_xax($vec, $this->{CV}->thread(2..$#{$this->{CV}{Dims}}), 
		$var->thread(0..$#{$this->{NFuncs}}));
}

# (nvars,newndims,foo) => (newndims,@gdims,foo)
# (nvars,@xdims)->thread) -> (@gdims)
# This neither make sense when dummized
sub calc_lcavg {
	my($this,$vec,$var) = @_;
	inner($vec->thread(0,map {-1} (0..$#{$this->{NFuncs}})),
		$this->{Mu}->thread(-1,1..$#{$this->{Mu}{Dims}}),
		$var->thread(0,1..$#{$this->{NFuncs}}+1));
}

# (nvars,newndims,foo) => (other gaussian) (newndims,@xdims,foo)
# (only for 1-D gaussians)
# This neither.
sub to_lcombgaussians {
	my($this,$vec,$gauss) = @_;
	calc_lccovariance($vec,$gauss->{CV});
	calc_lcavg($vec,$gauss->{Mu});
	$gauss->xxx_covariance();
}

# (nvars,ndata), (ndata,@xdims)
sub fromweighteddata {
	my($this,$data,$wt) = @_;
}

sub ph {my($a) = @_; for (keys %$a) {next if !ref $a->{$_} or 
	(ref $a->{$_}) eq "ARRAY";
   print "$_ :",$a->{$_},"\n	Dims:[",
	(join ',',@{$a->{$_}{Dims}}),"]\n";}}

1;



