=head1 NAME

PDL::Thread -- Funny PDL index manipulations

=head1 DESCRIPTION

This package provides several routines to be used with PDL xsubs
compiled with PDL::PP. PDL::PP-compiled routines know how to interpret
the fields C<Incs, ThreadDims, ThreadIncs> and C<Offs> which makes 
these routines possible.

=cut

package PDL;
use Carp;

use strict;

=head2 map

	$a->map('ind0,ind1...')

This function takes a rectangular slice of $a. The part "ind0" specifies
what happens to the index 0 etc. The specification can have the following
forms:

=over 4

=item :

The whole dimension i.e. don't slice

=item number

Take only the values at "number" so the dimension of this index becomes 1.

=item (number)

Same as the previous but additionally, remove the index.

=item number1:number2

Take the range number1 to number2

=item number1:number2:number3

Take the range number1 to number2 with step number3.

=item *

Add a new dummy index of width 1.

=back

=cut

sub map {
	my $this = shift;
	my $str = shift;
	my @inds = split ',',$str;
	my @parlist;
	my @dummies;
	my @newdummies;
	my $ind;
	my $nnewdummies = 0;
	for $ind (0..$#inds) {
		my $nthreal = $ind - $nnewdummies;
		my $size = $this->{Dims}[$ind]; 
		$_ = $inds[$ind];
# Check for special cases
		if(/^\*$/) {
			$nnewdummies ++;
			push @newdummies, $nthreal;
			next;
		}
# Now parse. Alternative slices:
		my (@p) = 
		(/^:$/ ? (-1) :
		/^([0-9]+)$/ ? (($1,1,$1)) : 
		/^\(([0-9]+)\)$/ ? (((push @dummies,$ind)and()) , ($1,1,$1)) : 
		/^([0-9]+):([0-9]+)$/ ? ($1,1,$2) :
		/^([0-9]+):([0-9]+):([0-9]+)$/ ? ($1,$3,$2) :
		croak ("Invalid index given to map: $_\n") );
		if($p[0] != -1) { push @parlist,($nthreal,@p) }
	}
	print "SLICING ",(join ',',@parlist),"\n";
	my $a = $this->_slice(@parlist);
	$a->_adddummies(@newdummies);
	$a->_obliterate(@dummies);
	$a;
}

=head2 thread

	$a->thread(indices)

This command is used to do "explicit threading". See L<PDL::PP> for
background on threading. Explicit threading requires there to be
matching number of thread indices for each of the arguments to the function,
so

	func($a->thread(1),$b->thread(2,3));

is currently illegal.

The arguments to the C<$a->thread> call refer to the dimensions of $a
and when calling the xsub function, the arguments in the same place
in the different parameters refer to the same thread index, that is:

	func($a->thread(2,3),$b->thread(0,1));

Means that there are two dimensions over which a we are threading.
This corresponds to the pseudocode (assuming that both $a and $b have
4 dimensions)

	func($a(:,:,0,0),$b(0,0,:,:));
	func($a(:,:,0,1),$b(0,1,:,:));
	func($a(:,:,0,2),$b(0,2,:,:));
	.
	.
	func($a(:,:,1,0),$b(1,0,:,:));
	.
	.
	func($a(:,:,x,y),$b(x,y,:,:));

A parameter -1 to thread means that this pdl should not be threaded
over that thread dimension and a dummy index is created.

Depending on what you are doing, you have to choose between explicit
and implicit threading or a combination of the two.

=cut

sub thread {
	my $this = shift;
	print "THREADING: "; $this->printdims();
	$this->_ensureincs();
	my $new = $this->_shallowcopy();
	$new->{ThreadInstrs} = [@_];
	for(@_) {
# Move dimension $_ from Dims and Incs to ThreadDims and ThreadIncs
		if($_ == -1) {
			push @{$new->{ThreadDims}}, 1;
			push @{$new->{ThreadIncs}}, 0;
		} else {
			push @{$new->{ThreadDims}},
				splice(@{$new->{Dims}},$_,1,"X");
			push @{$new->{ThreadIncs}},
				splice(@{$new->{Incs}},$_,1,"X");
		}
	}
	$new->{Dims} = [map {$_ eq "X" ? () : $_} @{$new->{Dims}}];
	$new->{Incs} = [map {$_ eq "X" ? () : $_} @{$new->{Incs}}];
	print "NEW: "; $new->printdims(); print "\n";
	return $new;
}

=head2 xchg

	$a->xchg(ind1,ind2)

Exchanges the indices with each other.

=cut

sub xchg {
	my ($this,$ind1,$ind2) = @_;
	@{$this->{Dims}}[$ind1,$ind2] = @{$this->{Dims}}[$ind2,$ind1];
	if($this->{Incs}) {
		@{$this->{Incs}}[$ind1,$ind2] = @{$this->{Incs}}[$ind2,$ind1];
	}
}

=head2 subthread

=head2 dummy

	$a->dummy(n[,m])

Inserts a dummy dimension as dimension I<n> of $a.
A dummy dimension has increment of 0 and dimension of one by default or
I<m> if given.

=cut

sub dummy {
	my $this = shift; 
	my($nth,$size) = @_;
	if(!$size) {$size = 1;}
	splice @{$this->{Dims}}, $_, 0, $size;
	if($this->{Incs}) {
		splice @{$this->{Incs}}, $_, 0, 0;
	}
}

=head2 asvector

	$a->asvector()

This changes the whole pdl structure into one-dimensional vector. 
The idea is that after this it is easy to do minimum/maximum/other
such functions over the vector without worrying about the logical
structure. Warning: NEVER use this after some other mapping operation
has been performed on the pdl: very strange things will happen.

=cut

sub asvector {
	my $this = shift;
	my $new = $this->_shallowcopy();
	my $val = 1;
	for (@{$new->{Dims}}) {$val *= $_;}
	$new->{Dims} = [$val];
	$new->{Incs} = [1];
	return $new;
}

=head2 printdims

	$a->printdims()

Prints the dimensions of $a to standard output. Just for debugging.

=cut

sub printdims {
	my $this = shift;
	for("Dims","Incs","ThreadDims","ThreadIncs") {
		if($this->{$_}) {
			print "$_=[",(join ',',@{$this->{$_}}),"] ";
		}
	}
	if($this->{Offs}) {
		print "Offs=$this->{Offs}";
	}
	print "\n";
}

# slice(dim,start,inc,end) makes a slice.
# Note that end can be less than start, with inc reversed if need be.
sub _slice {
	my $this = shift;
	$this->_ensureincs();
	my $new = $this->_shallowcopy();
	while($#_>=0) {
		my $dim = shift;
		my $start = shift; my $inc = shift; my $end = shift;
		if($start < 0 || $end < 0) 
		  {croak "Slice cannot start or end below zero\n";}
		if($start >= $new->{Dims}[$dim] ||
		   $end   >= $new->{Dims}[$dim]) 
		  {croak "Slice cannot start or end above limit\n";}
		if(($end - $start)*$inc < 0) {
			$inc = -$inc;
		}
		my $n = int(($end-$start) / $inc)+1;
		$new->{Offs} += $start * $new->{Incs}[$dim];
		$new->{Incs}[$dim] *= $inc;
		$new->{Dims}[$dim] = $n;
	}
	$new;
}

sub _obliterate {
	my $this = shift;
	for (@_) {
		$this->{Dims}[$_] = "X";
		$this->{Incs}[$_] = "X";
	}
	$this->{Dims} = [map {$_ eq "X" ? () : $_} @{$this->{Dims}}];
	$this->{Incs} = [map {$_ eq "X" ? () : $_} @{$this->{Incs}}];
}

sub _adddummies {
	my $this = shift;
	my $ind = 0;
	for(@_) {
# To make _adddummies(2,2,2) possible
		if( ref($this->{Dims}[$_]) ) {
			$this->{Dims}[$_] = [1,@{$this->{Dims}[$_]}];
			$this->{Incs}[$_] = [0,@{$this->{Incs}[$_]}];
		} else {
			$this->{Dims}[$_] = [1,$this->{Dims}[$_]];
			$this->{Incs}[$_] = [0,$this->{Incs}[$_]];
		}
	}
	$this->{Dims} = [map {ref $_ ? (@$_) : $_} @{$this->{Dims}}];
	$this->{Incs} = [map {ref $_ ? (@$_) : $_} @{$this->{Incs}}];
}

# Make a shallow copy: just a reference to data
sub _shallowcopy {
	my $this = shift;
	my $new = bless {}, ref($this);
	for ( grep(($_ ne "PDL" and $_ ne "Data"), keys %$this) ) {   # Efficient to ignore Data here
		$$new{$_} = PDL::Core::rcopyitem( $$this{$_} );  # Deep copy
	}
	if(ref $this->{Data}) {
		$$new{Data} = $$this{Data};
	} else {
		$$new{Data} = \$this->{Data}; # Special, make reference
	}
	return $new;
}

# Ensure that we have a valid {Incs} component.
sub _ensureincs {
	my $this = shift;
	if(!ref $this->{Incs}) {
		my $inc = 1;
		$this->{Incs} = [map {
			$inc *= $_; $inc / $_;} @{$this->{Dims}}];
	}
}



=head1 BUGS

All this is WAY too powerless yet: we need more power!

The documentation is currently written by the author.

The implementation is tentative: everything may change.

This really needs to be rewritten in C once eveything solidifies
and PDL starts converting the struct to C. Now it's more comfortable
to hone the interface in Perl. 

These routines only work with xsubs compiled with PDL::PP. The results
of using these in other ways are highly unpredictable.

=head1 AUTHOR

Tuomas J. Lukka (lukka@fas.harvard.edu)

=cut

package PDL::Thread;
sub foo {}

1;
