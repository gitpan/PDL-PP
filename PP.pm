=head1 NAME

PDL::PP - Generate PDL routines from concise descriptions

=head1 SYNOPSIS

	use PDL::PP qw/Modulename Packagename Prefix/;

	addhdr('#include "hdr.h"');

	addpm('sub foo {}');

	defpdl(
		'Transpose',
		'a(x,y,X); int [o]b(y,x,X)',
		'int c',
		'loop(x,y) %{
			$b() = $a();
		%}'
	);

	done();

=head1 DESCRIPTION

This module defines the routine C<defpdl> that generates xsub code from 
a short description such as the transpose function above. C<done> 
automatically writes the files C<Prefix.xs> and C<Prefix.pm>.

The idea is that since this concise description encodes in itself
(better than C code, which would be difficult to interpret) what
is necessary to do, this code can be compiled to C in many different
ways. Also, the resulting C code can be easily made to do the right thing
in many situations: for example, in the above code, the matrix b is
a destination matrix so the code can check whether b exists and
has the right dimensions or die or alternatively create a new b in that
case. 

Of course, a human can also code all the intelligent code, but if
there are tens of different routines, it gets very dull after a while.
And to think about reuse: in the above code, the line

	b() = a();

is interpreted by the routine. At some hypothetical future
time, if PDL starts supporting sparse matrices, this might still be
made to work. Also, this code could be used in a wildly different
environment from PDL, achieving a kind of universality. Alternatively,
the compiler could, for debugging, place bounds checking at each 
access to a and b (because they are stored in memory sequentially,
this would be far superior to the usual gcc bounds checking).

=head2 PDL variables

The second argument to C<defpdl> is either a ref to an
array of strings of the form

	typeoption [options]name(indices,X)

or a concatenation of strings like this with semicolons between them.
Options is a comma-separated list which can at the moment contain

=over 4
	
=item o

This pdl is used only for output and is therefore liable to be necessary
to create at runtime. In this case, all of its indices need to have
a defined value.

=item int

This pdl is of type integer and is not to be coerced to the same type
as everything else.

=back

The name is a lowercase alphanumeric name for the variable. One of 
the names can be preceded by ">" which means that is the function is 
called like C<$a = f($b)> instead of C<f($a,$b)> then this argument
is the output.
The indices part is a comma-separated list of lowercase index names or 
"..." or an uppercase index name for a "rest" index.

=head2 Indices

C<defpdl> uses named indices. In the first example, there were two named
indices, C<x> and C<y> and a "rest" index, C<X>. Each index name
is unique so the C<x> in both the definitions of C<a> and C<b> are interpreted
to mean the same number of elements and a runtime check is made of this.

The "rest" index is a special case which may contain several indices,
and must be currently in the same order. The idea is that the code will
be automatically looped over this set of indices. In the future, it may
be possible to have several different "rest" indices for different
sets of variables.

=head2 Loops

In the C code, it is possible to automatically create loops. In the
example, the line

	loop(x,y) 

Makes loops over the indices x and y. If all your dimensions mean
different things, then this is usually sufficient but if you have
some square matrices, for example correlation or so, you need to use
the syntax

	loop(x0,x1)

which starts two loops over the same size. Currently, to make it
easier to program, the loops use the sequences C<%{> and C<%}> (like
yacc) to start and end. In the future, this may change. 

As a point of interest, there is an actual parser and context manager
with stack and all in the code. Perl makes these things very easy to do.

=head2 Array access

C<defpdl> attempts to make the defaults do the right thing in a wide
variety of cases without the need to specify the indices explicitly.
However, special cases always arise and for those, the syntax

	loop(x1,x2) %{ a() = b(x => x1) * c(x => x2) %};

may be used (here the sizes could be C<[qw/[o]a(x,x) b(x) c(x)/]>, in 
which case this sets a to the outer product of b and c.

=head2 Naming

For user access, there are some standard naming conventions.
All loop variables have just the name inside the C<loop> declaration.
Index sizes have the name of the index followed by C<_size>.
The same name is used if it is necessary to specify the dimension
of an output variable as a parameter.

=head1 INFLUENCES

The ideas here have been influenced by the language Yorick as well as
matlab and scilab.

=head1 BUGS

Uncountably.

When using GCC, it would be much faster to just declare an array
with variable number of indices than to use pdl_malloc.
With other compilers, it would also be a lot faster to use a huge
largest N_DIM (16, for example, or if you want to be *ABSOLUTELY* certain,
50) and be done with it. Then it will be on the stack, and allocated 
and accessed rapidly.

At the moment, the code does not create nonexistent or invalid-sized
pdls. However, the change is fairly trivial.

The run-time error messages the code generates are really awful and
uninformative.

An important issue is whether this version puts C too far from us.
It is possible to use normal C loops instead of the loop() syntax
and so on, but I think it may come in handy pretty often.

The code is not very readable at the moment. It is fairly modular,
however.

The generated code is relatively inefficient, especially at access
times. The outer loops should update pointers to the data
accessed inside to be efficient. However, the comfort of writing code
like this is very nice.

The current type coercion is not good.

=head1 AUTHOR

Copyright (C) 1995 Tuomas J. Lukka (Tuomas.Lukka@helsinki.fi)

=cut

sub print {print @_;}

package PDL::PP;
use FileHandle;
require Exporter;
@ISA = qw(Exporter);

@EXPORT = qw/addhdr addpm defpdl done/;

use Carp;
use Data::Dumper;

use strict qw/vars refs/;

sub import {
	my ($mod,$modname, $packname, $prefix) = @_;
	$::PDLMOD=$modname; $::PDLPACK=$packname; $::PDLPREF=$prefix;
	$::PDLXS="";
	$::PDLPMROUT="";
	$::PDLPM="";
	@_=("PDL::PP");
	goto &Exporter::import;
}

sub addhdr {
	my ($hdr) = @_;
	$::PDLXS .= $hdr;
}

sub addpm {
	my ($pm) = @_;
	$::PDLPM .= $pm;
}

sub add_exported {
	my ($exp) = @_;
	$::PDLPMROUT .= $exp." ";
}

sub printxs {
	shift;
	$::PDLXS .= join'',@_;
}

sub done {
	print "DONE!\n";
	(my $fh = new FileHandle(">$::PDLPREF.xs")) or die "Couldn't open xs file\n";

$fh->print(qq%
/*
 * THIS FILE WAS GENERATED BY PDL::PP! Do not modify!
 */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "pdl.h"
#include "pdlcore.h"
static Core* PDL; /* Structure hold core C functions */
SV* CoreSV;       /* Get's pointer to perl var holding core structure */
 
$::PDLXS

BOOT:
   /* Get pointer to structure of core shared C routines */
   CoreSV = perl_get_sv("PDL::SHARE",FALSE);  /* SV* value */
   if (CoreSV==NULL)
     croak("This module requires use of PDL::Core first");
   PDL = (Core*) (void*) SvIV( CoreSV );  /* Core* value */
%);                                                                

	($fh = new FileHandle(">$::PDLPREF.pm")) or die "Couldn't open pm file\n";

$fh->print(qq%
# 
# GENERATED WITH PDL::PP! Don't modify!
#
package $::PDLPACK;

\@EXPORT = qw( $::PDLPMROUT);

use PDL::Core;
use DynaLoader;
\@ISA    = qw( PDL::Exporter DynaLoader ); 

bootstrap $::PDLMOD;

$::PDLPM;

;# Exit with OK status

1;

%);

}


###########################################################################
#
#	Here is the actual code that generates the XSUB
#

sub defpdl {
	my($name,$pdls, $others, $code) = @_;
	my($this) = {
		NAME => $name,
		MODULE => $::PDLMOD,PACKAGE => $::PDLPACK,
		PDLS => $pdls, PARS => $others, CODE => $code,
	};
	add_exported($name);
	bless $this,PDL::PP;
	$this->parse_pdls();
	$this->parse_pars();
	$this->parse_dims();
	$this->make_parlist();
	$this->parse_code();
	$this->print_xsheader();
	$this->print_xscoerce();
	$this->printxs("\t{\n");
	$this->print_xsdiminit();
	$this->print_xsgenericstart();
	for(@{$this->get_generictypes()}) {
		$this->print_xsgenericitem($_);
		$this->print_xsloopstart();
		$this->print_xscode();
		$this->print_xsloopend();
	}
	$this->print_xsgenericend();
	$this->printxs("\t}\n");
	$this->print_xsfooter();
}

# Parse the pdl parameters and form a data structure
sub parse_pdls { my($this) = @_;
# If we're given a string, separate it
	if(!ref $this->{PDLS}) {
		$this->{PDLS} = [map {
			/^\s*$/ ? () : $_; 	# Eliminate empties
		} split ';',$this->{PDLS}];
		
	}
# And get the info out of each one.
	for (@{$this->{PDLS}}) {
		/(?:
			(|\w+\b)	# $1: first option
			\[([^]]?)\]   	# $2: The initial [option] part
	         )?
		 ([a-z0-9]+)          	# $3: The name
		 \(([^)]+)\)  		# $4: The indices
		/x or croak "Invalid pdl def $_\n";
		print "PDL: '$1', '$2', '$3', '$4'\n";
		my($opt1,$opt2,$name,$inds) = ($1,$2,$3,$4);
		push @{$this->{PdlNames}}, $name;
		$this->{PdlFlags}{$name} = [(split ',',$opt2),($opt1?$opt1:())];
		my @inds = map{
			s/\s//g; 		# Remove spaces
			s/^\.\.\.$/_DOTS_/;     # ... => _DOTS_
			$_;
		} split ',', $inds;
		$this->{PdlInds}{$name} = [@inds];
		my %indcount = ();
		$this->{PdlIndCounts}{$name} = [
			map {
				0+($indcount{$_}++);
			} @inds
		];
		$this->{PdlIndTotCounts}{$name} = [
			map {
				($indcount{$_});
			} @inds
		];
		my $ind=0;
		for(@inds) {
			push @{$this->{IndPdls}{$_}},[$name,$ind];
		}
	}
	@{$this->{PdlOrder}} = (
	(map {
		(grep {/^o$/} @{$this->{PdlFlags}{$_}})   ?
		 () : $_
	} @{$this->{PdlNames}}),
	(map {
		(!(grep {/^o$/} @{$this->{PdlFlags}{$_}}))   ?
		 () : $_
	} @{$this->{PdlNames}}));
	$this->{IndNames} = [keys %{$this->{IndPdls}}];
}

# Parse the other parameter arguments
sub parse_pars { my($this) = @_;
	for (split ',',$this->{PARS}) {
		/^\s*(.+)\b(\w+)\s*$/ or croak "Invalid pdl parameter $_\n";
		$this->{Pars}{$2} = $1;
	}
}

# Find which dimensions have a corresponding parameter
# and organize the dimensions in the order to seek them (non-output first,
# then alphabetical (anything goes)
sub parse_dims { my($this) = @_;
	for (keys %{$this->{IndPdls}}) {
#		my @lst = $this->{IndPdls}{$_};
#		$this->{IndCheck}{$_} = [];
		if(defined $this->{Pars}{$_."_size"}) {
			$this->{IndCheck}{$_}=$_."_size";
		}
		/[A-Z]/ and (
			$this->{RestInds}{$_} = 1);
	}
}

# Do the appropriate substitutions in the code.
sub parse_code { my($this) = @_;
	$_ = $this->{CODE};
# First, separate the code into an array of C fragments (strings),
# variable references (strings starting with $) and
# loops (array references, 1. item = variable.
	my $coderef = [""];
	my @stack = ($coderef);
	my $control;
	while($_) {
		s/^(.*?)(\$[a-z]+\([^)]*\)|\bloop\([^)]+\)\s*%{|%}|$)//s or
			croak("Invalid program $_");
		$control = $2;
#		if(!($1 =~ /^\s*$/)) {
#			print "1: $1\n";
			push @{$stack[-1]},$1;
#		}
		if($control) {
#			print("2: $control\n");
			if($control =~ /^loop\(([^)]+)\) %{/) {
				push @{$stack[-1]},[[split ',',$1]];
				push @stack,$stack[-1][-1];
			} elsif($control =~ /^\$[a-z]+\([^)]*\)/) {
				push @{$stack[-1]},$control;
			} elsif($control =~ /^%}/) {
				pop @stack;
			} else {
				croak("Invalid control: $control\n");
			}
		} else {
			print("No \$2!\n");
		}
	}
# Then, in this form, put it together what we want the code to actually do.
#	print Dumper($coderef);
	$this->{Code} = $this->tree2code($coderef,[]);
}

sub tree2code { my($this,$code,$context) = @_;
	my $str = "";
	my $ind = -1;
	for(@$code) {
		$ind++; if($ind == 0) {next};
		if(ref $_) {
			$str .= $this->do_loop($_,$context);
		} elsif(/^\$/) {
			$str .= $this->do_access($_,$context);
		} else {
			$str .= $_;
		}
	}
	return $str;
}

sub do_loop { my($this,$loop,$context) = @_;
	my @newcontext=();
	my $text=""; my $endtext = "";
	for(@{$loop->[0]}) {
# Determine, which index this is. Chop one numeric character while not found.
		my $ind = $_;
		while(!grep {$_ eq $ind} @{$this->{IndNames}}) {
			if(!((chop $ind) =~ /[0-9]/)) {
				croak("Index not found for $_ ($ind)!\n");
			}
		}
		$text .= "{int $_; for($_=0; $_<${ind}_size; $_++)";
		$endtext .= "}";
		push @newcontext, [$ind,$_];
	}
	push @newcontext,@{$context};
	return "$text /* LOOP(".(join ',',(@{$loop->[0]})).")*/ {" . $this->tree2code($loop,\@newcontext)
		. "}$endtext";
}

sub do_indterm { my($this,$pdl,$ind,$subst,$context) = @_;
	if($this->{PdlInds}{$pdl}[$_] =~ /[A-Z]/) {
		return $pdl."_".$this->{PdlInds}{$pdl}[$_]."_offs";
	}
# Get informed
	my $indname = $this->{PdlInds}{$pdl}[$ind];
	my $indno = $this->{PdlIndCounts}{$pdl}[$ind];
	my $indtot = $this->{PdlIndTotCounts}{$pdl}[$ind];
# See if substitutions
	my $substname = ($indtot>1 ? $indname.$indno : $indname);
	my $incname = $indname.$indno;
	my $index;
	if($subst->{$substname}) {$index = delete $subst->{$substname};}
	else {
# No => get the one from the nearest context.
		for(@$context) {
			if($_->[0] eq $indname) {$index = $_->[1]; break;}
		}
	}
	if(!$index) {croak "Index not found: $pdl, $ind\n";}
	return $pdl."_".$incname."_inc"."*". $index;
}

sub do_access { my($this,$access,$context) = @_;
# Parse the access
	$access =~ /^\$([a-z]+)\(([^)]*)\)/ or
		croak ("Access wrong: $access\n");
	my $pdl = $1; 
# Parse substitutions into hash
	my %subst = map {/^\s*(\w+)\s*=>\s*(\w*)\s*$/ or croak "Invalid subst $_\n"; ($1,$2)} split ',',$2;
# Generate the text
	my $text = "(${pdl}_datap)"."[";
	$text .= join '+',map {
		$this->do_indterm($pdl,$_,\%subst,$context);
	} (0..$#{$this->{PdlInds}{$pdl}});
	$text .= "]";
# If not all substitutions made, the user probably made a spelling
# error. Barf.
	if(scalar(keys %subst) != 0) {
		croak("Substitutions left: ".(join ',',keys %subst)."\n");
	}
	return "$text /* ACCESS($access) */";
}

# Make the parameter lists for the XSUB
sub make_parlist { my($this) = @_;
	$this->{ShortPars} = 
	 join ',',@{$this->{PdlNames}},map {
	 	/\w\s+(\w+)/ or die "Invalid parameter $_";
		$1;
		} split ',',$this->{PARS};
	$this->{LongPars} = 
	 join '',map {"\t$_\n"}
	  (map {"pdl $_"} @{$this->{PdlNames}}),
	  split ',',$this->{PARS};
}

# Print the simple prototype of the XSUB function
sub print_xsheader { my($this) = @_;
	$this->printxs("\n\nMODULE = $this->{MODULE}   PACKAGE = $this->{PACKAGE}\n\n");
	$this->printxs("void\n$this->{NAME}($this->{ShortPars})\n$this->{LongPars}");
	$this->printxs("\tCODE:\n\t{\n");
}

# Print the code to initialize the dimensions of the matrices
# and possibly create new matrices.
# This is the trickiest part of the whole thing.
sub print_xsdiminit { my($this) = @_;
# Initialize dims to zero
	$this->printxs("\tint __restend; int __ind; int __flag; int __mult;\n");
	for (keys %{$this->{IndPdls}}) {
		my $ind = $_;
		if(/[A-Z0-9]+/) {
			$this->printxs("\tint ${_}_ndims = -1; int *${_}_sizes = NULL, *${_}_inds = NULL,".
			  (join',',map {"*${_}_${ind}_incs = NULL, ${_}_${ind}_offs"}
			    @{$this->{PdlNames}}
			  )
			  .";\n");
			next;
		} 
		if(!$this->{IndCheck}{$_}) {
			$this->printxs("\tint ${_}_size = -1, ${_}_incs = -1 ;\n");
		}
	}
	my $pdl;
	for $pdl (@{$this->{PdlNames}}) {
# Make the index increments. Here, if the same index repeats, all repetitions
# need to have their increments.
		my $ind;
		$this->printxs("\tint ".(join',',
		   map {$ind=$_; 
		         "${pdl}_$this->{PdlInds}{$pdl}[$ind]$this->{PdlIndCounts}{$pdl}[$ind]_inc"
		   	    }
			    0..$#{$this->{PdlInds}{$pdl}}).";\n");
		$this->printxs("\tint ${pdl}_ndims;\n");
	}
#	for (@{$this->{PdlNames}}) {
#		$this->printxs(qq#\tif(${_}.datatype != PDL_D) {croak("NON-DOUBLE!\n");}\n#);
#	}
# Now, loop over the matrices, filling in the dimensions.
# Check at compile-time to see when the rest dimensions are defined.
	my %restdef=();
	for(@{$this->{PdlOrder}}) {
		my $pdl = $_;
		$this->printxs("\t/* Checking indices of $_ */\n");
# First, get the rest indices.
		my $ind=0;
		my %restinds = map {
			$ind++;
			/[A-Z]/ ? 
				($_ => $ind-1)
			: ();
		} @{$this->{PdlInds}{$_}};
# Next, see that at most one is undefined.
		my @undef = grep {!defined $restdef{$_}} (keys %restinds);
		if($#undef > 0) {
			carp "More than one restind undefined!\n";
		}
# Calculate number of defined indices 
# and compare (run-time to actual number)
		$this->printxs("\t${_}_ndims = 0 ".(
			join '',map {"+ $_";} map {
				(defined($undef[0]) and $_ eq $undef[0]) ? () :
				/[A-Z]/ ? "${_}_ndims" : "1";
			} @{$this->{PdlInds}{$_}}).";\n");
		if($#undef == 0) {
# The undefined rest index must be the last one for now :(
			if($this->{PdlInds}{$_}[-1] ne $undef[0]) {
				carp "Undef restind not last!\n";
			}
			$this->printxs(
			 qq#\tif(${_}_ndims > ${_}.ndims) {croak("Too many dims for $_\\n");}\n#
			);
			$this->printxs(
			 "\t$undef[0]_ndims = ${_}.ndims - ${_}_ndims;
			  $undef[0]_sizes = PDL->malloc(sizeof(int) * $undef[0]_ndims);
			  $undef[0]_inds = PDL->malloc(sizeof(int) * $undef[0]_ndims);".(
			  join '',map {"
			  ${_}_$undef[0]_incs = PDL->malloc(sizeof(int) * $undef[0]_ndims);"}
			  	@{$this->{PdlNames}}
			)."
			  for(__ind = 0; __ind < $undef[0]_ndims; __ind++) {
			  	$undef[0]_sizes[__ind] = ${_}.dims[__ind + ${_}_ndims];
			  }\n"
			);
			$restdef{$undef[0]} = 1;
		} else {
			$this->printxs(
			qq#\tif(${_}_ndims != ${_}.ndims) {croak("Dimensions unequal!\\n");}\n#);
		}
# Test the dimension sizes.
# Alternatively, if this pdl is not defined, create the matrix
# with these dimensions.
#		my @incs = map {"\t${pdl}_ndims += $_;\n"}
#			map {/[A-Z]/ ? "${_}_ndims" : "1"} 
#			 @{$this->{PdlInds}{$_}};
# Now, test whether the indices are equal.
		$this->printxs("\t__flag=0;__mult=1;${pdl}_ndims=0;\n");
		my $a;
		my $no=0;
		$this->printxs(join '',
		  map {
		    $no++;
		    /[A-Z]/ ? "\tfor(__ind=0; __ind<${_}_ndims; __ind++) {
		       ${pdl}_${_}_incs[__ind] = __mult; __mult *= ${_}_sizes[__ind];
		       if(${pdl}.dims[${pdl}_ndims++] != ${_}_sizes[__ind]) 
		       	{__flag++;}}\n"
		       :"
		        if(${_}_size == -1) {${_}_size = ${pdl}.dims[${pdl}_ndims];}
		        ${pdl}_${_}$this->{PdlIndCounts}{$pdl}[$no-1]_inc 
				= __mult; __mult *= ${_}_size;
		        if(${pdl}.dims[${pdl}_ndims++] != ${_}_size) {__flag++;}\n"}
		    @{$this->{PdlInds}{$_}});
		$this->printxs(qq#\tif(__flag) {croak("Some dimensions not equal!\\n");}\n#);

	}
}

# Print the code to start the loop over the "rest" parameters
sub print_xsloopstart { my($this) = @_;
	$this->printxs("\t/* Starting loop over rest-dimensions */\n");
	for(keys %{$this->{RestInds}}) {
		$this->printxs(
		 qq#\tfor(__ind=0; __ind<${_}_ndims; __ind++) ${_}_inds[__ind] = 0;\n#);
	}
	$this->printxs("\t__restend=0; while(!__restend) { ");
 	my $pdl;
	for $pdl (@{$this->{PdlNames}}) {
	for(keys %{$this->{RestInds}}) {
		$this->printxs(
		 qq#\t${pdl}_${_}_offs=0;
		  for(__ind=0; __ind<${_}_ndims; __ind++) ${pdl}_${_}_offs+= ${_}_inds[__ind]*${pdl}_${_}_incs[__ind] ;\n#);
	}
	}
}

# Print the code
sub print_xscode { my($this) = @_;
	$this->printxs("\t/* This is the actual user-code */\n");
	$this->printxs($this->{Code});
}

# End the loop
sub print_xsloopend { my($this) = @_;
	$this->printxs("\t/* Ending loop over rest-dimensions */\n");
	$this->{ContLoopInd}++;
	my $contloop = "contloop".$this->{ContLoopInd};
	for(keys %{$this->{RestInds}}) {
		$this->printxs(
		 qq#\tfor(__ind=0; __ind<${_}_ndims; __ind++) {if(++(${_}_inds[__ind]) >= ${_}_sizes[__ind]) {${_}_inds[__ind]=0; break;} else goto $contloop;} \n#);
	}
	$this->printxs("\t__restend=1;\n\t$contloop: 1;}\n");
}

sub print_xsfooter { my($this) = @_;
	$this->printxs("\t}\n");
}

####
#
# Type coercion

sub print_xscoerce { my($this) = @_;
	$this->printxs("\tint __datatype=PDL_B;\n");
	for(@{$this->{PdlNames}}) {
		if(!grep {/^int$/} @{$this->{PdlFlags}{$_}}) {
			$this->printxs("\tif($_.datatype > __datatype)
				__datatype = $_.datatype;\n");
		} else {
			print "Not doing $_: has int type\n";
			$this->printxs(qq%\tif($_.datatype != PDL_L) croak("Invalid datatype for $_: should be long\n");%);
		}
	}
	for(@{$this->{PdlNames}}) {
		if(!grep {/^int$/} @{$this->{PdlFlags}{$_}}) {
			$this->printxs("\tPDL->converttype(&($_),__datatype,1);\n");
		}
	}
}

sub print_xsgenericstart { my($this) = @_;
	$this->printxs("/* Start generic loop */\n");
	$this->printxs("\tswitch(__datatype) { case -42: /* Warning eater */ {1;\n");
}

sub print_xsgenericitem { my($this,$item) = @_;
	$this->printxs("\t} break; case $item->[0]: {\n");
	for(@{$this->{PdlNames}}) {
		if(!grep {/^int$/} @{$this->{PdlFlags}{$_}}) {
			$this->printxs("\t$item->[1] *${_}_datap = ($item->[1] *)${_}.data;\n");
		} else {
			$this->printxs("\tint *${_}_datap = (int *)${_}.data;\n");
		}
	}
}

sub print_xsgenericend { my($this) = @_;
	$this->printxs("\t}}\n");
}

sub get_generictypes {
	return [[PDL_B,"unsigned char"],
		[PDL_S,"short"],
		[PDL_US,"unsigned short"],
		[PDL_L,"long"],
		[PDL_F,"float"],
		[PDL_D,"double"]];
}

