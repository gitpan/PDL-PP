=head1 NAME

PDL::PP - Generate PDL routines from concise descriptions

=head1 SYNOPSIS

	use PDL::PP qw/Modulename Packagename Prefix/;

	addhdr('#include "hdr.h"');

	addpm('sub foo {}');

	defpdl(
		'inner_product',
		'a(x); b(x); c(); TYPES:BSULFD;',
		'int d',
		'double tmp = 0;
		 loop(x) %{
		 	tmp += $a() * $b();
			$TFD(do_when_float, do_when_double);
		%}
		 $c() = tmp;'
	);

	done();

=head1 DESCRIPTION

This module defines the routine C<defpdl> that generates xsub code from 
a short description such as the inner_product function above. C<done()> 
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

	tmp += $a() * $b();

is interpreted by the routine. At some hypothetical future
time, if PDL starts supporting sparse matrices, this might still be
made to work. Also, this code could be used in a wildly different
environment from PDL, achieving a kind of universality. Alternatively,
the compiler could, for debugging, place bounds checking at each 
access to a and b (because they are stored in memory sequentially,
this would be far superior to the usual gcc bounds checking).

=head2 Indexing

PDL::PP makes it possible for the caller to use a powerful indexing system.
Each index has an associated increment in the flat data space so 
it becomes trivial to exchange two indices or so on.

=head2 Threading

A single call to a PDL::PP-defined xsub can be a loop over several dimensions.
For example, if a function expects a 2D argument and is called with 
a pdl with dimensions (5,4,3) the call is automatically translated into

	func($a(:,:,0)); func($a(:,:,1)); func($a(:,:,2)); 

except that it is faster. This is called implicit threading.

If, in the previous example, you had wanted to thread over the second dimension
instead of the last (which is the default for implicit threading), you
need to do either

	func($a->xchg(1,2));

Which exchanges dimensions 1 and 2, resulting in (5,3,4) or

	func($a->thread(1));

Which means that we want to thread over dimension 1 (the second dimension,
of width 4).

For functions of several variables, this gets slightly more involved.
Implicit threading takes the extra dimensions on all the variables and
tuples them up so that the first extra dimension on each variable is one
index to be threaded over (so all the first extra dimensions must be of
the same size) and so on for the second extra dimension and so on.

An extra dimension of size 1 is interpreted to have the same size as
any other dimension but with increment 0 (all values of the index
point to the same data).

For example, if func now expects to have data as

	f(a(x,y),b(x,y,z),c(x));

then we can call (now, the numbers in parentheses are dimension sizes):

	f(a(5,3,10,11),b(5,3,2,10,1,12),c(5,1,11,12));

The meaning of which should be clear from the numbers.

Explicit threading, in the case of several variables is different:
here it is demanded that each C<thread> have the same number of arguments,
the first arguments on each call corresponding to each other, similarly
for the second etc. 

=head2 PDL variables

The second argument to C<defpdl> is either a ref to an
array of strings of the form

	typeoption [options]name(indices)

or a concatenation of strings like this with semicolons between them.
Options is a comma-separated list which can at the moment contain

=over 4
	
=item o

This pdl is used only for output and is therefore liable to be necessary
to create at runtime. In this case, all of its indices need to have
a defined value. If threading is done when this pdl is created,
then this it will have all the 
threadindices threaded over in it.

=item t

This pdl is used only as a temporary and is therefore liable to be necessary
to create at runtime. In this case, all of its indices need to have
a defined value. On creation, no threadindices will be included to 
a temp.

=item int

This pdl is of type integer and is not to be coerced to the same type
as everything else.

=back

The name is a lowercase alphanumeric name for the variable. One of 
the names can be preceded by ">" which means that is the function is 
called like C<$a = f($b)> instead of C<f($a,$b)> then this argument
is the output (THIS IS NOT YET IMPLEMENTED).
The indices part is a comma-separated list of lowercase index names.
If necessary, the indices can have predetermined sizes with the
syntax

	a(x,y=2);

One of the strings in the second argument can be of the type

	TYPES:whichtypes

where whichtypes consists of a combination of the letters C<BSULFD>
meaning for which datatypes this function should be build for. The default
is all but if you are interfacing to a function library, for instance,
the function may only be defined for some data types.

=head2 Indices

C<defpdl> uses named indices. In the first example, there were two named
indices, C<x> and C<y> and a "rest" index, C<X>. Each index name
is unique so the C<x> in both the definitions of C<a> and C<b> are interpreted
to mean the same number of elements and a runtime check is made of this.

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

There is also the special case pointer access C<$P(x)> which gives you
a pointer to the first element of C<x> in the current rest dimension
indexes. Note that if you are using this and someone starts mapping
indices, you are in big trouble. This is for use only when you are
passing arguments to outside functions.

=head2 Naming

For user access, there are some standard naming conventions.
All loop variables have just the name inside the C<loop> declaration.
Index sizes have the name of the index followed by C<_size>.
The same name is used if it is necessary to specify the dimension
of an output variable as a parameter.

=head2 Type coercion

Usually, you don't have to worry about type coercion. What happens
is that PP converts all pdls to which you have not specified a
type to the highest common denominator.

Currently, it is only possible to specify pdls to be integer or the generic
type.

Often, when interfacing with fortran, there are two different versions
of a subroutine for singles and doubles which are called relatively
similarly. C<$T[typechars](typeactions)> provides a way to do this:
typechars is a sequence of the characters C<BSULFD> and typeactions
a comma-separated list.

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

The documentation is written by the author :(

=head1 AUTHOR

Copyright (C) 1996 Tuomas J. Lukka (lukka@fas.harvard.edu)

=cut

sub print {print @_;}

#####################################################################
#
# Encapsulate the accesses to a PDL.
#
package PDL::PP::Obj;
use Carp;

sub new {
	my ($type,$string,$parent) = @_;
	my $this = bless {},$type;
	$string =~
		/^
		 \s*(int|)\s*	# $1: first option
		 (?:
			\[([^]]?)\]   	# $2: The initial [option] part
	         )?\s*
		 ([a-z0-9]+)          	# $3: The name
		 \(([^)]*)\)  		# $4: The indices
		/x or confess "Invalid pdl def $_\n";
	print "PDL: '$1', '$2', '$3', '$4'\n";
	my($opt1,$opt2,$name,$inds) = ($1,$2,$3,$4);
	$this->{Name} = $name;
	$this->{PName} = $parent->{NAME};
	$this->{Flags} = [(split ',',$opt2),($opt1?$opt1:())];
	for(@{$this->{Flags}}) {
		/^o$/ and $this->{FlagOut}=1 and $this->{FlagCreat}=1 or
		/^t$/ and $this->{FlagTemp}=1 and $this->{FlagCreat}=1 or
		/^int$/ and $this->{FlagInt} = 1 or
		croak("Invalid flag $_ given for $string\n");
	}
	my @inds = map{
		s/\s//g; 		# Remove spaces
		$_;
	} split ',', $inds;
	$this->{IndObjs} = [map {$parent->get_indobj_make($_)} @inds];
	my $ind=0;
	$this->{Inds} = [map {$_->add_pdl($this,$ind++);$_->{Name}} 
		@{$this->{IndObjs}}];
	my %indcount = ();
	$this->{IndCounts} = [
		map {
			0+($indcount{$_}++);
		} @inds
	];
	$this->{IndTotCounts} = [
		map {
			($indcount{$_});
		} @inds
	];
	return $this;
}

# Print an access part.
sub do_access {
	my($this,$inds,$context) = @_;
	my $pdl = $this->{Name};
# Parse substitutions into hash
	my %subst = map 
	 {/^\s*(\w+)\s*=>\s*(\w*)\s*$/ or confess "Invalid subst $_\n"; ($1,$2)} 
	 	split ',',$inds;
# Generate the text
	my $text = "(${pdl}_datap + $this->{Name}_threadoffs)"."[";
	$text .= join '+','0',map {
		$this->do_indterm($pdl,$_,\%subst,$context);
	} (0..$#{$this->{Inds}});
	$text .= "]";
# If not all substitutions made, the user probably made a spelling
# error. Barf.
	if(scalar(keys %subst) != 0) {
		confess("Substitutions left: ".(join ',',keys %subst)."\n");
	}
	return "$text /* ACCESS($access) */";
}

sub do_indterm { my($this,$pdl,$ind,$subst,$context) = @_;
#	print Data::Dumper::Dumper($this);
#	print "IND: $ind\n";
# Get informed
	my $indname = $this->{Inds}[$ind];
	my $indno = $this->{IndCounts}[$ind];
	my $indtot = $this->{IndTotCounts}[$ind];
# See if substitutions
	my $substname = ($indtot>1 ? $indname.$indno : $indname);
	my $incname = $indname.($indtot>1 ? $indno : "");
	my $index;
	if(defined $subst->{$substname}) {$index = delete $subst->{$substname};}
	else {
# No => get the one from the nearest context.
		for(@$context) {
			if($_->[0] eq $indname) {$index = $_->[1]; break;}
		}
	}
	if(!defined $index) {confess "Index not found: $pdl, $ind, $indname\n";}
	return "__inc_$pdl"."_".$incname."*". $index;
}

sub do_pointeraccess {my($this) = @_;
	return "($this->{Name}_datap + $this->{Name}_threadoffs)";
}

sub get_indname { my($this,$ind) = @_;
	$this->{Inds}[$ind] . ($this->{IndTotCounts}[$ind]>1 ?
				$this->{IndCounts}[$ind] : "");
}

sub get_xsinddecls { my($this) = @_;
	"long $this->{Name}_threadoffs;
	 long *__implthreadincs_$this->{Name};\n".
	join '', map {
		"long __inc_$this->{Name}_".$this->get_indname($_).";\n";
	} (0..$#{$this->{Inds}});
}

#  Implicit threads use the variables
#   __nimplthreaddims
#   __implthreaddims
#   __implthreadincs_pdl
#
#

# Orig. __nimplthreaddims = 0
sub get_xsimplthread1 { my($this) = @_;
	my $ndims = $#{$this->{Inds}}+1;
	return "if(__nimplthreaddims < $this->{Name}.ndims - $ndims)
		   __nimplthreaddims = $this->{Name}.ndims - $ndims;";
}

# Then, __implthreaddims has been allocated, fill up the values with the
# assertions. Originally each implthreaddim is 1.
# Cases: implthreaddim == 1 -> place this, whatever this is
#	 implthreaddim != 1 -> if not equal, barf

sub get_xsimplthread2 { my($this) = @_;
  my $pdl = $this->{Name}; my $ninds = $#{$this->{Inds}}+1;
  return "
    if(__nimplthreaddims) {
  	__implthreadincs_$pdl = PDL->malloc(sizeof(int) * __nimplthreaddims);
	for(__ind = 0; __ind < __nimplthreaddims; __ind++) {
		(__implthreadincs_${pdl})[__ind] = 0;
	}
	for(__ind = $ninds; __ind < $pdl.ndims; __ind ++) {
		(__implthreadincs_$pdl)[__ind - $ninds] = $pdl.incs[__ind];
		if($pdl.dims[__ind] == 1) { continue; }
		if(__implthreaddims[__ind - $ninds] < $pdl.dims[__ind]) {
			if(__implthreaddims[__ind - $ninds] != 1) {
				croak(\"$this->{PName}: Invalid dimension %d given for $pdl: incompatible with threading 1 (was %d)\\n\",__ind,
					$pdl.dims[__ind]);
			}
			__implthreaddims[__ind - $ninds] = $pdl.dims[__ind];
		} else if(__implthreaddims[__ind - $ninds] != $pdl.dims[__ind]) {
			croak(\"Invalid dimension %d given for $pdl: incompatible with threading 2\\n\",__ind);
		}
	}
    }";
}

# Check the dimensions.
# I'll try to explain the semantics at some point.
sub get_xsnormdimchecks { my($this) = @_;
	my $pdl = $this->{Name};
# Sanity check to avoid strange pointer accesses
	"__flag = 0;
	 for(__ind = 0; __ind < $pdl.ndims; __ind++) 
	 	__flag += ($pdl.dims[__ind]-1) * $pdl.incs[__ind];
	 for(__ind = 0; __ind < $pdl.nthreaddims; __ind++) 
	 	__flag += ($pdl.threaddims[__ind]-1) * $pdl.threadincs[__ind];
	 if(__flag >= $pdl.nvals || __flag < 0) {
	 	croak(\"HELP!HELP!HELP! Invalid input dims for $pdl given!\\n\");
	 }
	".
	"if($this->{Name}.ndims < $#{$this->{Inds}}+1) {
		croak(\"$this->{PName}: Too few dimensions for $this->{Name} (ndims: %d, was %d)\\n\",
			$#{$this->{Inds}}+1, $pdl.ndims);
	 }" .
	("for(__ind=0;__ind < $this->{Name}.ndims; __ind++) 
		if($this->{Name}.dims[__ind] == 1) {
			$this->{Name}.incs[__ind] = 0;
		}\n"
	).
	(join '', map {
		"if($this->{Inds}[$_]_size == -1) {
			    $this->{Inds}[$_]_size = $this->{Name}.dims[$_];
			} else if($this->{Inds}[$_]_size != $this->{Name}.dims[$_]) {
			   if($this->{Inds}[$_]_size == 1) {
			      $this->{Inds}[$_]_size = $this->{Name}.dims[$_];
			   } else if($this->{Name}.dims[$_] == 1) {
			      /* Do nothing */
			   } else {
				croak(\"$this->{PName}: Wrong dimension $this->{Inds}[$_] for $this->{Name} (should be: %d, was %d)\\n\",
				 $this->{Inds}[$_]_size, $this->{Name}.dims[$_]);
			    }
			}
		__inc_$this->{Name}_".$this->get_indname($_)." =
			$this->{Name}.incs[$_];\n";
	} (0..$#{$this->{Inds}}));
}

sub get_xsthreaddimchecks { my($this) =@_;
	my $pdl = $this->{Name};
	"if($pdl.nthreaddims != __nthreaddims) {
		if($pdl.nthreaddims == 0) {
			/* Do nothing. Must test for this in threadloop!!! */
		} else if(__nthreaddims == -1) {
			__nthreaddims = $pdl.nthreaddims;
			__threaddims = PDL->malloc(sizeof(int) * __nthreaddims);
			for(__ind=0; __ind < __nthreaddims; __ind++) {
				__threaddims[__ind] = $pdl.threaddims[__ind];
			}
			__threadinds = PDL->malloc(sizeof(int) * __nthreaddims);
		} else {
			croak(\"$this->{PName}:Unequal number of threaded dims for $pdl\\n\");
		}
	 }
	 for(__ind = 0; __ind < __nthreaddims; __ind++) {
	 	if($pdl.threaddims[__ind] != __threaddims[__ind] &&
		   $pdl.threaddims[__ind] != 1) {
		   	if(__threaddims[__ind] == 1) {
				__threaddims[__ind] = $pdl.threaddims[__ind];
			} else 
			  croak(\"$this->{PName}:Unequal thread dimension %d for $pdl\\n\",__ind);
		}
	 }";
}

sub get_xsthreadoffs { my($this) = @_;
	my $pdl = $this->{Name};my $ninds = $#{$this->{Inds}}+1;
	"
	 ${pdl}_threadoffs= 0;
	 if($pdl.nthreaddims != 0) {
	 	for(__ind=0; __ind < __nthreaddims; __ind++) {
			${pdl}_threadoffs += __threadinds[__ind] *
				${pdl}.threadincs[__ind];
		}
	 }
	 if(__nimplthreaddims) {
	 	for(__ind = $ninds; __ind < $pdl.ndims; __ind ++) {
			${pdl}_threadoffs += __implthreadinds[__ind-$ninds] *
				(__implthreadincs_$pdl)[__ind-$ninds];
		}
	 }
	"
};
	
sub get_xsdatapdecl { my($this,$genlooptype) = @_;
	my $type; my $pdl = $this->{Name};
	if(!grep {/^int$/} @{$this->{Flags}}) {
		$type = $genlooptype;
	} else {
		$type = "long";
	}
	return "\t$type *${pdl}_datap = ($type *)${_}.data + ${_}.offs;\n";
}

sub get_xsdatatypetest { my($this) = @_;
	if(!grep {/^int$/} @{$this->{Flags}}) {
		return ("\tif($this->{Name}.datatype > __datatype)
			__datatype = $this->{Name}.datatype;\n");
	} else {
		print "Not doing $this->{Name}: has int type\n";
		return(qq%\tif($this->{Name}.datatype != PDL_L) croak("Invalid datatype for $this->{Name}: should be long\n");%);
	}

}

sub get_xscoerce { my($this) = @_;
	if(!grep {/^int$/} @{$this->{Flags}}) {
		return("\tPDL->converttype(&($this->{Name}),__datatype,1);\n");
	}
}

#####################################################################
#
# Encapsulate one index.

package PDL::PP::Ind;
use Carp;

sub new {
	my($type,$name) = @_;
	my $this = bless {Name => $name},$type;
	return $this;
}

sub add_pdl {
	my($this,$pdlobj,$nth) = @_;
	push @{$this->{Pdls}}, [$pdlobj,$nth];
}

sub add_value {
	my($this,$val) = @_;
	if(defined $this->{Value}) {
		if($val != $this->{Value}) {
			confess("For index $this->{Name} conflicting values $this->{Value} and $val given\n");
		}
	} else {
		$this->{Value} = $val;
	}
}

# This index will take its size value from outside parameter ...
sub set_outsidepar { my($this,$outpar) = @_;
	$this->{OutsidePar} = $outpar;
}

sub get_xsdecls { my($this) = @_;
	if(!$this->{OutsidePar}) {
		my $val = (defined($this->{Value})?$this->{Value}:-1);
		return "long $this->{Name}_size = $val;\n";
	} elsif(defined($this->{Value})) {
		return "if($this->{Name}_size != $this->{Value})
			croak(\"Value given to $this->{Name}_size not equal to $this->{Value}\\n\");"
	}
	return "";
}

#####################################################################
package PDL::PP;
use PDL::Core;
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
		Types => 'BSULFD',
	};
	add_exported($name);
	bless $this,PDL::PP;
	$this->parse_pdls();
	$this->parse_pars();
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
	my $typ;
	@{$this->{PDLS}} = 
		map {
			if(/^\s*TYPES:/ ) {$typ=$_; ();} else {$_}
		} (@{$this->{PDLS}});
	if($typ) {
		$typ =~ /^\s*TYPES:([BSULFD]+)\s*$/ or confess("Invalid types: $typ\n");
		$this->{Types} = $1;
	}
# And get the info out of each one.
	for (@{$this->{PDLS}}) {
		my $obj = new PDL::PP::Obj($_,$this);
		$this->{Pdls}{$obj->{Name}} = $obj;
		push @{$this->{PdlOrder}}, $obj->{Name};
		push @{$this->{PdlNames}}, $obj->{Name};
#		my $ind=0;
#		for(@{$this->) {
#			push @{$this->{IndPdls}{$_}},[$name,$ind++];
#		}
	}
#	@{$this->{PdlOrder}} = (
#	(map {
#		(grep {/^o$/} @{$this->{PdlFlags}{$_}})   ?
#		 () : $_
#	} @{$this->{PdlNames}}),
#	(map {
#		(!(grep {/^o$/} @{$this->{PdlFlags}{$_}}))   ?
#		 () : $_
#	} @{$this->{PdlNames}}));
#	$this->{IndNames} = [keys %{$this->{IndPdls}}];
}

# Parse the other parameter arguments
sub parse_pars { my($this) = @_;
	for (split ',',$this->{PARS}) {
		/^\s*(.+)\b(\w+)\s*$/ or confess "Invalid pdl parameter $_\n";
		$this->{Pars}{$2} = $1;
	}
}

# Find which dimensions have a corresponding parameter
# and organize the dimensions in the order to seek them (non-output first,
# then alphabetical (anything goes)

sub get_indobj_make {
	my($this,$expr) = @_;
	$expr =~ /^([a-z0-9]+)(?:=([0-9]+))?$/ or confess "Invalid index expr '$expr'\n";
	my $name = $1; my $val = $2;
	my $indobj;
	if(defined $this->{Inds}{$name}) {
		$indobj = $this->{Inds}{$name};
	} else {
		$indobj = new PDL::PP::Ind($name);
		$this->{Inds}{$name}=$indobj;
		if(defined $this->{Pars}{$name."_size"}) {
			$this->{Inds}{$name}->set_outsidepar($name."_size");
		}
	}
	if(defined $val) { $indobj->add_value($val); }
	return $indobj;
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
		s/^(.*?)(\$[a-zA-Z]+\([^)]*\)|\bloop\([^)]+\)\s*%{|%}|$)//s or
			confess("Invalid program $_");
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
			} elsif($control =~ /^\$[a-zA-Z]+\([^)]*\)/) {
				push @{$stack[-1]},$control;
			} elsif($control =~ /^%}/) {
				pop @stack;
			} else {
				confess("Invalid control: $control\n");
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
		while(!$this->{Inds}{$ind}) {
			if(!((chop $ind) =~ /[0-9]/)) {
				confess("Index not found for $_ ($ind)!\n");
			}
		}
		$text .= "{long $_; for($_=0; $_<${ind}_size; $_++)";
		$endtext .= "}";
		push @newcontext, [$ind,$_];
	}
	push @newcontext,@{$context};
	return "$text /* LOOP(".(join ',',(@{$loop->[0]})).")*/ {" . $this->tree2code($loop,\@newcontext)
		. "}$endtext";
}

sub do_macroaccess {my($this,$pdl,$inds) = @_;
	$pdl =~ /T([BSULFD]+)/ or confess("Macroaccess wrong: $pdl\n");
	my @lst = split ',',$inds;
	my @ilst = split '',$1;
	if($#lst != $#ilst) {confess("Macroaccess: different nos of args $pdl $inds\n");}
	return join ' ',map {
		"THISIS_$ilst[$_]($lst[$_])"
	} (0..$#lst) ;
}

#
# This function encodes one access to a variable
#
sub do_access { my($this,$access,$context) = @_;
# Parse the access
	$access =~ /^\$([a-zA-Z]+)\(([^)]*)\)/ or
		confess ("Access wrong: $access\n");
	my $pdl = $1; 
	my $inds = $2;
	if($pdl =~ /^T/) {
		return do_macroaccess($this,$pdl,$inds);
	} elsif($pdl eq "P") {
		return $this->{Pdls}{$inds}->do_pointeraccess();
	} else {
		return ($this->{Pdls}{$pdl}->do_access($inds,$context)). 
			"/* ACCESS($access) */";
	}
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
	$this->printxs("\tlong __restend; long __ind; 
		long __creating; long __flag; long __mult;
		long __nthreaddims=-1; int *__threaddims,*__threadinds;
		long __nimplthreaddims=0; long *__implthreadinds;
		long *__implthreaddims;\n");
	my @pdls = map {$this->{Pdls}{$_}} @{$this->{PdlOrder}};
	$this->printxs(join "",map {$_->get_xsinddecls()} @pdls);
	$this->printxs(join "",map {$_->get_xsdecls()} values %{$this->{Inds}});
	$this->printxs(join "",map {$_->get_xsimplthread1()} @pdls);
	$this->printxs(
	 "__implthreadinds = PDL->malloc(sizeof(int)*__nimplthreaddims);
	 __implthreaddims = PDL->malloc(sizeof(int)*__nimplthreaddims);
	 for(__ind=0; __ind<__nimplthreaddims; __ind++) 
	 	__implthreaddims[__ind] = 1;");
	$this->printxs(join "",map {$_->get_xsimplthread2()} @pdls);
	$this->printxs(join "",map {$_->get_xsnormdimchecks()} @pdls);
	$this->printxs(join "",map {$_->get_xsthreaddimchecks()} @pdls);
	my $pdl;
	my %restdef=();
	for(@{$this->{PdlOrder}}) {
		my $pdl = $_;
		my $isoutput = (grep {/^o$/} @{$this->{PdlFlags}{$pdl}});
		$isoutput = ($isoutput?1:0);
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
	}
}

# Print the code to start the loop over the "rest" parameters
sub print_xsloopstart { my($this) = @_;
	$this->printxs("\t/* Starting loop over thread-dimensions */\n");
	$this->printxs(
	 qq#\tfor(__ind=0; __ind<__nthreaddims; __ind++) __threadinds[__ind] = 0;
	    for(__ind=0; __ind<__nimplthreaddims; __ind++) __implthreadinds[__ind] = 0;\n#);
	$this->printxs("\t__restend=0; while(!__restend) { ");
 	my $pdl;
	$this->printxs(
		join '',map {
			$this->{Pdls}{$_}->get_xsthreadoffs();
		} (@{$this->{PdlOrder}})) ;
}

# Print the code
sub print_xscode { my($this) = @_;
	$this->printxs("\t/* This is the actual user-code */\n");
	$this->printxs("\t{\n$this->{Code}}");
}

# End the loop
sub print_xsloopend { my($this) = @_;
	$this->printxs("\t/* Ending loop over rest-dimensions */\n");
	$this->printxs("\t__restend=1;\n");
	$this->printxs(
	 qq#\tfor(__ind=0; __ind<__nthreaddims; __ind++) 
	 	{if(++(__threadinds[__ind]) >= __threaddims[__ind]) 
		 {__threadinds[__ind]=0;} else {__restend=0; break;} ;} 
	      if(!__restend) continue;
	      for(__ind=0; __ind<__nimplthreaddims; __ind++) 
	 	{if(++(__implthreadinds[__ind]) >= __implthreaddims[__ind]) 
		 {__implthreadinds[__ind]=0;} else {__restend=0; break;} ;} \n#);
	$this->printxs("\t;}\n");
}

sub print_xsfooter { my($this) = @_;
	$this->printxs("\t}\n");
}

####
#
# Type coercion

sub print_xscoerce { my($this) = @_;
	$this->printxs("\tlong __datatype=PDL_B;\n");
# First, go through all the types, selecting the most general.
	for(@{$this->{PdlOrder}}) {
		$this->printxs($this->{Pdls}{$_}->get_xsdatatypetest());
	}
# See which types we are allowed to use.
	$this->printxs("\tif(0) {}\n");
	for(@{$this->get_generictypes()}) {
		$this->printxs("\telse if(__datatype <= $_->[2]) __datatype = $_->[2];\n");
	} 
	$this->printxs("\telse {croak(\"Too high type %d given!\\n\",__datatype);}");
# Then, coerce everything to this type.
	for(@{$this->{PdlOrder}}) {
		$this->printxs($this->{Pdls}{$_}->get_xscoerce());
	}
}

sub print_xsgenericstart { my($this) = @_;
	$this->printxs("/* Start generic loop */\n");
	for(B,S,U,L,F,D) {
		$this->printxs("#undef THISIS_$_\n#define THISIS_$_(a)\n");
	}
	$this->printxs("\tswitch(__datatype) { case -42: /* Warning eater */ {1;\n");
}

sub print_xsgenericitem { my($this,$item) = @_;
	$this->printxs("\t} break; case $item->[0]: {\n");
	for(B,S,U,L,F,D) {
		$this->printxs("#undef THISIS_$_\n#define THISIS_$_(a)\n");
	}
	$this->printxs("#undef THISIS_$item->[3]\n#define THISIS_$item->[3](a) a\n");
	$this->printxs(join '',map{
		$this->{Pdls}{$_}->get_xsdatapdecl($item->[1]);
	} (@{$this->{PdlNames}})) ;
}

sub print_xsgenericend { my($this) = @_;
	$this->printxs("\tbreak;}
		default:croak(\"PP INTERNAL ERROR! PLEASE MAKE A BUG REPORT\\n\");}\n");
}

sub get_generictypes { my($this) = @_;
	return [map {
		$this->{Types} =~ /$_->[0]/ ? [PDL_.($_->[0]eq"U"?"US":$_->[0]),$_->[1],$_->[2],$_->[0]] : ()
	}
	       ([B,"unsigned char",$PDL_B],
		[S,"short",$PDL_S],
		[U,"unsigned short",$PDL_US],
		[L,"long",$PDL_L],
		[F,"float",$PDL_F],
		[D,"double",$PDL_D])];
}

