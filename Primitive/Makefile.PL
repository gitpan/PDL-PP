
# Makefile.PL for PDL::Primitive module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

use lib '../..';
use PDL::Core::Dev;            # Pick up development utilities

WriteMakefile(
 'NAME'  	=> 'PDL::Primitive',
# 'VERSION_FROM'	=> 'PP.pm',
 'TYPEMAPS'     => [PDL_TYPEMAP], 
 'OBJECT'       => 'Primitive$(OBJ_EXT)',           
 PM 	=> {'Primitive.pm' => '$(INST_LIBDIR)/Primitive.pm'},
 'INC'          => PDL_INCLUDE,
 'LIBS'         => [''],
 'clean'        => {'FILES'  => 'Primitive.xs'},
);

sub MY::postamble {
'
Primitive.xs Primitive.pm: primitive.pd ../PP.pm
	perl primitive.pd 
'
}  # Add genpp rule
