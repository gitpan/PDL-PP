
# Makefile.PL for PDL::Experiment module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

use lib '../..';
use PDL::Core::Dev;            # Pick up development utilities

WriteMakefile(
 'NAME'  	=> 'PDL::NBasic',
# 'VERSION_FROM'	=> 'PP.pm',
 'TYPEMAPS'     => [PDL_TYPEMAP], 
 'OBJECT'       => 'NBasic$(OBJ_EXT)',           
 PM 	=> {'NBasic.pm' => '$(INST_LIBDIR)/NBasic.pm'},
 'INC'          => PDL_INCLUDE,
 'LIBS'         => [''],
 'clean'        => {'FILES'  => 'NBasic.xs'},
);

sub MY::postamble {
'
NBasic.xs NBasic.pm: nbasic.pd ../PP.pm
	perl nbasic.pd 
'
}  # Add genpp rule
