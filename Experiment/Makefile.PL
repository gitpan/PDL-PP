
# Makefile.PL for PDL::Experiment module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

use lib '../..';
use PDL::Core::Dev;            # Pick up development utilities

WriteMakefile(
 'NAME'  	=> 'PDL::Experiment',
# 'VERSION_FROM'	=> 'PP.pm',
 'TYPEMAPS'     => [PDL_TYPEMAP], 
 'OBJECT'       => 'Experiment$(OBJ_EXT)',           
 PM 	=> {'Experiment.pm' => '$(INST_LIBDIR)/Experiment.pm'},
 'INC'          => PDL_INCLUDE,
 'LIBS'         => [''],
 'clean'        => {'FILES'  => 'Experiment.xs'},
);

sub MY::postamble {
'
Experiment.xs Experiment.pm: experiment.pd ../PP.pm
	perl experiment.pd 
'
}  # Add genpp rule
