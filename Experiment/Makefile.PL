
# Makefile.PL for PDL::Experiment module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

use lib '../..';
use PDL::Core::Dev;            # Pick up development utilities

WriteMakefile(
 'NAME'  	=> 'PDL::Experiment',
# 'VERSION_FROM'	=> 'PP.pm',
 VERSION	=> '0.001',
 'TYPEMAPS'     => [PDL_TYPEMAP], 
 'OBJECT'       => 'Experiment$(OBJ_EXT)',
 'INC'          => PDL_INCLUDE,
 'LIBS'         => [''],
 'clean'        => {'FILES'  => 'Experiment.xs'},
);

sub MY::postamble {
'
Experiment.xs: experiment.pd
	perl experiment.pd 
'
}  # Add genpp rule
