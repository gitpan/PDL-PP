
# Makefile.PL for PDL::Experiment module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

use lib '../..';
use PDL::Core::Dev;            # Pick up development utilities

WriteMakefile(
 'NAME'  	=> 'PDL::PP',
# 'VERSION_FROM'	=> 'PP.pm',
 VERSION	=> '0.005',
 'TYPEMAPS'     => [PDL_TYPEMAP], 
 'OBJECT'       => '',
 'INC'          => PDL_INCLUDE,
 'LIBS'         => [''],
 'clean'        => {},
);

sub MY::postamble {
'
'
}  # Add genpp rule
