use blib;
use PDL;
use PDL::Experiment;

$a = pdl [1,2],[3,4],[5,6];
$b = pdl [0,0,0],[0,0,0];

print $a; print $b;

PDLTEST1($a,$b,5);

print $b;

# Try out the matrix multiplier.

# $c = pdl [0,0,0],[0,0,0],[0,0,0];
$c = pdl [0,0],[0,0];

PDLTEST5($a,$b,$c);

print $c;

