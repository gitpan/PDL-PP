use blib;
use PDL;
use PDL::Experiment;
use Data::Dumper;

$a = pdl [1,2],[3,4],[5,6];
$b = pdl [0,0,0],[0,0,0];

print $a; print $b;

PDLTEST1($a,$b,5);

print $b;
$d = {%$b};
print Dumper($d);

print "TESTING CRETION\n";

$b=pdl 0;
print $b; 
$d = {%$b};
print Dumper($d);
PDLTEST1($a,$b,5);

print $b; 
$d = {%$b};
print Dumper($d);

# Try out the matrix multiplier.

# $c = pdl [0,0,0],[0,0,0],[0,0,0];
$c = pdl [0,0],[0,0];

PDLTEST2($a,$b,$c);

$a = byte $a;
$b = short $b;
$c = byte pdl [0,0],[0,0];

print "DT: $a->{Datatype}, $b->{Datatype} $c->{Datatype}\n";

PDLTEST2($a,$b,$c);

print $a; print $b;
print $c;

print "DT: $a->{Datatype}, $b->{Datatype} $c->{Datatype}\n";

$a = pdl [0,0.1],[0.2,0.3];
$b = pdl [0,0],[0,0];

PDLTEST3($a,$b,0);

print $a; print $b;

