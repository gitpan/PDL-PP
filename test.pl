# This script fails for now because the current version does not
# support creation.
#
# This will eventually change.

use blib;
use PDL;
use PDL::Experiment;
use PDL::Thread;
use Data::Dumper;

print "TRYING OUT SIMPLE THREADING AND FUNCTION\n";

$a = pdl [[1,2],[3,4],[5,6]],[[0,1],[2,3],[4,5]]
	;
$b = pdl [[0,0,0],[0,0,0]],[[0,0,0],[0,0,0]]
	;

$c = $a->thread(2); $d = $b->thread(2);
$e = $a->_shallowcopy();

# print Dumper(%{$a}),"\n";
# print Dumper(%{$e}),"\n";
# print Dumper(%{$c}),"\n";

PDLTEST1($c,$d,5);

print $a;
print "THREADED TRANSPOSE + 5 ->\n";
print $b;
# $d = {%$b};
# print Dumper($d);

print "TESTING IMPLICIT THREADING\n";

$a = pdl [[1,2],[3,4],[5,6]],[[0,1],[2,3],[4,5]]
	;
$b = pdl [[0,0,0],[0,0,0]],[[0,0,0],[0,0,0]]
	;
print Dumper(%$b);
print Dumper(%$a);
PDLTEST1($a,$b,4);
print $a;
print "THREADED TRANSPOSE + 4 ->\n";
print $b;

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

