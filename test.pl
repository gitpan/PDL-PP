# This script fails for now because the current version does not
# support creation.
#
# This will eventually change.

use blib;
use PDL;
use PDL::Experiment;
use PDL::Primitive;
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

$a = pdl [[1,2],[3,4],[5,6]],[[0,1],[2,3],[4,5]]
	;
$b = byte pdl [[0,0,0],[0,0,0]],[[0,0,0],[0,0,0]]
	;
print Dumper(%$b);
print Dumper(%$a);
print "B: DATATYPE, $b->{Datatype}\n";
PDLTEST1($a,$b,4);
print $a;
print "THREADED BYTE TRANSPOSE + 4 ->\n";
print "B: DATATYPE, $b->{Datatype}\n";
print $b;

print "TESTING CRETION\n";

$b=null;
print $b; 
$d = {%$b};
print Dumper($d);
PDLTEST1($a,$b,5);

print $b; 
$d = {%$b};
print Dumper($d);

# Try out the matrix multiplier.

$d = null;
maximum($b,$d);

print "MAXIMUM OF B:\n";
print $d;
