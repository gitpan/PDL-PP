use blib;
use PDL;
use PDL::Primitive;
use PDL::Thread;
use PDL::NBasic;
use Data::Dumper;

# In Experiment/experiment.pd I define three functions:
# sumover, inner and outer.
# inner: c = sum_i a_i b_i
#

# POWER-INDEXING
# Unfortunately, $a = $b doesn't work yet :( 
#	Keep pressurizing perl5-porters ;)

# sub dumpabc {print Dumper(%$a), Dumper(%$b), Dumper(%$c);}
sub dumpabc {}

$a = double zeroes(8,8); $b = double pdl 1..8;
print "A:"; $a->printdims();

# Make $c into a slice of array $a.

$c = $a->map('(1),:');

# The printout shows that we have a 1-dimensional slice.

print "C:"; $c->printdims();
dumpabc;

# Assign to this slice the vector b

 assgn($b,$a);
dumpabc;
 print "A NOW:\n",$a,"B NOW:\n",$b;

$a *= 0;
dumpabc;

print "A ZEROES\n";

assgn($b->thread(0),$c->thread(0));
dumpabc;

print "A NOW1:\n",$a,"B NOW:\n",$b;

assgn($b->thread(0),$a->thread(0));
dumpabc;
print "A NOW2:\n",$a,"B NOW:\n",$b;

my_biop1($a->thread(1),$b->thread(0),$a->thread(1),"+");

print "A NOW3:\n",$a,"B NOW:\n",$b;

# print "A: ",Dumper($a->{Data}),"\nC:",Dumper($c->{Data}),"\n";

# $a->thread(1) += $b;

$b = double xvals zeroes(3,3);
print "CREATION:\n";

$a = null;

print "A:",$a,"\n";

assgn($b,$a);

print "b:",$b,"\n";
print "A:",$a,"\n";
$a->printdims();


# THREADING

# This is how to do a matrix multiplication using the generic
# inner product:

# inner($a->thread(0,-1), $b->thread(-1,1), $c->thread(0,1));

# If (a and/or b) and c have too many dimensions, they are automatically
# threaded over.

# inner($a->thread(0,-1), $b->thread(-1,1), $c->thread(0,1));

print "DONE DONE\n";
