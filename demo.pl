use blib;
use PDL;
use PDL::Experiment;
use PDL::Thread;
use Data::Dumper;

# In Experiment/experiment.pd I define three functions:
# sumover, inner and outer.
# inner: c = sum_i a_i b_i
#

# POWER-INDEXING
# Unfortunately, $a = $b doesn't work yet :( 
#	Keep pressurizing perl5-porters ;)

$a = double zeroes(6,6); $b = double pdl 1,2,3,4,5,6;
print "A:"; $a->printdims();

# Make $c into a slice of array $a.

$c = $a->map('(3),:');

# The printout shows that we have a 1-dimensional slice.

print "C:"; $c->printdims();

# Assign to this slice the vector b

 assgn($b,$a);
 print "A NOW:\n",$a,"B NOW:\n",$b;

$a *= 0;

assgn($b->thread(0),$c->thread(0));

print "A NOW:\n",$a,"B NOW:\n",$b;

assgn($b->thread(0),$a->thread(0));
print "A NOW:\n",$a,"B NOW:\n",$b;


print "A: ",Dumper($a->{Data}),"\nC:",Dumper($c->{Data}),"\n";






# THREADING

# This is how to do a matrix multiplication using the generic
# inner product:

# inner($a->thread(0,-1), $b->thread(-1,1), $c->thread(0,1));

# If (a and/or b) and c have too many dimensions, they are automatically
# threaded over.

# inner($a->thread(0,-1), $b->thread(-1,1), $c->thread(0,1));


