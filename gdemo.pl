use blib;
use blib '../slatec';

# A demonstration on how to use gaussians.

use PDL;
use PDL::Gaussian;
use Data::Dumper;

# One 1-Dimensional gaussian

$foo = zeroes 5,5;

$a =new PDL::Gaussian([1],[]);

print $a->asstr,"\n";

$a->set_covariance ( double pdl [[1]] );
$a->set_mu ( double pdl [4] );

print "Gaussian now:\n ",$a->asstr(),"\n";

$b = double zeroes(10);

$a->calc_value((yvals transpose $b),$b);

print "Values(0..9)\n $b \n";

# imag $b;

$a->set_covariance( pdl [[5]] );

# imag "Values(0..9) $b \n";

# imag $b;

$a->set_covariance( pdl [[5]] );

$c = new PDL::Gaussian([2],[1]);
$c->{CV} = float pdl [[2,1],[1,5]];
$c->{Mu} = float pdl [[15,15]];

print "MUDIMS:";
$c->{Mu}->printdims();

$c->xxx_covariance();
print "OCOV\n";

$coords = zeroes(2,30,30);

$coords = yvals($coords) * xvals($coords) + 
	  zvals($coords) * (1-xvals($coords));

print "Coords: $coords\n";

$res = zeroes(1,30,30);

$c->calc_lnvalue($coords,$res);

print $res;
$res->printdims();

$res= $res->map('(0),:,:');
$res->{Data} = ${$res->{Data}};

print $res;
$res->printdims();

imag $res;


