use blib;
use PDL;
use PDL::NBasic;
use PDL::Thread;
require 'NOver.pm';

$a = xvals zeroes(5,5);
$b = zeroes(5,5);

$b .= $a->map('(2),:');

print $a,$b;

($c = $b->map('(3),:')) .= ($a->map(':,(4)'));

print $a; print $b;
