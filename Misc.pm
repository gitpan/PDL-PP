package PDL::Misc;
use Carp;
use PDL::NBasic;
@ISA = qw/PDL::Exporter/;
@EXPORT_OK = qw/ similar_assign /;

sub similar_assign {
	my($from,$to) = @_;
	if((join ',',@{$from->{Dims}}) ne (join ',',@{$to->{Dims}})) {
		confess "Similar_assign: dimensions [".
			(join ',',@{$from->{Dims}})."] and [".
			(join ',',@{$to->{Dims}})."] do not match!\n";
	}
	my_ufunc2($from,$to,"NOTHING");
}

1;
