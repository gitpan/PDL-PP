=head1 NAME

PDL::PPHooks - standard extensions for the PDL::PP preprocessor

=head1 DESCRIPTION

NOT REALLY THERE YET

=cut

package PP::Add_Call_Parameters;
sub get_type {return "CALLPARAMS";}
sub run {my($this,$obj) = @_;
	push @{$obj->{ParList}}, 
		map {/^(\w+\**)\s+(\w+)$/ or die "Invalid parameter $_"; [$1,$2]}
			split ',',$this->{Str}];
}

package PP;

sub Add_Call_Parameters {my ($str) = @_;
	bless {Str => $str},PP::Add_Call_Parameters;
}

#################
