package PDL::NOver;

PDL->overload::OVERLOAD(
	'.=' => sub {PDL::NBasic::my_ufunc2(@_[1],@_[0],"NOTHING");}
);

1;
