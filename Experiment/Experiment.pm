


package PDL::Experiment;

# $VERSION = "1.00";

@EXPORT_OK = qw( PDLTEST1 PDLTEST2 PDLTEST3 PDLTEST4 PDLTEST5 );

use PDL::Core;
use DynaLoader;
@ISA    = qw( PDL::Exporter DynaLoader ); 

bootstrap PDL::Experiment;


;# Exit with OK status

1;


