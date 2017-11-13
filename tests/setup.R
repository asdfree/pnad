if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

pnad_cat <-
	get_catalog( "pnad" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( pnad_cat ) ) / ceiling( nrow( pnad_cat ) / 5 ) )

pnad_cat <- unique( rbind( pnad_cat[ record_categories == this_sample_break , ] , pnad_cat[ pnad_cat$year == 2011 , ] ) )

# skip 2008 because it doesn't fit
pnad_cat <- subset( pnad_cat , year != 2008 )

lodown( "pnad" , pnad_cat )
