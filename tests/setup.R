if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

pnad_cat <-
	get_catalog( "pnad" ,
		output_dir = file.path( getwd() ) )

# sample 60% of the records
which_records <- sample( seq( nrow( pnad_cat ) ) , round( nrow( pnad_cat ) * 0.60 ) )

# always sample year == 2011
pnad_cat <- unique( rbind( pnad_cat[ which_records , ] , subset( pnad_cat , year == 2011 ) ) )

lodown( "pnad" , pnad_cat )
