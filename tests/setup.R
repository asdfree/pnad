if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

pnad_cat <-
	get_catalog( "pnad" ,
		output_dir = file.path( getwd() ) )

# skip 2008 because it doesn't fit
pnad_cat <- subset( pnad_cat , year != 2008 )

pnad_cat <- pnad_cat[ split( seq( nrow( pnad_cat ) ) , 1 + sort( seq( nrow( pnad_cat ) ) %% 13 ) )[[ this_sample_break ]] , ]

lodown( "pnad" , pnad_cat )
if( any( pnad_cat$year == 2011 ) ){
library(lodown)
# examine all available PNAD microdata files
pnad_cat <-
	get_catalog( "pnad" ,
		output_dir = file.path( getwd() ) )

# 2011 only
pnad_cat <- subset( pnad_cat , year == 2011 )
# download the microdata to your local computer


options( survey.lonely.psu = "adjust" )

library(survey)

pnad_df <- readRDS( pnad_cat[ 1 , 'output_filename' ] )

pop_types <- 
	data.frame( 
		v4609 = unique( pnad_df$v4609 ) , 
		Freq = unique( pnad_df$v4609 )
	)

prestratified_design <-
	svydesign(
		id = ~ v4618 ,
		strata = ~ v4617 ,
		data = pnad_df ,
		weights = ~ pre_wgt ,
		nest = TRUE
	)
	
rm( pnad_df ) ; gc()

pnad_design <- 
	postStratify( 
		design = prestratified_design ,
		strata = ~ v4609 ,
		population = pop_types
	)
	
rm( prestratified_design ) ; gc()
pnad_design <- 
	update( 
		pnad_design , 
		age_categories = factor( 1 + findInterval( v8005 , seq( 5 , 60 , 5 ) ) ) ,
		male = as.numeric( v0302 == 2 ) ,
		teenagers = as.numeric( v8005 > 12 & v8005 < 20 ) ,
		started_working_before_thirteen = as.numeric( v9892 < 13 )
	)
sum( weights( pnad_design , "sampling" ) != 0 )

svyby( ~ one , ~ region , pnad_design , unwtd.count )
svytotal( ~ one , pnad_design )

svyby( ~ one , ~ region , pnad_design , svytotal )
svymean( ~ v4720 , pnad_design , na.rm = TRUE )

svyby( ~ v4720 , ~ region , pnad_design , svymean , na.rm = TRUE )
svymean( ~ age_categories , pnad_design )

svyby( ~ age_categories , ~ region , pnad_design , svymean )
svytotal( ~ v4720 , pnad_design , na.rm = TRUE )

svyby( ~ v4720 , ~ region , pnad_design , svytotal , na.rm = TRUE )
svytotal( ~ age_categories , pnad_design )

svyby( ~ age_categories , ~ region , pnad_design , svytotal )
svyquantile( ~ v4720 , pnad_design , 0.5 , na.rm = TRUE )

svyby( 
	~ v4720 , 
	~ region , 
	pnad_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ started_working_before_thirteen , 
	denominator = ~ teenagers , 
	pnad_design ,
	na.rm = TRUE
)
sub_pnad_design <- subset( pnad_design , v4011 == 1 )
svymean( ~ v4720 , sub_pnad_design , na.rm = TRUE )
this_result <- svymean( ~ v4720 , pnad_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ v4720 , 
		~ region , 
		pnad_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pnad_design )
svyvar( ~ v4720 , pnad_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ v4720 , pnad_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ v4720 , pnad_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ male , pnad_design ,
	method = "likelihood" )
svyttest( v4720 ~ male , pnad_design )
svychisq( 
	~ male + age_categories , 
	pnad_design 
)
glm_result <- 
	svyglm( 
		v4720 ~ male + age_categories , 
		pnad_design 
	)

summary( glm_result )
library(convey)
pnad_design <- convey_prep( pnad_design )

sub_pnad_design <- 
	subset( 
		pnad_design , 
		!is.na( v4720 ) & v4720 != 0 & v8005 >= 15
	)

svygini( ~ v4720 , sub_pnad_design , na.rm = TRUE )
svytotal( ~one , pnad_design )
svytotal( ~factor( v0302 ) , pnad_design )
cv( svytotal( ~factor( v0302 ) , pnad_design ) )
}
