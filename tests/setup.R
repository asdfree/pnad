# mineiro data
# love verdade gave to me
# twelve karaoke..
dictionary_tf <- tempfile()

dictionary_url <-
	paste0(
		"https://ftp.ibge.gov.br/Trabalho_e_Rendimento/" ,
		"Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/" ,
		"Trimestral/Microdados/Documentacao/Dicionario_e_input_20221031.zip"
	)

download.file( dictionary_url , dictionary_tf , mode = 'wb' )

dictionary_files <- unzip( dictionary_tf , exdir = tempdir() )

sas_fn <- grep( '\\.sas$' , dictionary_files , value = TRUE )

sas_lines <- readLines( sas_fn , encoding = 'latin1' )
sas_start <- grep( '@0001' , sas_lines )

sas_end <- grep( ';' , sas_lines )

sas_end <- sas_end[ sas_end > sas_start ][ 1 ]

sas_lines <- sas_lines[ seq( sas_start , sas_end - 1 ) ]

# remove SAS comments
sas_lines <- gsub( "\\/\\*(.*)" , "" , sas_lines )

# remove multiple spaces and spaces at the end of each string
sas_lines <- gsub( "( +)" , " " , sas_lines )
sas_lines <- gsub( " $" , "" , sas_lines )

sas_df <- 
	read.table( 
		textConnection( sas_lines ) , 
		sep = ' ' , 
		col.names = c( 'position' , 'column_name' , 'length' ) ,
		header = FALSE 
	)

sas_df[ , 'character' ] <- grepl( '\\$' , sas_df[ , 'length' ] )

sas_df[ , 'position' ] <- as.integer( gsub( "\\@" , "" , sas_df[ , 'position' ] ) )

sas_df[ , 'length' ] <- as.integer( gsub( "\\$" , "" , sas_df[ , 'length' ] ) )

stopifnot( 
	sum( sas_df[ , 'length' ] ) == 
	( sas_df[ nrow( sas_df ) , 'position' ] + sas_df[ nrow( sas_df ) , 'length' ] - 1 ) 
)

this_tf <- tempfile()

this_url <-
	paste0(
		"https://ftp.ibge.gov.br/Trabalho_e_Rendimento/" ,
		"Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/" ,
		"Trimestral/Microdados/2023/PNADC_012023.zip"
	)

download.file( this_url , this_tf , mode = 'wb' )
library(readr)

pnad_tbl <-
	read_fwf(
		this_tf ,
		fwf_widths( 
			widths = sas_df[ , 'length' ] , 
			col_names = sas_df[ , 'column_name' ] 
		) ,
		col_types = 
			paste0( ifelse( sas_df[ , 'character' ] , "c" , "d" ) , collapse = '' )
	)

pnad_df <- data.frame( pnad_tbl )

names( pnad_df ) <- tolower( names( pnad_df ) )

pnad_df[ , 'one' ] <- 1
# pnad_fn <- file.path( path.expand( "~" ) , "PNAD" , "this_file.rds" )
# saveRDS( pnad_df , file = pnad_fn , compress = FALSE )
# pnad_df <- readRDS( pnad_fn )
library(survey)

pnad_design <-
	svrepdesign(
		data = pnad_df ,
		weight = ~ v1028 ,
		type = 'bootstrap' ,
		repweights = 'v1028[0-9]+' ,
		mse = TRUE ,
	)

pnad_design <-
	update(
	
		pnad_design ,
		
		pia = as.numeric( v2009 >= 14 )
	
	)

pnad_design <-
	update(
	
		pnad_design ,
		
		ocup_c = ifelse( pia == 1 , as.numeric( vd4002 %in% 1 ) , NA ) ,

		desocup30 = ifelse( pia == 1 , as.numeric( vd4002 %in% 2 ) , NA )
	)

pnad_design <- 

	update( 

		pnad_design , 

		uf_name =
		
			factor(
			
				as.numeric( uf ) ,
				
				levels = 
					c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 31L, 32L, 33L, 35L, 41L, 42L, 43L, 50L, 51L, 
					52L, 53L) ,
					
				labels =
					c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", 
					"Tocantins", "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", 
					"Paraiba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
					"Espirito Santo", "Rio de Janeiro", "Sao Paulo", "Parana", "Santa Catarina", 
					"Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goias", 
					"Distrito Federal")
					
			) ,

		age_categories = factor( 1 + findInterval( v2009 , seq( 5 , 60 , 5 ) ) ) ,

		male = as.numeric( v2007 == 1 ) ,

		region = substr( uf , 1 , 1 ) ,

		# calculate usual income from main job
		# (rendimento habitual do trabalho principal)
		vd4016n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4016 , NA ) ,

		# calculate effective income from main job
		# (rendimento efetivo do trabalho principal) 
		vd4017n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4017 , NA ) ,

		# calculate usual income from all jobs
		# (variavel rendimento habitual de todos os trabalhos)
		vd4019n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4019 , NA ) ,

		# calculate effective income from all jobs
		# (rendimento efetivo do todos os trabalhos) 
		vd4020n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4020 , NA ) ,

		# determine the potential labor force
		pea_c = as.numeric( ocup_c == 1 | desocup30 == 1 )
		
	)
sum( weights( pnad_design , "sampling" ) != 0 )

svyby( ~ one , ~ uf_name , pnad_design , unwtd.count )
svytotal( ~ one , pnad_design )

svyby( ~ one , ~ uf_name , pnad_design , svytotal )
svymean( ~ vd4020n , pnad_design , na.rm = TRUE )

svyby( ~ vd4020n , ~ uf_name , pnad_design , svymean , na.rm = TRUE )
svymean( ~ age_categories , pnad_design )

svyby( ~ age_categories , ~ uf_name , pnad_design , svymean )
svytotal( ~ vd4020n , pnad_design , na.rm = TRUE )

svyby( ~ vd4020n , ~ uf_name , pnad_design , svytotal , na.rm = TRUE )
svytotal( ~ age_categories , pnad_design )

svyby( ~ age_categories , ~ uf_name , pnad_design , svytotal )
svyquantile( ~ vd4020n , pnad_design , 0.5 , na.rm = TRUE )

svyby( 
	~ vd4020n , 
	~ uf_name , 
	pnad_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ ocup_c , 
	denominator = ~ pea_c , 
	pnad_design ,
	na.rm = TRUE
)
sub_pnad_design <- subset( pnad_design , ocup_c == 1 )
svymean( ~ vd4020n , sub_pnad_design , na.rm = TRUE )
this_result <- svymean( ~ vd4020n , pnad_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ vd4020n , 
		~ uf_name , 
		pnad_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pnad_design )
svyvar( ~ vd4020n , pnad_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ vd4020n , pnad_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ vd4020n , pnad_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ male , pnad_design ,
	method = "likelihood" )
svyttest( vd4020n ~ male , pnad_design )
svychisq( 
	~ male + age_categories , 
	pnad_design 
)
glm_result <- 
	svyglm( 
		vd4020n ~ male + age_categories , 
		pnad_design 
	)

summary( glm_result )
nationwide_adult_population <- svytotal( ~ pia , pnad_design , na.rm = TRUE )
	
stopifnot( round( coef( nationwide_adult_population ) / 1000000 , 3 ) == 174.228 )
stopifnot( round( cv( nationwide_adult_population ) / 1000000 , 3 ) == 0 )
	
nationwide_labor_force <- svytotal( ~ pea_c , pnad_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_labor_force ) / 1000000 , 3 ) == 107.257 )
stopifnot( round( cv( nationwide_labor_force ) * 100 , 1 ) == 0.2 )
	
nationwide_employed <- svytotal( ~ ocup_c , pnad_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_employed ) / 1000000 , 3 ) == 97.825 )
stopifnot( round( cv( nationwide_employed ) * 100 , 1 ) == 0.2 )
	
nationwide_unemployed <- svytotal( ~ desocup30 , pnad_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_unemployed ) / 1000000 , 3 ) == 9.432 )
stopifnot( round( cv( nationwide_unemployed ) * 100 , 1 ) == 1.2 )
	
nationwide_not_in_labor_force <-
	svytotal( ~ as.numeric( pia & !pea_c ) , pnad_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_not_in_labor_force ) / 1000000 , 3 ) == 66.972 )
stopifnot( round( cv( nationwide_not_in_labor_force ) * 100 , 1 ) == 0.3 )
	

library(convey)
pnad_design <- convey_prep( pnad_design )

svygini( ~ vd4020n , pnad_design , na.rm = TRUE )
library(srvyr)
pnad_srvyr_design <- as_survey( pnad_design )
pnad_srvyr_design %>%
	summarize( mean = survey_mean( vd4020n , na.rm = TRUE ) )

pnad_srvyr_design %>%
	group_by( uf_name ) %>%
	summarize( mean = survey_mean( vd4020n , na.rm = TRUE ) )
