#!/usr/bin/env Rscript
# 
# Prepare data.
#
# w.m.otte@umcutrecht.nl
#
#########################

library( 'plyr' )

###################################################
# FUNCTIONS
###################################################


###
# Read data.
##
read_data <- function( outdir )
{
	all1 <- read.csv( 'data/subset_data_1.csv.gz', row.names = 1 )
	all2 <- read.csv( 'data/subset_data_2.csv.gz', row.names = 1 )
	
	all <- rbind( all1, all2 )
	
	# read data
	#all <- read.csv( infile, row.names = 1 )
	all$study_year <- all$yearpub

	excluded_data <- all[ is.na( all$study_year ), ]
	
	# remove all rows without study year
	all <- all[ ! is.na( all$study_year ), ]

	# rebuttal test differences in properties of missed data
	rebuttal_data1 <- rbind( 
		c( 'all', 'allocation', quantile( all$RoB_allocation_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) ),
		c( 'excluded', 'allocation', quantile( excluded_data$RoB_allocation_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) ),
		
		c( 'all', 'random', quantile( all$RoB_random_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) ),
		c( 'excluded', 'random', quantile( excluded_data$RoB_random_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) ),
		
		c( 'all', 'blinding_pts', quantile( all$RoB_blinding_pts_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) ),
		c( 'excluded', 'blinding_pts', quantile( excluded_data$RoB_blinding_pts_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) ),
	
		c( 'all', 'blinding_outcome', quantile( all$RoB_blinding_outcome_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) ),
		c( 'excluded', 'blinding_outcome', quantile( excluded_data$RoB_blinding_outcome_prob, na.rm = TRUE, c( 0.25, 0.75 ) ) )
	)
	
	rebuttal_data2 <- rbind( 
		c( 'all', 'consort', 100 * ( summary( as.factor( all$CONSORT ) ) / nrow( all ) ) ),
		c( 'excluded', 'consort', 100 * ( summary( as.factor( excluded_data$CONSORT ) ) / nrow( excluded_data ) ) ),
		c( 'all', 'trialreg_any', 100 * ( summary( as.factor( all$CONSORT ) ) / nrow( all ) ) ),
		c( 'excluded', 'trialreg_any', 100 * ( summary( as.factor( excluded_data$CONSORT ) ) / nrow( excluded_data ) ) )
		
	)
	
	# write
	write.csv( rebuttal_data1, file = paste0( outdir, '/rebuttal_data_ROB.csv' ) )
	write.csv( rebuttal_data2, file = paste0( outdir, '/rebuttal_data_CONSORT.csv' ) )
	
	all$year_group <- '' 
	
	# first bin covers all papers before 1990	
	all[ all$study_year >= 1950 & all$study_year < 1990, 'year_group' ] <- 1987.5
	all[ all$study_year >= 1990 & all$study_year < 1995, 'year_group' ] <- 1992.5
	all[ all$study_year >= 1995 & all$study_year < 2000, 'year_group' ] <- 1997.5
	all[ all$study_year >= 2000 & all$study_year < 2005, 'year_group' ] <- 2002.5
	all[ all$study_year >= 2005 & all$study_year < 2010, 'year_group' ] <- 2007.5
	all[ all$study_year >= 2010 & all$study_year < 2018, 'year_group' ] <- 2012.5

	# only keep to following variables
	vars <- c( 	'pmid', 'journal', 'gender_first', 'gender_last', 'source_of_funding', 'trialreg_any', 'trialreg_ct.gov', 'CONSORT',
				'RoB_random_prob', 'RoB_allocation_prob', 'RoB_blinding_pts_prob', 'RoB_blinding_outcome_prob', 
				'study_year', 'year_group', 'nAuthors', 'gender_propFem', 'ncountries', 'h.first', 'h.last', 
				'acadAge_first', 'acadAge_last', 'acadPresence_first', 'acadPresence_last', 'neg.rate', 'pos.rate',
				'nCollabor.first', 'nCollabor.last', 'ninstitution', 'IF', 'n.publications',
				'firstcontinent', 'lastcontinent',  'ranking.first', 'ranking.last','catNew','jcontinent' ) 

	# select vars
	all <- all[ , vars ]

	# reorder some binary vars
	all$female_first <- all$gender_first
	all$female_last <- all$gender_last
	all$industrial_funding <- all$source_of_funding
	all$trial_registered <- all$trialreg_any
    all$trial_registered_gov <- all$trialreg_ct.gov
	all$consort_mentioned <- all$CONSORT

	levels( all$female_first ) <- c( 1, 0 )
	levels( all$female_last ) <- c( 1, 0 )
	levels( all$industrial_funding ) <- c( 1, 0, 0 )
	levels( all$trial_registered ) <- c( 0, 1 )
	levels( all$trial_registered_gov ) <- c( 0, 1 )
	levels( all$consort_mentioned ) <- c( 1, 0 )

	# convert rate to percentage
	all$neg.rate <- all$neg.rate * 100
	all$pos.rate <- all$pos.rate * 100

	# dichotomize IF; 
	all$impact_factor_cat5 <- '<=5'
	all$impact_factor_cat5[ all$IF > 5 ] <- '>5'

	# dichotomize IF; 
	all$impact_factor_cat3 <- '<=3'
	all$impact_factor_cat3[ all$IF > 3 ] <- '>3'

	# dichotomize IF; 
	all$impact_factor_cat10 <- '<=10'
	all$impact_factor_cat10[ all$IF > 10 ] <- '>10'

	# all catNew below 4000 -> other
	all$disciplines <- as.character( all$catNew )

	#"genetics_heredity",272
	all[ all$disciplines == 'genetics_heredity' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"otorhinolaryngology",984
	all[ all$disciplines == 'otorhinolaryngology' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"geriatrics_gerontology",1478
	all[ all$disciplines == 'geriatrics_gerontology' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"integrative_complementary",1548
	all[ all$disciplines == 'integrative_complementary' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"hematology",1720
	all[ all$disciplines == 'hematology' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"dermatology",2067
	all[ all$disciplines == 'dermatology' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"rehabilitation",2671
	all[ all$disciplines == 'rehabilitation' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"orthopedics",3262
	all[ all$disciplines == 'orthopedics' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"ophthalmology",3589
	all[ all$disciplines == 'ophthalmology' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"dentistry_oralSurgery",3666
	all[ all$disciplines == 'dentistry_oralSurgery' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	#"respiratory_system",3845
	all[ all$disciplines == 'respiratory_system' & !is.na( all$disciplines ), 'disciplines' ] <- 'other'

	# remove non-used binary vars
	all$gender_first <- NULL
	all$gender_last <- NULL
	all$source_of_funding <- NULL
	all$trialreg_any <- NULL
	all$trialreg_ct.gov <- NULL
	all$CONSORT <- NULL
	all$catNew <- NULL

	return( all )
}

###################################################
# END FUNCTIONS
###################################################

# make output directory
outdir <- 'out.prepare'
dir.create( outdir, showWarnings = FALSE )

# read raw data
#infile <- 'data/data.csv'  

# get data
all <- read_data( outdir )

# write selected and processed data
write.csv( all, file = paste0( outdir, '/data.csv' ) )

