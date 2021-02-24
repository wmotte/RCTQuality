#!/usr/bin/env Rscript

# read and plot data
#
# w.m.otte@umcutrecht.nl
#
#########################

library( 'ggplot2' )
library( 'plyr' )
library( 'glmmTMB' )
library( 'multcomp' )


###################################################
# FUNCTIONS
###################################################

# manual colors (red, blue)
manual.colors <- c( "#f4a582", "#92c5de", "#0571b0", "#ca0020", "gray40" )
manual.colors.two <- c( "#0571b0", "#ca0020" )

###
# Helper functions for glht posthoc with glmmTMB
##
glht_glmmTMB <- function (model, ..., component="cond") {
    glht(model, ...,
         coef. = function(x) fixef(x)[[component]],
         vcov. = function(x) vcov(x)[[component]],
         df = NULL)
}
modelparm.glmmTMB <- function (model, 
                               coef. = function(x) fixef(x)[[component]],
                               vcov. = function(x) vcov(x)[[component]],
                               df = NULL, component="cond", ...) {
    multcomp:::modelparm.default(model, coef. = coef., vcov. = vcov.,
                        df = df, ...)
}

###
std <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

###
# ggplot style function.
##
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

###
# Get proportional CI
##
get.prop.ci <- function( k, n, lower = TRUE )
{

	if( k > n ) stop( "*** ERROR ***: could not calculate proportional CI as k > n!" )

	p <- prop.test( x = k, n = n )

	if( lower )
	{
		return( p$conf.int[ 1 ] )
	} else {
		return( p$conf.int[ 2 ] )
	}
}

###
# Plot number of trials
##
plot_n_pub <- function( df.cont, var = 'number_of_trials', ylabel = 'Trials', outdir, fillcolour = '#31a354' )
{
	print( fillcolour )
	xlabel = 'Year'
	df.cont$x <- df.cont$year_group
	df.cont$y <- 1

	# get means and 95% confidence intervals
	df <- df.cont
	df$x <- as.factor( df.cont$year_group )
	df$y <- 1

	# get means and 95% confidence interval - with linear regression
	ds <- ddply( df, .( x ), summarise, mean = sum( y, na.rm = T ) )
	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	p <- ggplot( ds, aes( x = x, y = mean ) ) +
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge(), fill = fillcolour ) +
			#geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +
			scale_y_continuous( breaks = number_ticks( 8 ) ) +
		   	ylab( paste0( ylabel, "" ) ) +
			xlab( xlabel ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) )

	# write to file
	ggsave( p, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.png', sep = '' ), dpi = 200, width = 4, height = 7 )

	# write corresponding number to csv
	write.csv( ds, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.csv', sep = '' ) )
}

###
# Get freq of studies per year
##
extract_n_year <- function( all, outdir )
{
	# get summaries
	ds <- ddply( all, .( study_year ), summarise, freq = length( study_year ) )
	write.csv( ds, file = paste0( outdir, '/n_rcts_per_year.csv' ) )
}

###
# Plot cont.
##
plot.cont <- function( df.cont, var, ylabel = 'Y', outdir, fillcolour = '#3690c0' )
{
	print( fillcolour )
	xlabel = 'Year'
	df.cont$x <- df.cont$year_group
	df.cont$y <- df.cont[, var ]

	# get means and 95% confidence intervals
	df <- df.cont
	df$x <- as.factor( df.cont$year_group )
	df$y <- df.cont[, var ]

	# get means and 95% confidence interval - with linear regression
	ds <- ddply( df, .( x ), summarise, mean = mean( y, na.rm = T ), lower = mean( y, na.rm = T ) - 1.96 * std( y, na.rm = T ), upper = mean( y, na.rm = T ) + 1.96 * std( y, na.rm = T ) )
	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	p <- ggplot( ds, aes( x = x, y = mean, ymin = lower, ymax = upper ) ) +
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge(), fill = fillcolour ) +
			geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +

			scale_y_continuous( breaks = number_ticks( 8 ), limits = c( 0, 0.68 ) ) +
		   	ylab( paste0( ylabel, "" ) ) +
			xlab( xlabel ) +
			theme_classic( base_size = 14 ) +

			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) )

	# write to file
	ggsave( p, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.png', sep = '' ), dpi = 200, width = 4, height = 7 )

	# write corresponding number to csv
	write.csv( ds, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.csv', sep = '' ) )
}

###
# Plot binary data.
##
plot.binary <- function( df.bin, var = var, ylabel = "Y", outdir = outdir, xlabel = "Year", fillcolour = '#3690c0' )
{
	#var <- 'trial_registered_gov'
	#ylabel <- 'In clinical trials.gov'

	print( fillcolour )
	xlabel = 'Year'
	df.bin$x <- df.bin$year_group
	df.bin$y <- df.bin[, var ]

	# get means and 95% confidence intervals
	df <- df.bin
	df$x <- as.factor( df.bin$year_group )
	df$y <- df.bin[, var ]

	# percentage and 95% CI
	ds <- ddply( df, .( x ), summarise, 
				mean = 100 * sum( y, na.rm = TRUE ) / length( y ),							
				lower = 100 * get.prop.ci( k = sum( y, na.rm = TRUE ), n = length( y ), lower = TRUE ),
				upper = 100 * get.prop.ci( k = sum( y, na.rm = TRUE ), n = length( y ), lower = FALSE ) )

	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	p <- ggplot( ds, aes( x = x, y = mean, ymin = lower, ymax = upper ) ) +
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge(), fill = fillcolour ) +
			geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +
			scale_y_continuous( breaks = number_ticks( 8 ), limits = c( 0, 50 ) ) +
			ylab( ylabel ) +
			xlab( xlabel ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) )

	# write to file
	ggsave( p, file = paste( outdir, '/', gsub( '%', 'perc', gsub( ' ', '_', ylabel ) ), '.png', sep = '' ), dpi = 200, width = 4, height = 7 )

	# write corresponding number to csv
	write.csv( ds, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.csv', sep = '' ) )
}


###################################################
# END FUNCTIONS
###################################################


# make output directory
outdir <- 'out.plot.main'
dir.create( outdir, showWarnings = FALSE )

# read prepared data
infile <- 'out.prepare/data.csv'  

# get data
all <- read.csv( infile, row.names = 1 )

# req. for rebuttal
extract_n_year( all, outdir )

# plot number of publications
plot_n_pub( all, 'trials', 'Trials', outdir )

# plot risk-of-bias
plot.cont( all, 'RoB_allocation_prob', 'Allocation bias', outdir )
plot.cont( all, 'RoB_random_prob', 'Randomization bias', outdir )
plot.cont( all, 'RoB_blinding_pts_prob', 'Blinding of participants and personnel bias', outdir )
plot.cont( all, 'RoB_blinding_outcome_prob', 'Blinding of outcome bias', outdir )

# plot registration and CONSORT
plot.binary( all, 'trial_registered', 'Registered (%)', outdir )
plot.binary( all, 'consort_mentioned', 'With CONSORT statement (%)', outdir )


plot.cont( all, "nAuthors", 'Number of authors', outdir )
plot.cont( all, "ninstitution", 'Number of institutions', outdir )
plot.cont( all, "female_first", 'Female first', outdir )
plot.cont( all, "female_last", 'Female last', outdir )

plot.cont( all, "h.first", 'H-index first', outdir )
plot.cont( all, "h.last", 'H-index last', outdir )


########################################
# plot impact factor density
########################################
p <- ggplot( data = all, aes( x = IF ) ) +
		geom_density( fill = '#31a354' ) +
		geom_vline( xintercept = 3, colour = 'gray30', linetype="dashed" ) +
		geom_vline( xintercept = 5, colour = 'gray30', linetype="dashed" ) +
		geom_vline( xintercept = 10, colour = 'gray30', linetype="dashed" ) +
		xlim( 0, 25 ) +
		xlab( 'Impact factor' ) +
		ylab( 'Density' ) +
		theme_classic( base_size = 14 ) +
		theme( legend.position = 'none', axis.title = element_text( face = "bold" ) )

ggsave( p, file = paste( outdir, '/density__impact_factor.png', sep = '' ), dpi = 200, width = 4, height = 7 )

if_data <- data.frame( var = 'if<=5', n = (sum( all$IF <= 5, na.rm = TRUE )), perc = ( 100 * sum( all$IF <= 5, na.rm = TRUE ) / nrow( all ) ) )
write.csv( if_data, file = paste0( outdir, '/density__impact_factor.csv' ) )

