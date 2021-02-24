#!/usr/bin/env Rscript
#
# facet plots 
#
# w.m.otte@umcutrecht.nl
#
#########################

library( 'ggplot2' )
library( 'plyr' )
library( 'Hmisc' )
library( "cowplot" )

###################################################
# FUNCTIONS
###################################################

# manual colors (red, blue)
manual.colors <- c( "#f4a582", "#92c5de", "#0571b0", "#ca0020", "gray40" )
manual.colors.two <- c( "#0571b0", "#ca0020" )

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
# Plot cont.
##
plot.facet <- function( type = 'cont', df.cont, var2, label2, var1, label1, outdir, fillcolour = '#3690c0' )
{
	xlabel = 'Year'
    
    ylimits <- c( 0, 95 )
    if( type == 'cont' )
        ylimits <- c( 0, 65 )

	# get means and 95% confidence intervals
	df <- df.cont
	df$x <- as.factor( df.cont$year_group )
	df$y <- df.cont[, var1 ]
	df$group <- df.cont[, var2 ]

	if( type == 'cont' )
	{
		# get means and 95% confidence interval - with linear regression
		ds <- ddply( df, .( x, group ), summarise, mean = 100 * mean( y, na.rm = T ), 
												lower = 100 * ( mean( y, na.rm = T ) - 1.96 * std( y, na.rm = T ) ), 
												upper = 100 * ( mean( y, na.rm = T ) + 1.96 * std( y, na.rm = T ) ), N = length( y ) )
	} else {
		# percentage and 95% CI
		ds <- ddply( df, .( x, group ), summarise, 
				mean = 100 * sum( y, na.rm = TRUE ) / length( y ),							
				lower = 100 * get.prop.ci( k = sum( y, na.rm = TRUE ), n = length( y ), lower = TRUE ),
				upper = 100 * get.prop.ci( k = sum( y, na.rm = TRUE ), n = length( y ), lower = FALSE ), N = length( y ) )	
	}

	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	# Default bar plot
	p <- ggplot( ds, aes( x = x, y = mean, fill = group ) ) + 
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge() ) +
			geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +
			scale_y_continuous( breaks = number_ticks( 8 ), limits = ylimits ) +
		   	scale_fill_manual( values = c( '#E69F00', '#d95f0e' ) ) +
			ylab( label1 ) +
			xlab( xlabel ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'top', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) ) + labs( fill = label2 )

	# write to file
	ggsave( p, file = paste( outdir, '/', var2, '_', var1, '.png', sep = '' ), dpi = 200, width = 4, height = 7 )
	write.csv( ds, file = paste( outdir, '/', var2, '_', var1, '.csv', sep = '' ) )

	return( p )
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
	ds <- ddply( df, .( x ), summarise, mean = sum( y, na.rm = T ), N = length( y ) )
	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	p <- ggplot( ds, aes( x = x, y = mean ) ) +
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge(), fill = fillcolour ) +
			#geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +
			scale_y_continuous( breaks = number_ticks( 8 ), limits = c( 0, 95000 ) ) +
		   	ylab( paste0( ylabel, "" ) ) +
			xlab( xlabel ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) )

	# write to file
	ggsave( p, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.png', sep = '' ), dpi = 200, width = 4, height = 8 )

	# write corresponding number to csv
	write.csv( ds, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.csv', sep = '' ) )

	return( p )
}

###
# Plot cont.
##
plot.cont <- function( df.cont, var, ylabel = 'Y', outdir, fillcolour = '#3690c0', limits = NULL )
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
	ds <- ddply( df, .( x ), summarise, mean = mean( y, na.rm = T ), 
							lower = mean( y, na.rm = T ) - 1.96 * std( y, na.rm = T ), 
							upper = mean( y, na.rm = T ) + 1.96 * std( y, na.rm = T ), N = length( y ) )
	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	p <- ggplot( ds, aes( x = x, y = mean, ymin = lower, ymax = upper ) ) +
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge(), fill = fillcolour ) +
			geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +
		   	ylab( paste0( ylabel, "" ) ) +
			xlab( xlabel ) +
			theme_classic( base_size = 12 ) +
			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) )

	if( is.null( limits ) ) {
		p <- p + scale_y_continuous( breaks = number_ticks( 7 ) )
	} else {
		p <- p + scale_y_continuous( breaks = number_ticks( 7 ), limits = limits )
	}

	# write to file
	ggsave( p, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.png', sep = '' ), dpi = 200, width = 4, height = 7 )

	# write corresponding number to csv
	write.csv( ds, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.csv', sep = '' ) )

	return( p )
}

###
# Create stacked neg/pos rates
##
get_prepared_neg_pos <- function( all )
{
	# merge pos/neg words
	df <- all[ , colnames( all ) %in% c( "year_group", "neg.rate", "pos.rate" ) ]

	# wide to long
	df_long <- reshape2::melt( df, id = c( "year_group" ) ) 

	# get means and 95% confidence interval - with linear regression
	ds <- ddply( df_long, .( year_group, variable ), summarise, mean = 100 * sum( value, na.rm = TRUE ) / length( value ), N = length( value ) )
	levels( ds$variable ) <- c( 'negative', 'positive' )
	ds$year_group <- as.factor( ds$year_group )
	ds$variable <- factor( ds$variable, levels = rev( levels( ds$variable ) ) )

	levels( ds$year_group ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )
	return( ds )
}


###################################################
# END FUNCTIONS
###################################################


# make output directory
outdir <- 'out.plot.facets'
dir.create( outdir, showWarnings = FALSE )

# read prepared data
infile <- 'out.prepare/data.csv'  

# get data
all <- read.csv( infile, row.names = 1 )

# add helper variable
all$all <- as.factor( 'jif' )

# convert prob to percentage
all$gender_propFem <- all$gender_propFem * 100



###########################
# I. Figure 2
###########################


p1 <- plot_n_pub( all, 'trials', 'Trials (×1000)', outdir )
p2 <- plot.cont( all, "nAuthors", 'Authors', outdir )
p3 <- plot.cont( all, "gender_propFem", 'Proportion female', outdir )
p4a <- plot.cont( all, "h.first", 'H-index (first)', outdir )
p4b <- plot.cont( all, "h.last", 'H-index (last)', outdir )
p5 <- plot.cont( all, "ncountries", 'Countries', outdir )
p6 <- plot.cont( all, "ninstitution", 'Institutions', outdir )

# neg.rate + pos.rate in single figure
ds_neg_pos <- get_prepared_neg_pos( all )

# Stacked bias for medical disciplines"#f4a582", 
p7_8 <- ggplot( data = ds_neg_pos, aes( x = year_group, y = mean, group = variable, fill = variable ) ) + 
		geom_col( colour = 'gray20' ) +
		scale_y_continuous( breaks = number_ticks( 12 ) ) +
		scale_fill_manual( values = c( "#3690c0", "#92c5de" ) ) + #, "#0571b0", '#E69F00', '#d95f0e' ) ) +
		ylab( "Frequency (%)" ) +
		xlab( "Time" ) +
		theme_classic( base_size = 12 ) +
		geom_text( aes( label = round( mean, 1 ) ), size = 4, position = position_stack( vjust = 0.5 ) ) + 
		theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) ) +
		theme( axis.text.y = element_blank(), axis.ticks.y = element_blank() )

# arrange the three plots in a single row
p <- plot_grid(
	p1 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Number of trials", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	p2 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Number of authors", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	
	# REMOVED in rebuttal
	#p3 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Percentage female", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	#p4a + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  H-index (first)", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	#p4b + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  H-index (last)", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	p5 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Countries", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	p6 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Institutions", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	#p7_8 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Word frequencies", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	
	align = 'vh',
	labels = c( "A", "B", "C", "D", "E", "F", "G", "H" ),
	label_size = 13,
	hjust = -1,
	nrow = 2
)

# save to disk
save_plot( paste0( outdir, "/FACET__Figure_2__ytitles.png" ), p, base_height = NULL, base_width = 10, base_asp = 1 )

# arrange the three plots in a single row
p <- plot_grid(
	#p1 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Number of trials", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	#p2 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Number of authors", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	
	# REMOVED in rebuttal
	p3 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Percentage female", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	#p7_8 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Word frequencies", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	
	p4a + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  H-index (first)", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	p4b + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  H-index (last)", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	#p5 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Countries", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	#p6 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = "  Institutions", hjust = 0, vjust = 1, size = 5, fontface = 2, colour = 'gray40' ),
	
	align = 'vh',
	labels = c( "", "", "", "", "E", "F", "G", "H" ),
	label_size = 13,
	hjust = -1,
	nrow = 1
)

# save to disk
save_plot( paste0( outdir, "/FACET__Figure_2__ytitles__Suppl.png" ), p, base_height = NULL, base_width = 10, base_asp = 1.7 )


###################################
## II. Combine bias plots
###################################

p1 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_allocation_prob', 'Allocation bias', outdir )
p2 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_random_prob', 'Randomization bias', outdir )
p3 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_blinding_pts_prob', 'Blinding of people bias', outdir )
p4 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_blinding_outcome_prob', 'Blinding of outcome bias', outdir )
p5 <- plot.facet( 'bin', all, 'all', 'All', 'trial_registered', 'RCT registration', outdir )
p6 <- plot.facet( 'bin', all, 'all', 'All', 'consort_mentioned', 'CONSORT Statement', outdir )

# arrange the three plots in a single row
pcombined <- plot_grid(

	p1 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Allocation bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
	p2 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Randomization bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
	p3 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Blinding of people bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
	p4 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Blinding of outcome bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
	p5 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " RCT registration (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
	p6 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " With CONSORT Statement (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),

	align = 'vh',
	labels = c( "A", "B", "C", "D", "E", "F" ),
	label_size = 14,
	hjust = -1,
	nrow = 3
)

# save
save_plot( paste0( outdir, "/FACET_all__ytitles.png" ), pcombined, base_height = NULL, base_width = 6, base_asp = 0.5 )



######################################
## III. Iterate over JIF 3, 5 and 10
######################################
for( jif in c( 'impact_factor_cat3', 'impact_factor_cat5', 'impact_factor_cat10' ) )
{
	# plots
	p1 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_allocation_prob', 'Allocation bias', outdir )
	p2 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_random_prob', 'Randomization bias', outdir )
	p3 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_blinding_pts_prob', 'Blinding of people bias', outdir )
	p4 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_blinding_outcome_prob', 'Blinding of outcome bias', outdir )
	p5 <- plot.facet( 'bin', all, jif, 'Impact factor', 'trial_registered', 'RCT registration', outdir )
	p6 <- plot.facet( 'bin', all, jif, 'Impact factor', 'consort_mentioned', 'CONSORT Statement', outdir )

	# arrange the three plots in a single row
	pcombined <- plot_grid(

		p1 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Allocation bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
		p2 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Randomization bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
		p3 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Blinding of people bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
		p4 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " Blinding of outcome bias (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
		p5 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " RCT registration (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),
		p6 + theme( legend.position = "none", axis.title = element_blank() ) + annotate("text", -Inf, Inf, label = " With CONSORT Statement (%)", hjust = 0, vjust = 1, size = 4, fontface = 2, colour = 'gray40' ),

		align = 'vh',
		labels = c( "A", "B", "C", "D", "E", "F" ),
		label_size = 14,
		hjust = -1,
		nrow = 3
	)

	# extract a legend that is laid out horizontally
	legend_b <- get_legend( p1 + guides( color = guide_legend( nrow = 1 ) ) + theme( legend.position = "top" ) )

	# add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
	p <- plot_grid( pcombined, legend_b, ncol = 1, rel_heights = c(1, .05))

	# save
	save_plot( paste0( outdir, "/FACET_", jif, ".png" ), p, base_height = NULL, base_width = 7, base_asp = 0.6 )
}

