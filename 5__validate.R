#!/usr/bin/env Rscript
# Determine correspondance between RR and human ratings
#######################################################
library( 'plyr' )
library( 'ggplot2' )
library( 'caret' )
library( 'cowplot' )
library( 'rel' ) # ckap

#######################################################
# FUNCTIONS
#######################################################

###
# ggplot style function.
##
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

## get data
get_data <- function()
{
	# read combined data
	df <- read.csv( 'input.validation/human_and_robotreviewer.csv.gz', row.names = 1 )
	
	df$judgement_binary <- 'Low_risk'
	df[ df$judgement != 'Low risk', 'judgement_binary' ] <- 'High-Unclear_risk'
	
	return( df )
}

###
# Process RoB domain
##
process <- function( df, tag, cl, title )
{
	# select tag
	sub <- df[ df$bias %in% tag, ]	

	# select col
	sub$cl <- sub[ , cl ]
	
	# remove NA
	sub <- sub[ !is.na( sub$cl ), ]
	
	# summary data
	sum <- ddply( sub, c( "judgement_binary" ), summarise, median = median( cl ), K = length( cl ) )
	sum$N <- nrow( sub )
	sum$domain <- tag
	
	# generate binary RoB outcome
	sub$class1 <- 'Low_risk'
	sub$class1[ sub$cl > 0.5 ] <- 'High-Unclear_risk'
	
	acc <- caret::confusionMatrix( as.factor( sub$judgement_binary ), as.factor( sub$class1 ) )

	acc1 <- round( 100 * acc$overall[ c( 'Kappa', 'Accuracy', 'AccuracyLower', 'AccuracyUpper' ) ], 1 )
	acc2 <- round( 100 * acc$byClass[ c( 'Sensitivity', 'Specificity' ) ], 1 )
	
	sum$acc <- paste0( acc1[ 'Accuracy' ], " (CI: ", acc1[ 'AccuracyLower' ], '–', acc1[ 'AccuracyUpper' ], ")" )

	## Sensitivity with 95% confidence interval 
	mtx <- table( as.factor( sub$judgement_binary ), as.factor( sub$class1 ) )
	sensMean <- caret::sensitivity( mtx )
	sens_errors <- sqrt( caret::sensitivity( mtx ) * ( 1 - caret::sensitivity( mtx ) ) / sum( mtx[ , 1 ] ) )
	sensLower <- caret::sensitivity( mtx ) - 1.96 * sens_errors
	sensUpper <- caret::sensitivity( mtx ) + 1.96 * sens_errors
	
	## Specificity with 95% confidence interval 
	specMean <- caret::specificity( mtx )
	spec_errors <- sqrt( caret::specificity( mtx ) * ( 1 - caret::specificity( mtx ) ) / sum( mtx[ , 2 ] ) )
	specLower <- caret::specificity( mtx ) - 1.96 * spec_errors
	specUpper <- caret::specificity( mtx ) + 1.96 * spec_errors
	
	#sum$kappa <- acc1[ 'Kappa' ]
	sum$sensitivity <- paste0( round( 100 * sensMean, 1 ), " (CI: ", round( 100 * sensLower, 1 ), '-', round( 100 * sensUpper, 1 ), ")" )
	sum$specificity <- paste0( round( 100 * specMean, 1 ), " (CI: ", round( 100 * specLower, 1 ), '-', round( 100 * specUpper, 1 ), ")" )
		
	caret::confusionMatrix( table( sub$judgement_binary, sub$class1 ) )
	
	# Cohen's kappa with Fleiss corrected standard error formula
	tmp <- ckap( data = cbind( as.factor( sub$judgement_binary ), as.factor( sub$class1 ) ), weight = "unweighted", conf.level = 0.95 )
	tmp$kappa2 <- round( 100 * tmp$est, 1 )
	tmp$kappa2_lb <- round( 100 * tmp$lb, 1 )
	tmp$kappa2_ub <- round( 100 * tmp$ub, 1 )
	
	sum$kappa <- paste0( tmp$kappa2, " (CI: ", tmp$kappa2_lb, '-', tmp$kappa2_ub, ")" )
	
	# set factor
	sub$judgement_binary <- as.factor( sub$judgement_binary )
	#sub$judgement <- as.factor( sub$judgement )
	levels( sub$judgement_binary ) <- c( 'High-Unclear', 'Low' )
	#levels( sub$judgement ) <- c( 'High', 'Low' )
	
	fillcolour <- '#31a354' 
	p <- ggplot( data = sub, aes( x = judgement_binary, y = cl ) ) + 
		geom_violin( fill = fillcolour ) +
		geom_hline( yintercept = 0.5, colour = 'gray30', linetype = "dashed" ) +
		scale_y_continuous( breaks = number_ticks( 6 ), limits = c( 0, 0.95 ) ) +
		ylab( "Predicted risk" ) +
		xlab( title ) +
		theme_classic( base_size = 10 ) +
		theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 0, hjust = 0.5 ) )
	
	out <- list( text = sum, plot = p )
	
	return( out )
}

#######################################################
# END FUNCTIONS
#######################################################

# generate output dir
outdir <- 'out.validate'
dir.create( outdir, showWarnings = FALSE )

# read data
df <- get_data()

# get accuracy numbers
m_allocation <- process( df, tag = "2. Allocation concealment?", cl = "RoB_allocation_prob", title = "Allocation bias" )
m_random <- process( df, tag = "1. Random sequence generation?", cl = "RoB_random_prob", title = "Randomization bias" )
m_blinding_pts <- process( df, tag = "3. Blinding of participants and personnel?", cl = "RoB_blinding_pts_prob", title = "Blinding of people bias" )
m_blinding_outcome <- process( df, tag = "4. Blinding of outcome assessment?", cl = "RoB_blinding_outcome_prob", title = "Blinding of outcome bias" )

# combine
all <- rbind( rbind( rbind( m_allocation$text, m_random$text ), m_blinding_pts$text ), m_blinding_outcome$text )

# write to disk
write.csv( all, file = paste0( outdir, '/accuracies.csv' ) )

# total 41,358
sum( all$K )

# arrange the three plots in a single row
p <- plot_grid(
	m_random$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Randomization bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	m_allocation$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Allocation bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	m_blinding_pts$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Blinding of people bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	m_blinding_outcome$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Blinding of outcome bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	
	align = 'vh',
	labels = c( "A", "B", "C", "D" ),
	label_size = 10,
	hjust = -1,
	vjust = 2,
	nrow = 2,
	ncol = 2
)

# save to disk
save_plot( paste0( outdir, "/Figure_validation.png" ), p, base_height = NULL, base_width = 6, base_asp = 0.8 ) 


