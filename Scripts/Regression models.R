#######################################################################
# Script:	Regression																									#
# Project: R Bootcamp - Final Project							                    #
# Programmer:   Santino Diaz-Palma                                    #
#                                                                     #
# Purpose: This script is how I created my Table 1 and other relevant #
#               descriptive statistics. 													    #
#                                                                     #
#######################################################################

#Load packages
library(tidyverse)
library(broom)
library(gtsummary)
library(here)

#load in clean data
lgbtq_cols <- c("id","title","original_title","original_language","overview", "release_date",
								"popularity", "vote_average", "vote_count", "adult", "video", "genre_ids")
languages_above_100 <- c("en", "de", "fr", "it", "ja", "es", "pt")
lgbtq <- read_csv(here::here("Data", "movieData.csv"),
									na = c("-1", "-2", "-3", "-4", "-5", "-998", "xx"),
									skip = 1,
									col_names = lgbtq_cols,
									col_select = -c("overview","genre_ids")) |>
	mutate(original_language = as.factor(original_language)) |>
	mutate(original_language = ifelse(original_language %in% languages_above_100,
																		as.character(original_language), "Other")) |>
	mutate(adult = factor(adult, labels = c("No", "Yes")),
				 video = factor(video, labels = c("No", "Yes")),
				 original_language = factor(original_language, labels =
				 													 	c("German",
				 													 		"English",
				 													 		"French",
				 													 		"Italian",
				 													 		"Japanese",
				 													 		"Spanish",
				 													 		"Other",
				 													 		"Portuguese")))
#Linear Model
linear_model <- lm(popularity ~ original_language + adult + vote_average,
											data = lgbtq
)
tbl_regression(
	linear_model,
	# include the intercept
	intercept = TRUE,
	# relabel the variables
	label = list(
		original_language ~ "Language of Film",
		vote_average ~ "Average Number of Votes",
		adult ~ "Adult film (y/n)"
	)
)

#Figure 1
lang_freq_table <- table(lgbtq$original_language)
lang_data <- as.data.frame(lang_freq_table)
names(lang_data) <- c("Language","Number of Films")
figure1 <- ggplot(lang_data, aes(x = Language, y = `Number of Films`)) +
	geom_bar(stat = 'identity', fill = 'skyblue') +
	labs(title = 'Most Popular Languages amognst LGBTQ+ films', x = 'Language', y ='Number of Films') +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Save Figure as PDF
ggsave(here::here("Results", "Figure_1.pdf"), plot = figure1, device = "pdf")

#Function
stdevi <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	squared_diff <- (x-mean_val)^2
	variance <- sum(squared_diff) / (length(x) - 1)
	stdev <- sqrt(variance)
	return(stdev)
}
#confirm that function works as intended
	stdevi(lgbtq$vote_average)	#function
	sd(lgbtq$vote_average)		#built-in sd
