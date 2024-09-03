#######################################################################
# Script:	Descriptive stats																						#
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

# simple table
tbl_summary(
	lgbtq,
	by = adult,
	include = c(
		popularity, vote_average, original_language,
		vote_count, video
	),
	label = list(
		popularity ~ "Popularity",
		vote_average ~ "Average Votes",
		original_language ~ "Language of Film",
		vote_count ~ "Vote Count",
		video ~ "Video"
	),
	missing_text = "Missing"
)


