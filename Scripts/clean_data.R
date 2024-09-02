
#######################################################################
# Script: R Bootcamp - Final Project							                    #
# Programmer:   Santino Diaz-Palma                                    #
#                                                                     #
# Purpose: This script is how I created my clean dataset to be used   #
#               in my brief analysis and Quarto render. 					    #
#                                                                     #
#######################################################################

#Load packages
library(tidyverse)
library(broom)
library(gtsummary)
library(here)

#Lets call in our dataset and remove two variables (overview and genre) as they are not relevant
#	to our research question.
lgbtq_cols <- c("id","title","original_title","original_language","overview", "release_date",
								"popularity", "vote_average", "vote_count", "adult", "video", "genre_ids")
lgbtq <- read_csv(here::here("Data", "movieData.csv"),
									na = c("-1", "-2", "-3", "-4", "-5", "-998", "xx"),
									skip = 1,
									col_names = lgbtq_cols,
									col_select = -c("overview","genre_ids"))

#Lets check our NA values and see if were missing anything or need to do anything
summary(lgbtq)
	## Lets look at the languages more closely
			language_counts <- summary(as.factor(lgbtq$original_language))
			print(language_counts)
	## for conciseness, lets re-do  some code to reformat language to only
			### languages that ahve over 100 titles, and label those under 100 as "other"
						lgbtq <- read_csv(here::here("Data", "movieData.csv"),
												na = c("-1", "-2", "-3", "-4", "-5", "-998", "xx"),
												skip = 1,
												col_names = lgbtq_cols,
												col_select = -c("overview","genre_ids")) |>
			###	Convert original_language to a factor
						mutate(original_language = as.factor(original_language)) |>
			### Replace languages not in the languages_above_100 list with "Other"
						mutate(original_language = ifelse(original_language %in% languages_above_100,
																					as.character(original_language), "Other"))
			### confirm changes
						print(table(lgbtq$original_language))

# Now that Language is fixed, there are some other variables that could use some additional
# 	clarity. Let's change the levels to be more intuitive.
		lgbtq_cats <- lgbtq |>
			mutate(
				 adult = factor(adult, labels = c("No", "Yes")),
				 video = factor(video, labels = c("No", "Yes")))

# check to make sure coding is correct
	count(lgbtq_cats, adult)

# remove observations with any missing data
	lgbtq_cc <- na.omit(lgbtq_cats)

# check to make sure it worked
	count(lgbtq_cc, release_date)
	summary(lgbtq_cc)

## Create folder for clean data (if it doesn't exist already)
if (!dir.exists(here::here("data", "clean"))) {
	dir.create(here::here("data", "clean"))
}

## Save complete data in (newly made) folder
write_rds(lgbtq, here::here("data", "clean", "lgbtq_clean.rds"))




