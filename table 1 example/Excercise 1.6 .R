library(tidyverse)
library(gtsummary)

# Load and clean data
nlsy_cols <- c(
	"glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
	"id", "nsibs", "samp", "race_eth", "sex", "region",
	"income", "res_1980", "res_2002", "age_bir"
)
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols
) |>
	mutate(
		region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
		sex_cat = factor(sex, labels = c("Male", "Female")),
		race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
		eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
		glasses_cat = factor(glasses, labels = c("No", "Yes"))
	)

#Ex 3: Making a tbl_summary(), including categorical region, race/ethnicity, income, and the sleep variables
tbl_summary(nlsy,
						include = c(region_cat, race_eth_cat, income, sleep_wkdy, sleep_wknd))

#or another way to code the above code
tbl_summary(nlsy,
						include = c(region_cat, race_eth_cat, income, starts_with("sleep")))

#adding labels to the above code
tbl_summary(nlsy,
						include = c(region_cat, race_eth_cat, income, sleep_wkdy, sleep_wknd),
            label = list(region_cat ~ "Region", race_eth_cat ~ "Race/Ethnicity", income ~ "Income", sleep_wkdy ~ "Sleep Weekday", sleep_wknd ~ "Sleep Weekend"))

#or
tbl_summary(nlsy,
						include = c(region_cat, race_eth_cat, income, starts_with("sleep")),
						label = list(region_cat ~ "Region", race_eth_cat ~ "Race/Ethnicity", income ~ "Income", sleep_wkdy ~ "Sleep Weekday", sleep_wknd ~ "Sleep Weekend"))

#Ex 4: Stratifying the table by sex and adding a p-value comparing the sexes and an overall column combining both sexes.
tbl_summary(nlsy,
					 by = sex_cat,
					 include = c(region_cat, race_eth_cat, income, starts_with("sleep")),
					 label = list(region_cat ~ "Region", race_eth_cat ~ "Race/Ethnicity", income ~ "Income", sleep_wkdy ~ "Sleep Weekday", sleep_wknd ~ "Sleep Weekend")) |>
	# change the test used to compare sex_cat groups
	add_p(test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test"
	)) |>
	# add a total column with the number of observations
	add_overall(col_label = "**Total** N = {N}")

#Ex 5: For the income variable, showing the 10th and 90th percentiles of income with 3 digits, and for the sleep variables, showing the min and the max with 1 digit.
tbl_summary(nlsy,
						by = sex_cat,
						include = c(region_cat, race_eth_cat, income, starts_with("sleep")),
						label = list(region_cat ~ "Region", race_eth_cat ~ "Race/Ethnicity", income ~ "Income", sleep_wkdy ~ "Sleep Weekday", sleep_wknd ~ "Sleep Weekend"),
statistic = list(starts_with("sleep") ~ "min = {min}; max = {max}",
								 income = "({p10} to {p90})"),
digits = list(starts_with("sleep")~c(1,1), income~c(3,3)))
