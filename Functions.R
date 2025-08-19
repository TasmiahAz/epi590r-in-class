x <- c(100, 200, 300, 400, 500)

new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

new_mean(x = x)

new_mean(x = c(9, 41, 78))

y <- c(41,42,43,44,45,46,47)

new_mean(y)


# starting out with a number to test
z <- 5
# I want my function to return this number
z^2
square <- function(z) {
	square_val <- z*z
	return(square_val)
}

# testing it out
square(z)
square(53)




# add two numbers function
add_two_numbers <- function(p,q) {
	add_val <- p+q
	return(add_val)
}

add_two_numbers(4,6)

add_two_numbers(5,10)

add_two_numbers(p=5,q=26)





#using our new_mean() function to calculate proportions and add multiplier as an argument

prop <- function(x, multiplier) {
	n <- length(x)
	mean_val <- multiplier*sum(x) / n
	return(mean_val)
}

x <- c(0, 1, 1)

prop(x, multiplier = 100)

prop(x, multiplier = 1)

prop(x = c(1, 0, 1, 0), multiplier = 1)

prop(x = c(1, 0, 1, 0), multiplier = 100)

#2.5 Exercise 1


raise <- function(x, power) {
	result <- x^power
	return(result)
}

#test
raise(x=2, power=3)

raise(x=12, power=2)


#2.5 Exercise 2

#Making a default argument
raise <- function(x, power) {
	result <- x^2
	return(result)
}

# test
raise(x = 5)

raise(x = 3)

raise(x = 3, power = 3)





#2.6 Exercise 1
std <- function(x) {
	denominator <- length(x) - 1
	mean_x <- mean(x)
	differences <- x - mean_x
	squared_differences <- differences^2
	numerator <- sum(squared_differences)
	standard_dev <- sqrt(numerator/denominator)
	if (length(x)<=1) {standard_dev="NA"}
	else{standard_dev <- standard_dev}
	return(standard_dev)
}


x <- c(23452, 1234, 235, 34634)

std(x)

#another way to do
std <- function(x) {
	if (length(x)<=1) {return(NA)
	}else{denominator <- length(x) - 1
	mean_x <- mean(x)
	differences <- x - mean_x
	squared_differences <- differences^2
	numerator <- sum(squared_differences)
	standard_dev <- sqrt(numerator/denominator)
	return(standard_dev)}
}


x <- c(23452, 1234, 235, 34634)

std(x)



#another way to do
std <- function(x) {
	denominator <- length(x) - 1
	if (denominator <= 0) {return(NA)
		} else {
			mean_x <- mean(x)
			differences <- x - mean_x
			squared_differences <- differences^2
			numerator <- sum(squared_differences)
			standard_dev <- sqrt(numerator/denominator)
			return(standard_dev)}
			}


x <- c(23452, 1234, 235, 34634)

std(x)


#2.6 Exercise 2

std <- function(x, na.rm = TRUE) {
	if (na.rm == TRUE) {
		x <- na.omit(x)
	}
	if (length(x) <= 1) {
		return(NA)
	} else {
		denominator <- length(x) - 1
		mean_x <- mean(x)
		differences <- x - mean_x
		squared_differences <- differences^2
		numerator <- sum(squared_differences)
		standard_dev <- sqrt(numerator / denominator)
		return(standard_dev)
	}
}
x <- c(23452, 1234, 235, 34634)

x <- c(23452, 1234, 235, NA, 34634, NA, NA)

std(x)






#2.6 Exercise 3
library(tidyverse)
library(gtsummary)

# load and clean data
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


std <- function(x, na.rm = TRUE) {
	if (na.rm == TRUE) {
		x <- na.omit(x)
	}
	if (length(x) <= 1) {
		return(NA)
	} else {
		denominator <- length(x) - 1
		mean_x <- mean(x)
		differences <- x - mean_x
		squared_differences <- differences^2
		numerator <- sum(squared_differences)
		standard_dev <- sqrt(numerator / denominator)
		return(standard_dev)
	}
}


std(nlsy$income, na.rm = TRUE)


sd(nlsy$income, na.rm = TRUE)
