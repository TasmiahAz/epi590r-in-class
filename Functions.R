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
