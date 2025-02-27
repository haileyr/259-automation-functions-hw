
plus_minus_SD <- function(vec, num_of_SDs = 1) {
  mean_value <- mean(vec)
  sd_value <- sd(vec)
  lower_bound <- mean_value - num_of_SDs * sd_value
  upper_bound <- mean_value + num_of_SDs * sd_value
  return(c(lower_bound, upper_bound))
}

