# this program is meant to show that having a different
# value of rho, actually affects how your time series formula
# graph will look like


  

x = vector()

# This will run the timeseries formula
# with specific rho value and takes number of times
# from a user, and run it that number of times
time_series_func <- function(rho, num_times) {
  # generate either -1 or 1 random walk sample for num_times
  errors = sample(c(-1,1), num_times, replace=TRUE)
  
  # run the formula for num_times and see where the position it ends
  for (i in 1:num_times) {
    if (i == 1) {
      x[i] = errors[i]
    }
    else {
      x[i] = rho * x[i-1] + errors[i]
    }
    print(paste0("error[", i, "] = ", errors[i], ",  Current position: ", x[i]))
  }  
}

# guess what I have found

# when rho < 1
# It keeps pulling down the value, it will remain close to the 0
time_series_func(0.5, 100)
# when rho = 1, which is random walk
# You don't know where it goes
time_series_func(1, 100)
# when rho > 1, it is called explosive
# It keeps growing and growing, the position value will become infinite
# What this is funny is, once it gets the direction,
# it will be explosve to the side that it is heading, whether it is
# positive or negative
time_series_func(1.1, 100)



