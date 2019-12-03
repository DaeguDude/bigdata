random_walk <- function(number) {
  x_t = 0
  for (i in 1:number) {
    # random number generation in R either +1, -1
    error = sample(c(1,-1), 1)
    x_t = x_t + error
    print(paste0(i, "th error: ", error))
  
  }
  print(paste0("random walk for ", number, " times are finished"))
  print(paste0("current position:", x_t))
}