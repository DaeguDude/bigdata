coinflip <- c("heads", "tails")

# I will store number of heads to b, each time the event occurs
b = 0
for (i in 1:50000) {
  coinflip_result =  sample(coinflip, size = 386, replace=TRUE)
  num_heads = sum(coinflip_result == 'heads')
  b[i] = num_heads
}

hist(b, xlim=c(0,386), xlab="number of heads", main="b histogram", freq=FALSE)
curve(dnorm(x, mean=mean(b), sd=sd(b)), add=TRUE, col="red")

