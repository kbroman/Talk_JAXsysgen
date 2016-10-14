set.seed(12399540+SUB)

n.ind <- 250
n.sim <- 100000/8

hsq <- c(0.025, 0.05, 0.075)
lod <- matrix(ncol=length(hsq), nrow=n.sim)
a <- sqrt(hsq/(1-hsq))

for(i in 1:n.sim) {
  for(j in seq(along=hsq)) {

    x <- sample(c(-1,1), n.ind, repl=TRUE)
    y <- x*a[j] + rnorm(n.ind)

    rss0 <- sum(lm(y ~ 1)$resid^2)
    rss1 <- sum(lm(y ~ factor(x))$resid^2)
    lod[i,j] <- (250/2)*log10(rss0/rss1)
  }
  if(i==round(i, -1)) cat(i, "\n")
}
ve <- 1 - 10^(-2*lod/n.ind)
save(lod, ve, file="selbiasSUB.RData", compress=TRUE)
