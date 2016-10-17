# simulate multivariate example

library(qtl)
library(broman)

set.seed(2511772)

n <- 200
a <- 20

map <- sim.map(rep(100, 4), n.mar=21, anchor=TRUE, include=FALSE, eq=TRUE)

x <- sim.cross(map, type="bc", n.ind=n,
               model=rbind(c(1,52,0), c(2,52,0),c(3,52,0), c(4,42,0), c(4, 62,0)))

y <- cbind(x$qtlgeno %*% (a*c(1, 0, 1, 1, 0)),
           x$qtlgeno %*% (a*c(0, 1, 1, 0, 1))) + rmvn(n, c(0,0), rbind(c(1, 0.5), c(0.5, 1)))

x <- calc.genoprob(x, step=1)
x$pheno <- y
out <- scanone(x, phe=1:2, method="hk")

out.mvn <- scanone.mvn(x, y)
plot(out.mvn)
plot(out, lod=1:2, col=c("slateblue", "hotpink"), add=TRUE)
