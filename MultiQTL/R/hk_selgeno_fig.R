if(bw) source("colors_bw.R") else source("colors.R")

if(!bw) {
  pdf(file="../Figs/hk_selgeno.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
} else {
  pdf(file="../Figs/hk_selgeno_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
}

data(hyper)

cx <- 0.6
par(mfrow=c(1,2), cex=cx, cex.lab=1/cx, cex.axis=1/cx, las=1)
x <- pull.geno(hyper)[,"D4Mit214"]
u <- runif(length(x), -0.075, 0.075)
y <- hyper$pheno[,1]
ex <- order(y)[c(1:46, 251-(1:45))]

par(mar=c(5.1,4.1,0.1,1.1))
plot(y ~ x, type="n", xlab="Genotype", ylab="phenotype",
     xlim=c(0.5,2.5), xaxs="i", xaxt="n")
if(bw) {
  abline(v=1:2, lty=3, col="black")
} else {
  abline(v=1:2, lty=3, col="white")
}  
points(x+u, y)
axis(side=1, at=1:2, labels=c("AA","AB"))
me <- tapply(y, x, mean)
segments((1:2)-0.15, me, (1:2)+0.15, me, lwd=3, col=color[1])
abline(lm(y~x)$coef, col=color[1])

par(mar=c(5.1,5.1,0.1,0.1))
z <- x; z[-ex] <- 1.5
plot(y ~ z, type="n", xlab="Genotype", ylab="phenotype",
     xlim=c(0.5,2.5), xaxs="i", xaxt="n")
if(bw) {
  abline(v=1:2, lty=3, col="black")
  points(z+u, y, col=c("black", "gray")[(z==1.5) + 1])
} else {
  abline(v=1:2, lty=3, col="white")
  points(z+u, y, col=c("white", color[7])[(z==1.5) + 1])
}
axis(side=1, at=1:2, labels=c("AA","AB"))
me <- tapply(y, z, mean)
zz <- as.numeric(names(me))
segments(zz-0.15, me, zz+0.15, me, lwd=3, col=color[1])
abline(lm(y[ex]~z[ex])$coef, col=color[1])
abline(lm(y~z)$coef, col=color[1], lty=2)

dev.off()
