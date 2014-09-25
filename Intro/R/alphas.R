source("colors.R")
attach("Rcahce/alphas.RData")
alphas <- alphas.data[,c(1,2,3,4,7)]


#postscript("../Figs/fig3.eps", horiz=FALSE, height=6, width=7.5,
#           onefile=FALSE, paper="special", pointsize=16)
pdf(file="../Figs/alphas.pdf", width=8.5, height=6.5, pointsize=12, onefile=TRUE)
par(las=1, mar=c(5.1,5.1,4.1,2.1))
par(fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor)
plot(alphas[,1], alphas[,5], lwd=2, xlab="Chromosome length (cM)",
     ylab = "", type="n", ylim=c(0.0015, 0.0041))
#o <- lm(alphas[,5] ~ alphas[,1])
#abline(o$coef, lwd=2)
L <- alphas[,1]
x <- seq(min(L),max(L), length=501)
lines(x, 1-0.95^(x/sum(L)), lty=2)
mtext(side=2, line=4, expression(paste("Chromosome-specific ", alpha)), las=0)
#mtext(side=2, line=4, expression(alpha[i]), las=1)
for(i in c(3,1)) {
  n <- 5e6
  x <- alphas[,1]+(i-2)*0.1
  points(x, alphas[,i+1], col=color[i], lwd=2, cex=0.6,
         pch=c(1,0,0)[i])  
  se <- sqrt(alphas[,i+1]*(1-alphas[,i+1])/n)
  segments(x, alphas[,i+1]-2*se, x, alphas[,i+1]+2*se,
           col=color[i], lwd=2)
}
dev.off()

detach(2)
