
load("Rcache/selbias.RData")
if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf(file="../Figs/selbias_bw.pdf", width=5, height=6, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",col.main="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  pdf(file="../Figs/selbias.pdf", width=5, height=6, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mfrow=c(3,1))
par(mar=c(5.1, 1.1, 3.1, 1.1))

for(i in 1:3) {
  temp <- hist(ve[,i], breaks=seq(0, 0.25, len=251), plot=FALSE)
  x <- rep(temp$breaks, rep(2, length(temp$breaks)))*100
  y <- c(0,rep(temp$density, rep(2, length(temp$counts))),0)
  truth <- c(2.5,5,7.5)[i]
  plot(x, y, main=paste("True variance explained = ", truth, "%", sep=""),
       yaxt="n", ylab="",       ylim=c(0,23), yaxs="i", type="l",
       xlab="Estimated percent variance explained", xlim=c(0,25), xaxs="i")

  thr <- 1-10^(-2/250*3)
  lo <- min(temp$breaks[temp$breaks >= thr])*100

  lod <- -250/2*log10(1-ve[,i])
  me <- mean(100*ve[lod>=3,i])
  power <- mean(lod>=3)*100
  bias <- (me - truth)/truth*100
  

  xx <- x[x>=lo]
  yy <- y[x>=lo]
  yy[1] <- 0
  if(bw) {
    polygon(xx,yy, density=10, lwd=2)
    abline(v=truth, lwd=2)
    abline(v=me, lwd=2, lty=2)
  } else {
    polygon(xx, yy, col=color[1])
    abline(v=truth, lwd=2, col=color[2])
    abline(v=me, lwd=2, col=color[3])
  }

  text(20, 20, paste("Power = ", round(power), "%", sep=""), adj=c(0, 0.5))
  text(20, 17, paste("Bias = ", round(bias), "%", sep=""), adj=c(0, 0.5))
}

dev.off()
