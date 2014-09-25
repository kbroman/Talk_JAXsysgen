if(bw) source("colors_bw.R") else source("colors.R")


for(i in 0:9) {
  file <- paste("Rcache/perm1_0", i, ".RData", sep="")
  if(file.exists(file)) {
    attach(file)
    if(i==0) op <- operm
    else op <- c(op, operm)
    detach(2)
  }
}

if(bw) {
  pdf(file="../Figs/perm_hist_bw.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/perm_hist.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(4.1, 1.1, 1.1, 1.1))

temp <- hist(op, breaks=2*sqrt(length(op)), main="", yaxt="n", ylab="",
             xlab="", border=bgcolor, axes=FALSE, xlim=c(0,max(op)), xaxs="i")
axis(line=-1, side=1)
title(xlab="Genome-wide maximum LOD score", line=2)
x <- rep(temp$breaks, rep(2, length(temp$breaks)))
#x <- x[-c(1, length(x))]
y <- c(0,rep(temp$counts, rep(2, length(temp$counts))),0)
lines(x,y, col=color[1], lwd=2)
u <- par("usr")
segments(0,0,u[2],0)

dev.off()

