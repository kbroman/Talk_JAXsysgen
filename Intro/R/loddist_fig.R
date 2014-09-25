if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf(file="../Figs/loddist_bw.pdf", width=4.5, height=4.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black", #bty="n",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  pdf(file="../Figs/loddist.pdf", width=4.5, height=4.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white", #bty="n",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}  
#par(mar=c(5.1,0.1,0.1,0.1))


file <- "Rcache/loddist.RData"
if(file.exists(file)) {
  load(file)
} else {
  set.seed(12201969)
  x <- seq(0,5,length=513)
  y1 <- dchisq(x*2*log(10), 1)*(2*log(10))
  y2 <- density(apply(matrix(rchisq(15000000,1),ncol=150),1,max)/(2*log(10)),
                from=0, to=max(x), n=1024, width=0.4)
  save(x, y1, y2, file=file, compress=TRUE)
}
par(mar=c(4.1,1.1,0.1,1.1))
plot(x, y1, type="l", ylim=c(0,1), lwd=2, lty=2,
     xlab="LOD score", ylab="", yaxt="n", xaxs="i", yaxs="i")
lines(y2$x,y2$y,lwd=2)
dev.off()
