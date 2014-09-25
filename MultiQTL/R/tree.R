if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf(file="../Figs/tree_bw.pdf", width=4, height=4, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/tree.pdf", width=4, height=4, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=c(0,0,0,0))
plot(0,0,type="n", xlab="", xaxt="n", ylab="", yaxt="n", bty="n",
     xlim=c(5,85), ylim=c(0,100))
segments(c(10,30,50,50),c(10,50,90,90),c(30,50,30,70),c(50,10,50,50),lwd=2)
points(c(10,30,50,50,70),c(10,50,90,10,50),cex=9,lwd=2,pch=c(15,16,16,15,15),col=bgcolor)
points(c(10,30,50,50,70),c(10,50,90,10,50),cex=9,lwd=2,pch=c(15,16,16,15,15)-15)
text(c(10,50,70),c(10,10,50),c("100","80","20"),cex=2)
if(bw) {
  text(50,90,expression(q[2]),cex=2)
  text(30,50,expression(q[1]),cex=2)
} else {
  text(50,90,expression(q[2]),cex=2,col=color[3])
  text(30,50,expression(q[1]),cex=2,col=color[1])
}
text(17,30,"A",cex=1.6,adj=1)
text(43,30,"H or B",cex=1.6,adj=0)
text(37,70,"A",cex=1.6,adj=1)
text(63,70,"H or B",cex=1.6,adj=0)
dev.off()
