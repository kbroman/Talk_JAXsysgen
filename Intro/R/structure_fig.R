if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf(file="../Figs/structure_bw.pdf", width=9, height=4, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/structure.pdf", width=9, height=4, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

plot(0,0,type="n",xlab="",ylab="", bty="n", xlim=c(0,100), ylim=c(0,100),
     xaxt="n", yaxt="n")
text(40,90,"QTL",cex=1.8)
text(10,10,"Markers",cex=1.8)
text(70,10,"Phenotype",cex=1.8)
text(90,90,"Covariates",cex=1.8)
arrows(c(35,15),c(75,25),c(15,35),c(25,75),lwd=2, len=0.1, lend=1, ljoin=1)
arrows(25,10,55,10,lwd=2, len=0.1, lend=1, ljoin=1)
segments(25,10,53,10,lwd=2, col=bgcolor, lend=1, ljoin=1)
arrows(25,10,55,10,lwd=2,lty=2, len=0.1, lend=1, ljoin=1)
arrows(45,75,65,25,lwd=2, len=0.1, lend=1, ljoin=1)
arrows(85,75,75,25,lwd=2, len=0.1, lend=1, ljoin=1)


dev.off()
