set.seed(4131972)
if(bw) source("colors_bw.R") else source("colors.R")
if(bw) {
  pdf(file="../Figs/covar_bw.pdf", width=5, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black", col.main="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/covar.pdf", width=5, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
      bg=bgcolor)
}

library(qtlbook)
data(myocard)
myocard <- calc.genoprob(subset(myocard, chr=6), step=1)

sex <- myocard$pheno$sex
male <- scanone(subset(myocard, ind=sex==1))
female <- scanone(subset(myocard, ind=sex==0))

par(mfrow=c(2,1), mar=c(5.1,4.1,4.1,0.1))
if(bw) {
  plot(male, female, col="black", lty=1:2, ylab="LOD score")
  u <- par("usr")
  legend(u[1], u[4], lwd=2, col="black", legend=c("male","female"),
         xjust=0, yjust=1, y.intersp=1.5, lty=1:2)

} else {
  plot(male, female, col=color[1:2], lty=1, ylab="LOD score")
  u <- par("usr")
  legend(u[1], u[4], lwd=2, col=color[1:2], legend=c("male","female"),
         xjust=0, yjust=1, y.intersp=1.5, lty=1)
}
title(main="Chromosome 6")


par(las=1)
g <- pull.geno(fill.geno(myocard))[,"D6Mit373"]
x <- g + (1-sex)*3.5
y <- myocard$pheno[,1]
z <- runif(length(y), -0.25, 0.25)
me <- tapply(y, x, mean)
plot(x+z, y, xlab="", ylab="% myocarditis", xaxt="n", cex=0.8)
abline(v=unique(x), col="gray", lty=2)
axis(side=1, at=sort(unique(x)), labels=rep(c("AA","AB","BB"), 2))
axis(side=1, at=c(2, 5.5), labels=c("Males", "Females"), line=2, tick=FALSE, cex.axis=1.3)
x <- as.numeric(names(me))
if(bw) {
  segments(x-0.3, me, x+0.3, me, lwd=2)
} else {
  segments(x-0.3, me, x+0.3, me, lwd=2, col=color[2])
}
title(main="D6Mit373")


dev.off()

