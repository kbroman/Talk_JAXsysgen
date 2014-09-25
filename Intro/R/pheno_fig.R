if(bw) source("colors_bw.R") else source("colors.R")

library(qtlbook)
data(iron)
for(i in 1:2) iron$pheno[,i] <- log2(iron$pheno[,i])

lab1 <- expression(paste(log[2], " liver"))
lab2 <- expression(paste(log[2], " spleen"))
#png(file="../Figs/pheno.png", width=1125, height=600, res=108,
#       pointsize=14)
if(bw) {
  pdf(file="../Figs/pheno_bw.pdf", width=9.75, height=5.2, pointsize=12, onefile=TRUE)
} else {
  pdf(file="../Figs/pheno.pdf", width=9.75, height=5.2, pointsize=12, onefile=TRUE)
}
layout(cbind(c(1,2),c(3,3)))
if(bw) {
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mar=c(5.1, 1.1, 1.1, 1.1))
hist(iron$pheno[,1], breaks=34, main="", yaxt="n", ylab="", xlab=lab1)
hist(iron$pheno[,2], breaks=34, main="", yaxt="n", ylab="", xlab=lab2)

par(mar=c(5.1, 4.1, 1.1, 1.1), las=1)
sex <- as.numeric(iron$pheno$sex)
plot(iron$pheno[,1:2], xlab=lab1, ylab=lab2,
     pch=c(1,4)[sex], col=color[2:1][sex], lwd=2)
me <- apply(iron$pheno[,1:2], 2, tapply, iron$pheno$sex, mean)
points(me[,1], me[,2], col=color[7], lwd=3, pch=c(1,4), cex=1.5)
rug(iron$pheno[sex==1,1], side=1, col=color[2], ticksize=0.02)
rug(iron$pheno[sex==2,1], side=1, col=color[1], ticksize=0.02)
rug(iron$pheno[sex==1,2], side=2, col=color[2], ticksize=0.02)
rug(iron$pheno[sex==2,2], side=2, col=color[1], ticksize=0.02)
u <- par("usr")
abline(h=u[3], v=u[1])

points(c(4.6,4.6), c(10, 10.2), lwd=2, pch=c(4,1), col=color[1:2])
text(c(4.7, 4.7), c(10, 10.2), c("Male", "Female"), adj=c(0, 0.5))
rect(4.5, 9.9, 5.2, 10.3)

dev.off()

