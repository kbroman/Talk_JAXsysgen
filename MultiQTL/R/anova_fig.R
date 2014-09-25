# D4Mit214
# D12Mit20


if(bw) source("colors_bw.R") else source("colors.R")

data(hyper)

if(bw) {
  pdf(file="../Figs/anova_bw.pdf", width=5, height=5.2, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  pdf(file="../Figs/anova.pdf", width=5, height=5.2, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mfrow=c(1,2))
par(mar=c(5.1, 4.1, 1.1, 0.1))

y <- hyper$pheno[,1]
x <- pull.geno(fill.geno(subset(hyper, chr=c(4,12))))[,c("D4Mit214", "D12Mit20")]
z <- runif(length(y), -0.2, 0.2)
par(las=1)
plot(x[,1]+z, y, ylab="blood pressure",
     xlab="Genotype at D4Mit214", xaxt="n")
axis(side=1, at=1:2, labels=c("BB","AB"))
me <- tapply(y, x[,1], mean)
if(bw) {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[1])
} else {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[2])
}


par(mar=c(5.1,2.1,1.1,2.1))
plot((x[,2]+z), y, ylab="", yaxt="n",
     xlab="Genotype at D12Mit20", xaxt="n")
axis(side=1, at=1:2, labels=c("BB","AB"))

me <- tapply(y, x[,2], mean)
if(bw) {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[1])
} else {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[2])
}


dev.off()

