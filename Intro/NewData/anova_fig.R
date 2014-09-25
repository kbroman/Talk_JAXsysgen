
if(bw) source("colors_bw.R") else source("colors.R")

file <- "sug.RData"
if(file.exists(file)) {
  load(file)
} else {
  sug <- read.cross("csv", "", "sug.csv", genotypes=c("CC","CB", "BB"), alleles=c("C","B"))
  save(sug, file=file)
}

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

y <- sug$pheno[,3]
x <- pull.geno(fill.geno(subset(sug, chr=c(15,6))))[,c("D15MIT184", "D6MIT259")]
z <- runif(length(y), -0.2, 0.2)
par(las=1)
plot(x[,1]+z, y, ylab="Body weight",
     xlab="Genotype at D15Mit184", xaxt="n")
axis(side=1, at=1:3, labels=c("CC","CB","BB"))
me <- tapply(y, x[,1], mean, na.rm=TRUE)
if(bw) {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[1])
} else {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[2])
}


par(mar=c(5.1,2.1,1.1,2.1))
plot((x[,1]+z), y, ylab="", yaxt="n",
     xlab="Genotype at D6Mit259", xaxt="n")
axis(side=1, at=1:3, labels=c("CC","CB","BB"))

me <- tapply(y, x[,2], mean, na.rm=TRUE)
if(bw) {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[1])
} else {
  segments(1:3-0.25, me, 1:3+0.25, me, lwd=3, col=color[2])
}


dev.off()

