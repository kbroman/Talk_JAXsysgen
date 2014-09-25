if(bw) source("colors_bw.R") else source("colors.R")

file <- "sug.RData"
if(file.exists(file)) {
  load(file)
} else {
  sug <- read.cross("csv", "", "sug.csv", genotypes=c("CC","CB", "BB"), alleles=c("C","B"))
  save(sug, file=file)
}
  

lab1 <- "Body weight"
lab2 <- "Heart weight"
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
hist(sug$pheno[,3], breaks=25, main="", yaxt="n", ylab="", xlab=lab1)
hist(sug$pheno[,4], breaks=25, main="", yaxt="n", ylab="", xlab=lab2)

par(mar=c(5.1, 4.1, 1.1, 1.1), las=1)
plot(sug$pheno[,3:4], xlab=lab1, ylab=lab2,
     pch=21, bg=color[1])
rug(sug$pheno[,3], side=1, col=color[1], ticksize=0.02)
rug(sug$pheno[,4], side=2, col=color[1], ticksize=0.02)
u <- par("usr")
abline(h=u[3], v=u[1])

dev.off()

