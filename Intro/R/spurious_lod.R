if(bw) source("colors_bw.R") else source("colors.R")
file <- "Rcache/spuriouslod.RData"
load(file)

if(bw) {
  pdf(file="../Figs/spurious_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/spurious.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mfrow=c(2,1), mar=c(5.1, 1.1, 0.1, 0.1))
hist(listeria$pheno[,1], breaks=30, xlab="Phenotype", yaxt="n", ylab="", main="")
par(mar=c(4.1,4.1,2.1,0.1))
if(bw) {
  plot(outl.em, ylab="LOD score", col="black")
} else {
  plot(outl.em, ylab="LOD score", col="white")
}
dev.off()

