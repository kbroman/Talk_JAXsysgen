if(bw) source("colors_bw.R") else source("colors.R")

file <- "Rcache/alod.RData"
if(file.exists(file)) {
  load(file)
} else {
  library(qtlbook)
  data(iron)
  for(i in 1:2) iron$pheno[,i] <- log2(iron$pheno[,i])
  iron <- calc.genoprob(iron, step=1, err=0.01)

  out <- scanone(iron, phe=1:2)
  save(out, iron, file=file, compress=TRUE)
}

lab1 <- expression(paste(log[2], " liver"))
lab2 <- expression(paste(log[2], " spleen"))
#png(file="../Figs/alod.png", width=1125, height=600, res=108,
#       pointsize=14)
if(bw) {
  pdf(file="../Figs/alod_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/alod.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 1.1))

if(bw) {
  plot(out, lodcolumn=1:2, col="black", ylab="LOD score", chr=1:19, lty=1:2)
} else {
  plot(out, lodcolumn=1:2, col=color[1:2], ylab="LOD score", chr=1:19)
}

u <- par("usr")
if(bw) {
  legend(u[1], u[4], lwd=2, lty=1:2, legend=c(lab1, lab2),
         xjust=0, yjust=1, y.intersp=1.5)
} else {
  legend(u[1], u[4], lwd=2, col=color[1:2], legend=c(lab1, lab2),
         xjust=0, yjust=1, y.intersp=1.5)
}
dev.off()

