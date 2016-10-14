if(bw) source("colors_bw.R") else source("colors.R")

load("Rcache/alod.RData")
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
out.hk <- scanone(iron, phe=1, method="hk")
save(out, iron, out.hk, file=file, compress=TRUE)

lab1 <- "IM"
lab2 <- "HK"
if(bw) {
  pdf(file="../Figs/hk_lod_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/hk_lod.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 1.1))

if(bw) {
  plot(out, out.hk, lodcolumn=1, col="black", lty=1:2, ylab="LOD score", chr=c(2,7,15,16))
  u <- par("usr")
  legend(u[1], u[4], lwd=2, col="black", lty=1:2, legend=c(lab1, lab2),
         xjust=0, yjust=1, y.intersp=1.5)
} else {
  plot(out, out.hk, lodcolumn=1, col=color[1:2], lty=1, ylab="LOD score", chr=c(2,7,15,16))
u <- par("usr")
legend(u[1], u[4], lwd=2, col=color[1:2], lty=1, legend=c(lab1, lab2),
       xjust=0, yjust=1, y.intersp=1.5)
}

dev.off()

