if(bw) source("colors_bw.R") else source("colors.R")

file <- "Rcache/alod_c4.RData"
if(file.exists(file)) {
  load(file)
} else {
  data(hyper)
  g <- fill.geno(hyper)$geno[[4]]$data[,"D4Mit164"]
  hyper <- calc.genoprob(hyper, step=1, err=0.01)

  out.c4 <- scanone(hyper, phe=1, addcovar=g)
  save(out.c4, g, file=file, compress=TRUE)
}

load("Rcache/alod.RData")

if(bw) {
  pdf(file="../Figs/alod_c4_bw.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/alod_c4.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 2.6))

if(bw) {
  plot(out, out.c4, col="black", ylab="LOD score", chr=1:19, lty=2:1)
  legend("topright", lwd=2, lty=2:1, c("Interval mapping","Control for chr 4"))
} else {
  plot(out, out.c4, col=color[1:2], ylab="LOD score", chr=1:19)
  legend("topright", lwd=2, col=color[1:2], c("Interval mapping","Control for chr 4"))
}

dev.off()



