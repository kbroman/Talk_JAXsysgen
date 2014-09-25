if(bw) source("colors_bw.R") else source("colors.R")

file <- "Rcache/2dscan.RData"
if(file.exists(file)) {
  load(file)
} else {
  data(hyper)
  hyper <- calc.genoprob(hyper, step=2.5, err=0.01)
  out2 <- scantwo(hyper, method="em")
  hyper <- calc.genoprob(hyper, step=10, err=0.01)
  out2coarse <- scantwo(hyper, method="em")
  save(out2, out2coarse, file=file, compress=TRUE)
}

if(bw) {
  pdf(file="../Figs/2dscan_allchr_bw.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/2dscan_allchr.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

if(bw) {
  plot(out2coarse, col.scheme="gray")
} else {
  plot(out2coarse)
}

dev.off()

if(bw) {
  pdf(file="../Figs/2dscan_selchr_bw.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/2dscan_selchr.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

if(bw) {
  plot(out2, col.scheme="gray", chr=c(1,4,6,7,15))
} else {
  plot(out2, chr=c(1,4,6,7,15))
}

dev.off()

if(bw) {
  pdf(file="../Figs/2dscan_selchr_fv1_bw.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/2dscan_selchr_fv1.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

if(bw) {
  plot(out2, col.scheme="gray", chr=c(1,4,6,7,15), lower="cond-int")
} else {
  plot(out2, chr=c(1,4,6,7,15), lower="cond-int")
}

dev.off()

