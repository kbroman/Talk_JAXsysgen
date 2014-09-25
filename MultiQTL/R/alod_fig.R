if(bw) source("colors_bw.R") else source("colors.R")

file <- "Rcache/alod.RData"
if(file.exists(file)) {
  load(file)
} else {
  data(hyper)
  hyper <- calc.genoprob(hyper, step=1, err=0.01)

  out <- scanone(hyper, phe=1)
  save(out, hyper, file=file, compress=TRUE)
}

#png(file="../Figs/alod.png", width=1125, height=600, res=108,
#       pointsize=14)
if(bw) {
  pdf(file="../Figs/alod_bw.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/alod.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 2.6))

if(bw) {
  plot(out, col="black", ylab="LOD score", chr=1:19, lty=1)
} else {
  plot(out, col=color[1], ylab="LOD score", chr=1:19)
}

dev.off()


for(i in 0:9) {
  file <- paste("Rcache/perm1_0", i, ".RData", sep="")
  if(file.exists(file)) {
    attach(file)
    if(i==0) op <- operm
    else op <- c(op, operm)
    detach(2)
  }
}


if(bw) {
  pdf(file="../Figs/alod2_bw.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/alod2.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 2.6))

qu <- quantile(op, 0.95)
if(bw) {
  plot(out, col="black", ylab="LOD score", chr=1:19, lty=1)
  abline(h=qu, lty=2)
  axis(side=4, at=qu, label="5%", las=1, tick=FALSE, line=-0.5)
} else {
  plot(out, col=color[1], ylab="LOD score", chr=1:19)
  abline(h=qu, col=color[2], lty=2)
  axis(side=4, at=qu, label="5%", las=1, col.axis=color[2], col=color[2], tick=FALSE, line=-0.5)
}
dev.off()


if(bw) {
  pdf(file="../Figs/alod3_bw.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/alod3.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 2.6))

qu <- quantile(op, c(0.99, 0.95, 0.37))
if(bw) {
  plot(out, col="black", ylab="LOD score", chr=1:19, lty=1)
  abline(h=qu, lty=c(4,2,3))
  axis(side=4, at=qu, label=c("1%","5%","63%"), las=1, tick=FALSE, line=-0.5)
} else {
  plot(out, col=color[1], ylab="LOD score", chr=1:19)
  abline(h=qu, col=color[2], lty=c(4,2,3))
  axis(side=4, at=qu, label=c("1%","5%","63%"), las=1, col.axis=color[2], col=color[2], tick=FALSE, line=-0.5)
}
dev.off()

