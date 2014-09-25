if(bw) source("colors_bw.R") else source("colors.R")

attach("Rcache/out2_coarse2.RData")
attach("Rcache/out2_fine.RData")
attach("Rcache/out2_fine_c1.RData")

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
  plot(out2.c, col.scheme="gray")
} else {
  plot(out2.c)
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

if(bw) {
  pdf(file="../Figs/2dscan_chr1_bw.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/2dscan_chr1.pdf", width=7.1, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

if(bw) {
  plot(out2.c1, col.scheme="gray", lower="cond-add", zlim=c(1.65, 1.65))
} else {
  plot(out2.c1, lower="cond-add", zlim=c(1.65, 1.65))
}

dev.off()

for(i in 1:3) detach(2)


