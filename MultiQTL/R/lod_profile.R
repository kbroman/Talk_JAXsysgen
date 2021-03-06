if(bw) source("colors_bw.R") else source("colors.R")

file <- "Rcache/lod_profile.RData"
if(file.exists(file)) {
  load(file)
} else {
  rm(list=ls())
  data(hyper)
  hyper <- subset(hyper, chr=c(1,4,6,15))
  hyper <- sim.geno(hyper, err=0.001, n.draws=512, step=1)
  qtl <- makeqtl(hyper, chr=c("1","4","6","15"), pos=c(48,30,24,20))
  qtl <- refineqtl(hyper, qtl=qtl, formula=y~q1+q2+q3*q4, incl.markers=TRUE)

  # kluge to fix slight differences from other plots
  lp <- attr(qtl, "lodprofile")
  lp[[1]][,3] <- lp[[1]][,3]-max(lp[[1]][,3])+6.3
  lp[[2]][,3] <- lp[[2]][,3]-max(lp[[2]][,3])+12.2
  lp[[3]][,3] <- lp[[3]][,3]-max(lp[[3]][,3])+7.9
  lp[[4]][,3] <- lp[[4]][,3]-max(lp[[4]][,3])+7.1
  attr(qtl, "lodprofile") <- lp

  out <- scanone(hyper, method="imp")

  save(qtl, out, file=file)
}

if(bw) {
  pdf(file="../Figs/lod_profile_bw.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/lod_profile.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 2.6))

if(bw) {
  plot(out, lty=2, ylim=c(0, 13), ylab="Profile LOD score")
  plotLodProfile(qtl, col="black", add=TRUE)
} else {
  plot(out, col=color[2], ylim=c(0, 13), ylab="Profile LOD score")
  plotLodProfile(qtl, col=color[1], add=TRUE)
}

dev.off()



