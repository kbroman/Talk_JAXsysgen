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
iron <- sim.geno(subset(iron, chr=c(2,7,15,16)), n.draws=128, step=1, err=0.01)
out.imp <- scanone(iron, phe=1, method="imp")

lab1 <- "IM"
lab2 <- "HK"
lab3 <- "eHK"
lab4 <- "imp"

#png(file="../Figs/alod.png", width=1125, height=600, res=108,
#       pointsize=14)
if(!bw) {
  pdf(file="../Figs/imp_lod_comp.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
} else {
  pdf(file="../Figs/imp_lod_comp_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
}
par(mar=c(5.1, 5.1, 1.1, 1.1))

if(bw) {
  plot(out, out.hk, out.ehk, lodcolumn=1, col=c("black","black","gray"),
       ylab="LOD score", chr=c(2,7,15,16),
       lty=c(1,2,2))
  plot(out.imp, add=TRUE, chr=c(2,7,15,16), col="black", lty=3)
} else {
  plot(out, out.hk, out.ehk, lodcolumn=1, col=color[c(1,2,7)], ylab="LOD score", chr=c(2,7,15,16),
       lty=c(1,1,2))
  plot(out.imp, add=TRUE, chr=c(2,7,15,16), col=color[3], lty=1)
}

u <- par("usr")
if(bw) {
  legend(u[1], u[4], lwd=2, col=c("black","black","gray","black"), legend=c(lab1, lab2, lab3, lab4),
         xjust=0, yjust=1, y.intersp=1.5, lty=c(1,2,2,3))
} else {
  legend(u[1], u[4], lwd=2, col=color[c(1,2,7,3)], legend=c(lab1, lab2, lab3, lab4),
         xjust=0, yjust=1, y.intersp=1.5, lty=c(1,1,2,1))
}

dev.off()

