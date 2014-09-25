if(bw) source("colors_bw.R") else source("colors.R")

data(hyper)

#png(file="../Figs/genodata.png", width=1125, height=750, res=108,
#       pointsize=14)
#png(file="../Figs/genodata.png", width=1125*2, height=750*2, res=108*2,
#       pointsize=14)
if(bw) {
  pdf(file="../Figs/pheno_bw.pdf", width=9, height=4, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  pdf(file="../Figs/pheno.pdf", width=9, height=4, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mar=c(5.1,1.1,1.1,1.1))

hist(hyper$pheno[,1], breaks=seq(82, 128.5, by=1.5),
     main="", yaxt="n", ylab="", xlab="Blood pressure")

dev.off()
