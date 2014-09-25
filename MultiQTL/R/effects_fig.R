
#data(hyper)
#hyper.sub <- sim.geno(subset(hyper, chr=c(1,4,6,15)), step=1,
#                      n.draws=1024, err=0.001)
#save(hyper.sub, file="Rcache/hyperimp_c14615.RData", compress=TRUE)
attach("Rcache/hyperimp_c14615.RData")
attr(hyper.sub, "alleles") <- c("B","A")
save(hyper.sub, file="Rcache/hyperimp_c14615.RData", compress=TRUE)
source("myeffectplot.R")

#attr(hyper, "alleles") <- c("B","A")


# chr  1: 48.3  (c1.loc45)
# chr  4: D4Mit164
# chr  6: 24.0  (c6.loc24)
# chr 15: 19.5  (c15.loc14)

for(bw in 0:1) {

if(bw) source("colors_bw.R") else source("colors.R")
if(bw) {
  pdf(file="../Figs/meffects_bw.pdf", width=9.75, height=6.5, pointsize=16, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",col.main="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  pdf(file="../Figs/meffects.pdf", width=9.75, height=6.5, pointsize=16, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mfrow=c(1,4), mar=c(5.1,4.1,3.1,0.1), las=1)

effectplot(hyper.sub, mname1="c1.loc45", ylim=c(97, 106.5),
           main="Chr 1 @ 48 cM", ylab="blood pressure",
           xlab="Genotype", col=color[1])
effectplot(hyper.sub, mname1="D4Mit164", ylim=c(97, 106.5),
           main="Chr 4 @ 30 cM", ylab="",
           xlab="Genotype", col=color[1])
effectplot(hyper.sub, mname1="c6.loc24", ylim=c(97, 106.5),
           main="Chr 6 @ 24 cM", ylab="",
           xlab="Genotype", col=color[1])
effectplot(hyper.sub, mname1="c15.loc14", ylim=c(97, 106.5),
           main="Chr 15 @ 20 cM", ylab="",
           xlab="Genotype", col=color[1])
dev.off()



#1 @ 68.3 ["c1.loc65"]; 4 @ 30 [D4Mit164]
#6 @ 60 [c6.loc60]; 15 @ 18 [D15Mit152]


if(bw) {
  pdf(file="../Figs/ieffects_bw.pdf", width=9.75, height=6.5, pointsize=16, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",col.main="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  pdf(file="../Figs/ieffects.pdf", width=9.75, height=6.5, pointsize=16, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mfrow=c(1,2), mar=c(5.1,4.1,3.1,0.1), las=1)

if(!bw) {
  myeffectplot(hyper.sub, mname1="c1.loc65", mname2="D4Mit164", ylim=c(94,111),
               main="1 x 4", ylab="blood pressure",
               xlab="Chr 4 genotype", legend.lab="Chr 1 genotype",
               col=color[1:2])
  myeffectplot(hyper.sub, mname1="c6.loc60", mname2="D15Mit152", ylim=c(94,111),
               main="6 x 15", ylab="",
               legend.lab="Chr 6 genotype",
               xlab="Chr 15 Genotype", col=color[1:2])
} else {
  myeffectplot(hyper.sub, mname1="c1.loc65", mname2="D4Mit164", ylim=c(94,111),
               main="1 x 4", ylab="blood pressure",
               xlab="Chr 4 genotype", legend.lab="Chr 1 genotype",
               col=rep("black", 2), lty=1:2)
  myeffectplot(hyper.sub, mname1="c6.loc60", mname2="D15Mit152", ylim=c(94,111),
               main="6 x 15", ylab="",
               legend.lab="Chr 6 genotype",
               xlab="Chr 15 Genotype", col=rep("black", 2), lty=1:2)
}


dev.off()
}

detach(2)
