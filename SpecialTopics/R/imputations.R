data(hyper)

source("colors.R")

png("../Figs/multiimp.png", height=900, width=1350, pointsize=24)
#pdf("../Figs/multiimp.pdf", height=6, width=9, pointsize=18)
par(las=1, bg=bgcolorpng, fg="white", col="white", col.axis=color[1], col.lab=color[1], col.main=color2[1],
    mar=c(5.1, 3.1, 4.2, 1.1), mfrow=c(2,3))
geno.image(hyper, main="", alternate.chrid=TRUE, reorder=1)
mtext(side=3, "Original data", col=color2[1], line=2.5)
for(i in 1:5) {
  hyperf <- fill.geno(hyper)
  geno.image(hyperf, main="", alternate.chrid=TRUE, reorder=1)
  mtext(side=3, paste("Imputation", i), col=color2[1], line=2.5)
}
dev.off()
