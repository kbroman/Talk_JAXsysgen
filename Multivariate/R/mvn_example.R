# illustration of multivariate QTL analysis

library(qtlbook)
data(iron)
iron <- iron["-X",]
iron <- calc.genoprob(iron, step=1, err=0.002)
iron$pheno[,1:2] <- log2(iron$pheno[,1:2])

out <- scanone(iron, phe=1:2, method="hk")

library(qtlpvl)
out.mvn <- scanone.mvn(iron, as.matrix(iron$pheno[,1:2]))

out <- cbind(out, out.mvn)
colnames(out)[5] <- "multivar"

# scatterplot
source("colors.R")
pdf("../Figs/iron_scatter.pdf", height=6.5, width=6.5, pointsize=14)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, las=1, mar=c(5.1, 4.1, 0.1, 0.1))
plot(iron$pheno[,1], iron$pheno[,2],
     xlab="liver", ylab="spleen",
     pch=21, bg=color[(iron$pheno$sex=="f")+1],
     col=color[(iron$pheno$sex=="f")+1])
legend("topleft", c("female", "male"),
       pch=21, pt.bg=color[2:1], col=color[2:1])
dev.off()

# LOD curves
pdf("../Figs/iron_lod.pdf", height=6.5, width=10.5, pointsize=14)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, mar=c(5.1, 4.1, 0.1, 0.1))
plot(out, lod=1:2, col=color[1:2],
     ylim=c(0, max(out[,5])), ylab="LOD score")
legend("topleft", c("liver", "spleen"), col=color[1:2], lty=1)
dev.off()

# LOD curve with multivariate
pdf("../Figs/iron_lod_mvn.pdf", height=6.5, width=10.5, pointsize=14)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, mar=c(5.1, 4.1, 0.1, 0.1))
plot(out, lod=1:3, col=c(color[1:2], "white"), lty=c(1,1,2),
     ylim=c(0, max(out[,5])), ylab="LOD score")
legend("topleft", c("liver", "spleen", "multivariate"), col=c(color[1:2], "white"), lty=1)
dev.off()
