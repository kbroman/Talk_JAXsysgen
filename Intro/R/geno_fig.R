if(bw) source("colors_bw.R") else source("colors.R")

library(qtlbook)
data(iron)

#png(file="../Figs/genodata.png", width=1125, height=750, res=108,
#       pointsize=14)
#png(file="../Figs/genodata.png", width=1125*2, height=750*2, res=108*2,
#       pointsize=14)
if(bw) {
  pdf(file="../Figs/genodata_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else {
  pdf(file="../Figs/genodata.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mar=c(5.1, 5.1, 2.1, 1.1), las=1)
g <- pull.geno(iron)
g[is.na(g)] <- 0
n <- ncol(g)+(-1:0)
sex <- iron$pheno$sex
pgm <- iron$pheno$pgm
temp <- g[sex=="f" & pgm==1,n]
temp[temp==1] <- 3
g[sex=="f" & pgm==1,n] <- temp
temp <- g[sex=="m",n]
temp[temp==2] <- 3
g[sex=="m",n] <- temp
me <- apply(iron$pheno[,1:2], 1, mean)
image(1:totmar(iron), 1:nind(iron), t(g)[,order(me)], col=c(bgcolor, color[c(1,4,2)]),
      xlab="Markers", ylab="Individuals")
nm <- nmar(iron)
abline(v=cumsum(nm)+0.5, col="white")
u <- par("usr")
abline(h=u[3:4], v=u[1:2], col="white")
pos <- (cumsum(nm) + cumsum(c(0,nm[-length(nm)])))/2+0.5
axis(side=3, at=pos, labels=rep("", length(pos)))
text(pos, u[4]+diff(u[3:4])*0.04, names(iron$geno), xpd=TRUE)
dev.off()

