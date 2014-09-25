source("colors.R")

file <- "Rcache/alod.RData"
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

sex <- as.numeric(iron$pheno$sex)
pgm <- iron$pheno$pgm

X <- cbind(as.numeric(sex==2), as.numeric(sex==1 & pgm==1), as.numeric(sex==1 & pgm==0))
y1 <- iron$pheno[,1]
y2 <- iron$pheno[,2]
n <- nind(iron)

lod1 <- n/2*log10(sum(lm(y1~1)$resid^2) / sum(lm(y1 ~ -1 + X)$resid^2))
lod2 <- n/2*log10(sum(lm(y2~1)$resid^2) / sum(lm(y2~-1+X)$resid^2))


badout <- out
badout[badout[,1]=="X",3] <- badout[badout[,1]=="X",3] + lod1
badout[badout[,1]=="X",4] <- badout[badout[,1]=="X",4] + lod2


lab1 <- expression(paste(log[2], " liver"))
lab2 <- expression(paste(log[2], " spleen"))

pdf(file="../Figs/xlod1.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor)
par(mar=c(5.1, 5.1, 1.1, 1.1))

plot(out, lodcolumn=1:2, col=color[1:2], ylab="LOD score", chr=c(2,7,8,9,16,"X"), ylim=c(0,16.26))

u <- par("usr")
legend(u[1], u[4], lwd=2, col=color[1:2], legend=c(lab1, lab2),
       xjust=0, yjust=1, y.intersp=1.5)

dev.off()


pdf(file="../Figs/xlod2.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor)
par(mar=c(5.1, 5.1, 1.1, 1.1))

plot(badout, lodcolumn=1:2, col=color[1:2], ylab="LOD score", chr=c(2,7,8,9,16,"X"), ylim=c(0,16.26))

u <- par("usr")
legend(u[1], u[4], lwd=2, col=color[1:2], legend=c(lab1, lab2),
       xjust=0, yjust=1, y.intersp=1.5)

dev.off()

