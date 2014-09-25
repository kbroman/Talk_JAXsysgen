if(bw) source("colors_bw.R") else source("colors.R")

file <- "Rcache/aperm.RData"
if(file.exists(file)) {
  load(file)
} else {
  load("Rcache/alod.RData")
  operm <- scanone(iron, phe=1, chr=1:19, method="hk", n.perm=10000)
  save(operm, file=file, compress=TRUE)
}

if(bw) {
  pdf(file="../Figs/perm_hist_bw.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/perm_hist.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
par(mar=c(4.1, 1.1, 1.1, 1.1))

temp <- hist(operm, breaks=200, main="", yaxt="n", ylab="",
               xlab="", border=bgcolor, axes=FALSE, xlim=c(0,max(operm)), xaxs="i")
axis(line=-1, side=1)
title(xlab="Genome-wide maximum LOD score", line=2)
x <- rep(temp$breaks, rep(2, length(temp$breaks)))
#x <- x[-c(1, length(x))]
y <- c(0,rep(temp$counts, rep(2, length(temp$counts))),0)
lines(x,y)#, col=color[1])

#abline(h=0)

dev.off()

