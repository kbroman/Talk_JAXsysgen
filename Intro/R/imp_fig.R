if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf(file="../Figs/imp_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/imp.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}
set.seed(12201969+15) 

data(hyper)
hyper <- subset(hyper, chr=12, ind=1:10)
m <- c(0,16,22,40,56)
names(m) <- names(hyper$geno[[1]]$map)
hyper$geno[[1]]$map <- m
marpos <- round(pull.map(hyper)[[1]])

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(6,100), ylim=c(3.5,103.5), xaxs="i", yaxs="i")
xl <- c(30,97)
yp <- 93
yd <- 1
segments(xl[1], yp, xl[2], yp)

L <- diff(range(marpos))
otherpos <- seq(0, L, by=2)
otherpos <- otherpos[is.na(match(otherpos, marpos))]

xotherpos <- otherpos * diff(xl)/L + xl[1]
xmarpos <- marpos * diff(xl)/L + xl[1]
segments(xmarpos, yp-yd*2, xmarpos, yp+yd*2)
segments(xotherpos, yp-yd, xotherpos, yp+yd)
text(xmarpos, yp+yd*6, marpos)

xd <- 4
text(xl[1]-xd, yp, "Genetic map:", adj=c(1,0.5))

yp2 <- yp-yd*8
ind <- 9 # or 13, 14, 57, 58
nmar <- length(marpos)
nother <- length(otherpos)
points(xmarpos, rep(yp2,nmar), pch=15, cex=1.5,
       col=color[hyper$geno[[1]]$data[ind,]])
points(xmarpos, rep(yp2,nmar), pch=0, cex=1.5)
points(xotherpos, rep(yp2, nother), pch=0, cex=1.5)

text(xl[1]-xd, yp2, "Observed data:", adj=c(1,0.5))

hyper <- sim.geno(hyper, n.draws=64, step=2)
hyper$geno[[1]]$draws <- hyper$geno[[1]]$draws[,,-(11:22)] 
xallpos <- sort(c(xmarpos, xotherpos))
npos <- length(xallpos)
yp3 <- yp2-yd*8
text(xl[1]-xd, yp3, "Imputations:", adj=c(1,0.5))
i <- 1
while(yp3 > 0) {
  points(xallpos, rep(yp3,npos), pch=15, cex=1.5,
         col=color[hyper$geno[[1]]$draws[ind,,i]])
  points(xallpos, rep(yp3,npos), pch=0, cex=1.5)
  yp3 <- yp3 - yd*5
  i <- i+1
}

yp4 <- yp2 - yd*6 - yd*3*5
xp2 <- 12
yd2 <- yd*6
points(xp2, yp4, pch=15, cex=1.5, col=color[1])
points(xp2, yp4, pch=0, cex=1.5)
text(xp2+xd*0.5, yp4, "= AA", adj=c(0,0.5))

points(xp2, yp4-yd2, pch=15, cex=1.5, col=color[2])
points(xp2, yp4-yd2, pch=0, cex=1.5)
text(xp2+xd*0.5, yp4-yd2, "= AB", adj=c(0,0.5))

points(xp2, yp4-2*yd2, pch=0, cex=1.5)
text(xp2+xd*0.5, yp4-2*yd2, "= missing", adj=c(0,0.5))

dev.off()

# end of imp_illustration.R
