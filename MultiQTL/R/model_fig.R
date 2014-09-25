if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf(file="../Figs/models_bw.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/models.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(10,110), ylim=c(-3,97), xaxs="i", yaxs="i")

x <- c(25,40)
y <- c(75,85)
points(x, y, pch=16)
segments(x[1], y[1], x[2], y[2], lwd=2, ljoin=1, lend=1)
text(45, 90, "A", font=2, col=color[1])

x <- c(20, 65/2, 38)
y <- c(15, 35, 10)
points(x, y, pch=16)
#segments(x[1], y[1], x[2], y[2], lwd=2, ljoin=1, lend=1)
segments(x[1], y[1], x[3], y[3], lwd=2, ljoin=1, lend=1)
text(45, 40, "B", font=2, col=color[1])

x <- c(20, 65/2, 38)+50
y <- c(15, 35, 10)+50
points(x, y, pch=16)
segments(x[1], y[1], x[2], y[2], lwd=2, ljoin=1, lend=1)
segments(x[1], y[1], x[3], y[3], lwd=2, ljoin=1, lend=1)
segments(x[2], y[2], x[3], y[3], lwd=2, ljoin=1, lend=1)
text(95, 90, "C", font=2, col=color[1])

x <- c(80, 70, 66, 71, 87, 93, 91)-4
y <- c(20, 5, 23, 36, 5, 17, 33)
points(x, y, pch=16)
for(i in 2:length(x))
  segments(x[1], y[1], x[i], y[i], lwd=2, ljoin=1, lend=1)
text(95, 40, "D", font=2, col=color[1])

dev.off()

