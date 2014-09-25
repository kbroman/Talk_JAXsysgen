
if(bw) source("colors_bw.R") else source("colors.R")

##############################
# base model
##############################
if(bw) {
  pdf(file="../Figs/hyper_models1_bw.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/hyper_models1.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

text(110, 60, "LOD = 23.1", adj=c(0, 0.5), col=color[1])

dev.off()

##############################
# drop chr 1
##############################
if(bw) {
  pdf(file="../Figs/hyper_models2_bw.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/hyper_models2.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

text(110, 60, "LOD = 23.1", adj=c(0, 0.5), col=color[1])
text(53, 70, "6.3", col=color[2], adj=c(1,0.5))

dev.off()


##############################
# drop one at a time
##############################
if(bw) {
  pdf(file="../Figs/hyper_models3_bw.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/hyper_models3.pdf", width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

text(53, 70, "6.3", col=color[2], adj=c(1,0.5))
text(97, 70, "12.2", col=color[2], adj=c(0,0.5))
text(53, 50, "7.9", col=color[2], adj=c(1,0.5))
text(97, 50, "7.1", col=color[2], adj=c(0,0.5))
text(75, 45.3, "5.7", col=color[2], adj=c(0.5,0.5))

u <- par("usr")
text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
     adj=c(0,0), col=color[1])

dev.off()

##############################
# another interaction?
##############################
int1 <- c(1,1,1,2,2)
int2 <- c(2,3,4,3,4)
lod <- c("0.6","0.1","0.4","0.2","0.0")

for(i in 1:5) {

  if(bw) {
    pdf(file=paste("../Figs/hyper_models", i+3, "_bw.pdf", sep=""),
        width=9.75, height=6.5, pointsize=20, onefile=TRUE)
    par(fg="black",col="black",col.axis="black",col.lab="black",
        bg=bgcolor)
  } else {
    pdf(file=paste("../Figs/hyper_models", i+3, ".pdf", sep=""),
        width=9.75, height=6.5, pointsize=20, onefile=TRUE)
    par(fg="white",col="white",col.axis="white",col.lab="white",
        bg=bgcolor)
  }

  par(mar=rep(0.1,4))
  plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
       xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

  x <- c(60, 90, 60, 90)
  y <- c(70, 70, 50, 50)
  segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
  segments(x[int1[i]], y[int1[i]], x[int2[i]], y[int2[i]], lwd=2, ljoin=1, lend=1,
           col=color[1])
  points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
  points(x, y, pch=1, cex=4, lwd=2)
  text(x,y,c("1","4","6","15"))
  text(97, 60, lod[i], col=color[2], adj=c(0,0.5))


  u <- par("usr")
text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
     adj=c(0,0), col=color[1])
#  text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#       adj=c(0,0), col=color[1])

  dev.off()
}

##############################
# another QTL?
##############################
if(bw) {
  pdf(file=paste("../Figs/hyper_models", 9, "_bw.pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file=paste("../Figs/hyper_models", 9, ".pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
text(x,y,c("1","4","6","15"))

xx <- c(40,40,40)
yy <- c(80,60,40)
points(xx, yy, pch=1, cex=4, lwd=2, col=color[1])
text(xx, yy, c("1","2","5"), col=color[1])
text(xx-7, yy, c("1.87", "1.52","1.62"), col=color[2], adj=c(1,0.5))

u <- par("usr")
text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
     adj=c(0,0), col=color[1])
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=color[1])

dev.off()

##############################
# another interacting QTL?
##############################
if(bw) {
  pdf(file=paste("../Figs/hyper_models", 10, "_bw.pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file=paste("../Figs/hyper_models", 10, ".pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
xx <- 110; yy <- 40
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
segments(x[4], y[4], xx, yy, lwd=2, ljoin=1, lend=1, col=color[1])
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
points(xx, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx, yy, pch=1, cex=4, lwd=2, col=color[1])
text(x,y,c("1","4","6","15"))
text(xx,yy,"7", col=color[1])
text(xx,yy+6, "2.66", col=color[2], adj=c(0.5,0))

u <- par("usr")
text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
     adj=c(0,0), col=color[1])
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=color[1])

dev.off()

##############################
# another interacting QTL?
##############################
if(bw) {
  pdf(file=paste("../Figs/hyper_models", 11, "_bw.pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file=paste("../Figs/hyper_models", 11, ".pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
xx <- 40; yy <- 40
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
segments(x[3], y[3], xx, yy, lwd=2, ljoin=1, lend=1, col=color[1])
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
points(xx, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx, yy, pch=1, cex=4, lwd=2, col=color[1])
text(x,y,c("1","4","6","15"))
text(xx,yy,"4", col=color[1])
text(xx,yy+6, "2.82", col=color[2], adj=c(0.5,0))


u <- par("usr")
text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
     adj=c(0,0), col=color[1])
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=color[1])

dev.off()

######################################################################
# add another pair of QTL?
######################################################################
##############################
# another interacting QTL?
##############################
if(bw) {
  pdf(file=paste("../Figs/hyper_models", 12, "_bw.pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file=paste("../Figs/hyper_models", 12, ".pdf", sep=""),
      width=9.75, height=6.5, pointsize=20, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

par(mar=rep(0.1,4))
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
     xlim=c(0,150), ylim=c(0,100), xaxs="i", yaxs="i")

x <- c(60, 90, 60, 90)
y <- c(70, 70, 50, 50)
xx <- 45; yy <- 40; xxd <- 9
segments(x[3], y[3], x[4], y[4], lwd=2, ljoin=1, lend=1)
#segments(x[3], y[3], xx, yy, lwd=2, ljoin=1, lend=1, col=color[1])
points(x, y, pch=16, cex=4, lwd=2, col=bgcolor)
points(x, y, pch=1, cex=4, lwd=2)
points(xx, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx-xxd, yy, pch=16, cex=4, lwd=2, col=bgcolor)
points(xx, yy, pch=1, cex=4, lwd=2, col=color[1])
points(xx-xxd, yy, pch=1, cex=4, lwd=2, col=color[1])
text(x,y,c("1","4","6","15"))
text(xx,yy,"3b", col=color[1])
text(xx-xxd,yy,"3a", col=color[1])
text(xx-xxd/2,yy+6, "4.60", col=color[2], adj=c(0.5,0))


u <- par("usr")
text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19    ",
    T[m] + T[i]^H, " = 5.31    ", T[m] + T[i]^L, " = 3.88")),
     adj=c(0,0), col=color[1])
#text(0, 0, expression(paste(T[m], " = 2.69    ", T[i]^H, " = 2.62    ", T[i]^L, " = 1.19")),
#     adj=c(0,0), col=color[1])

dev.off()

