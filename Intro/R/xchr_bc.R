set.seed(122069)
if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf("../Figs/xchr_bc_bw.pdf", height=6.5, width=10,
      onefile=FALSE, pointsize=14)
  par(mar=rep(0.1,4),fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor, bty="n")
} else {
  pdf("../Figs/xchr_bc.pdf", height=6.5, width=10,
      onefile=FALSE, pointsize=14)
  par(mar=rep(0.1,4),fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor, bty="n")
}
plot(0,0,type="n",xlim=c(0,200),ylim=c(49,5),xaxt="n",yaxt="n",
     xlab="",ylab="",xaxs="i",yaxs="i")

######################################################################
# (AxB)xA
######################################################################
x <- 0; y <- 0
cl <- 7
clx <- 2.5
cw <- 1.5
cd <- 0.5
cdown <- 2.5
xc <- x+14; xd <- 15
yc <- y+10; yd <- 12
xlab <- x+25

text(xlab,y+6,"(A x B) x A",cex=1.3, col=color2[1])
segments(xc,yc,xc+xd,yc,lwd=2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"A",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"B",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)

  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)

  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[2], border=color[2], lend=1, ljoin=1)
}

segments(xc+xd/2,yc,xc+xd/2,yc+yd,lwd=2)

xc <- xc+xd/2
yc <- yc+yd

segments(xc,yc,xc+xd,yc,lwd=2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,expression(F[1]),cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"A",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)

  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)

  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}


ycp <- yc+yd
yd <- yd + 5

segments(xc+xd/2,yc,xc+xd/2,ycp,lwd=2)

xc <- xc
yc <- yc+yd

segments(xc,ycp,xc+xd,ycp,lwd=2)
segments(xc,ycp,xc,yc,lwd=2)
segments(xc+xd,ycp,xc+xd,yc,lwd=2)

points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"BC",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"BC",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
}


u <- runif(1,0.2,0.8)
u1 <- u; u2 <- 1
if(sample(1:2,1)==1) { u1 <- 0; u2 <- u }
if(bw) {
  rect(xc-cd-cw,yc+cdown+cl*u1,xc-cd,yc+cdown+cl*u2,
       lend=1, ljoin=1, lwd=2, angle=45, density=20)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)

  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  u <- runif(1,0.2,0.8)
  u1 <- u; u2 <- 1
  if(sample(1:2,1)==1) { u1 <- 0; u2 <- u }
  rect(xc+xd-cd-cw,yc+cdown+cl*u1,xc+xd-cd,yc+cdown+cl*u2,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown+cl*u1,xc-cd,yc+cdown+cl*u2,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)

  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  u <- runif(1,0.2,0.8)
  u1 <- u; u2 <- 1
  if(sample(1:2,1)==1) { u1 <- 0; u2 <- u }
  rect(xc+xd-cd-cw,yc+cdown+cl*u1,xc+xd-cd,yc+cdown+cl*u2,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}



######################################################################
# (BxA)xA
######################################################################
x <- 50; y <- 0
xc <- x+14
yc <- y+10;yd <- 12
xlab <- x + 25

text(xlab,y+6,"(B x A) x A",cex=1.3, col=color2[1])
segments(xc,yc,xc+xd,yc,lwd=2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"B",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"A",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}
  
segments(xc+xd/2,yc,xc+xd/2,yc+yd,lwd=2)

xc <- xc+xd/2
yc <- yc+yd

segments(xc,yc,xc+xd,yc,lwd=2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,expression(F[1]),cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"A",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}

ycp <- yc+yd
yd <- yd + 5

segments(xc+xd/2,yc,xc+xd/2,ycp,lwd=2)

xc <- xc
yc <- yc+yd

segments(xc,ycp,xc+xd,ycp,lwd=2)
segments(xc,ycp,xc,yc,lwd=2)
segments(xc+xd,ycp,xc+xd,yc,lwd=2)

points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"BC",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"BC",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  u <- runif(1,0.2,0.8)
  u1 <- u; u2 <- 1
  if(sample(1:2,1)==1) { u1 <- 0; u2 <- u }
  rect(xc-cd-cw,yc+cdown+cl*u1,xc-cd,yc+cdown+cl*u2,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  
  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  u <- runif(1,0.2,0.8)
  u1 <- u; u2 <- 1
  if(sample(1:2,1)==1) { u1 <- 0; u2 <- u }
  rect(xc+xd-cd-cw,yc+cdown+cl*u1,xc+xd-cd,yc+cdown+cl*u2,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
}  else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  u <- runif(1,0.2,0.8)
  u1 <- u; u2 <- 1
  if(sample(1:2,1)==1) { u1 <- 0; u2 <- u }
  rect(xc-cd-cw,yc+cdown+cl*u1,xc-cd,yc+cdown+cl*u2,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  
  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  u <- runif(1,0.2,0.8)
  u1 <- u; u2 <- 1
  if(sample(1:2,1)==1) { u1 <- 0; u2 <- u }
  rect(xc+xd-cd-cw,yc+cdown+cl*u1,xc+xd-cd,yc+cdown+cl*u2,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}
  




######################################################################
# Ax(AxB)
######################################################################
x <- 100; y <- 0
xc <- x+14 + xd/2
yc <- y+10;yd <- 12

text(25+100,y+6,"A x (A x B)",cex=1.3, col=color2[1])
#text(5,y+4,"c",cex=1.2,font=2)
segments(xc,yc,xc+xd,yc,lwd=2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"A",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"B",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[2], border=color[2], lend=1, ljoin=1)
}

segments(xc+xd/2,yc,xc+xd/2,yc+yd,lwd=2)

xc <- xc-xd/2
yc <- yc+yd

segments(xc,yc,xc+xd,yc,lwd=2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,expression(F[1]),cex=1.2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"A",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[2], border=color[2], lend=1, ljoin=1)
}
ycp <- yc+yd
yd <- yd + 5

segments(xc+xd/2,yc,xc+xd/2,ycp,lwd=2)

xc <- xc
yc <- yc+yd

segments(xc,ycp,xc+xd,ycp,lwd=2)
segments(xc,ycp,xc,yc,lwd=2)
segments(xc+xd,ycp,xc+xd,yc,lwd=2)

points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"BC",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"BC",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  
  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  
  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[2], border=color[2], lend=1, ljoin=1)
}


######################################################################
# Ax(BxA)
######################################################################
x <- 150; y <- 0
xc <- x+14 + xd/2
yc <- y+10;yd <- 12
xlab <- x+25

text(xlab,y+6,"A x (B x A)",cex=1.3, col=color2[1])
#text(55,y+4,"d",cex=1.2,font=2)
segments(xc,yc,xc+xd,yc,lwd=2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"B",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"A",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}

segments(xc+xd/2,yc,xc+xd/2,yc+yd,lwd=2)

xc <- xc-xd/2
yc <- yc+yd

segments(xc,yc,xc+xd,yc,lwd=2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,expression(F[1]),cex=1.2)
points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"A",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  
  rect(xc+xd-cd,yc+cdown,xc+xd-cd-cw,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}

ycp <- yc+yd
yd <- yd + 5

segments(xc+xd/2,yc,xc+xd/2,ycp,lwd=2)

xc <- xc
yc <- yc+yd

segments(xc,ycp,xc+xd,ycp,lwd=2)
segments(xc,ycp,xc,yc,lwd=2)
segments(xc+xd,ycp,xc+xd,yc,lwd=2)

points(xc,yc,pch=16,cex=4.5,lwd=2,col=bgcolor)
points(xc,yc,pch=1,cex=4.5,lwd=2)
text(xc,yc,"BC",cex=1.2)
points(xc+xd,yc,pch=15,cex=4,lwd=2,col=bgcolor)
points(xc+xd,yc,pch=0,cex=4,lwd=2)
text(xc+xd,yc,"BC",cex=1.2)

if(bw) {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       lend=1, ljoin=1, angle=45, density=20, lwd=2)
  
  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       lend=1, ljoin=1, lwd=2)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       lend=1, ljoin=1, lwd=2)
} else {
  rect(xc-cd-cw,yc+cdown,xc-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+cd,yc+cdown,xc+cd+cw,yc+cdown+cl,
       col=color[2], border=color[2], lend=1, ljoin=1)
  
  rect(xc+xd-cd-cw,yc+cdown,xc+xd-cd,yc+cdown+cl,
       col=color[1], border=color[1], lend=1, ljoin=1)
  rect(xc+xd+cd,yc+cdown,xc+xd+cd+cw,yc+cdown+clx,
       col=color[1], border=color[1], lend=1, ljoin=1)
}

dev.off()
