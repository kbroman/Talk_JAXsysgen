# enumerate unlabelled graphs with a given number of vertices,
#     as a function of the number of edges

enumgraphs <-
function(nvert=2)
{
  if(nvert > 8) stop("Only thinking about nvert <= 8.")

  if(nvert==1) x <- y <- 0
  else if(nvert==2) {
    x <- c(-0.5,0.5)
    y <- c(0,0)
  }
  else if(nvert==3) {
    x <- c(-0.5,0.5,0)
    y <- c(0.5,0.5,-0.5)
  }
  else if(nvert==4) {
    x <- c(-0.5,0.5,-0.5,0.5)
    y <- c(0.5,0.5,-0.5,-0.5)
  }
  else if(nvert==5) {
    x <- c(-0.3,0.3,-0.5,0.5,0)
    y <- c(0.5,0.5,0,0,-0.5)
  }
  else if(nvert==6) {
    x <- c(-0.25,0.25,-0.5,0.5,-0.25,0.25)
    y <- c(0.5,0.5,0,0,-0.5,-0.5)
  }
  else if(nvert==7) {
    x <- c(-0.2,0.2,-0.5,0.5,-0.4,0.4,0)
    y <- c(0.5,0.5,0.2,0.2,-0.2,-0.2,-0.5)
  }
  else if(nvert==8) {
    x <- c(-0.2,0.2,-0.5,0.5,-0.5,0.5,-0.2,0.2)
    y <- c(0.5,0.5,0.2,0.2,-0.2,-0.2,-0.5,-0.5)
  }
  
  maxedge <- choose(nvert,2)

  if(nvert==1)
    plot(x,y, xaxt="n", yaxt="n", xlab="", ylab="",
         pch=16, cex=1.5, xlim=c(-1,1), ylim=c(-1,1),
         xaxs="i", yaxs="i")

  else if(nvert==2) {
    plot(x,y, xaxt="n", yaxt="n", xlab="", ylab="",
         pch=16, cex=1.5, xlim=c(-1,3), ylim=c(-1,1),
         xaxs="i", yaxs="i")
    points(x+2,y, pch=16, cex=1.5)
    segments(x[1]+2, y[1], x[2]+2, y[2], lwd=2)
    abline(v=1)
  }

  else if(nvert==3) {
    plot(x,y, xaxt="n", yaxt="n", xlab="", ylab="",
         pch=16, cex=1.5, xlim=c(-1,7), ylim=c(-1,1),
         xaxs="i", yaxs="i")
    for(i in c(2,4,6)) {
      points(x+i,y, pch=16, cex=1.5)
      segments(x[1]+i, y[1], x[2]+i, y[2], lwd=2)
    }
    for(i in c(4,6)) 
      segments(x[1]+i, y[1], x[3]+i, y[3], lwd=2)
    segments(x[2]+6,y[2],x[3]+6, y[3], lwd=2)
      
    abline(v=c(1,3,5))
  }

  else if(nvert==4) {
    plot(x,y, xaxt="n", yaxt="n", xlab="", ylab="",
         pch=16, cex=1.5, xlim=c(-1,maxedge*2-1), ylim=c(-5,1),
         xaxs="i", yaxs="i")
    for(i in (1:maxedge)*2) {
      points(x+i,y, pch=16, cex=1.5)
    }
    for(i in (1:maxedge)*2) {
      points(x+i,y, pch=16, cex=1.5)
    }
    for(i in c(4,6,8))
        points(x+i,y-2, pch=16, cex=1.5)
    points(x+6,y-4,pch=16,cex=1.5)

    segments(x[1]+2, y[1], x[2]+2, y[2], lwd=2)

    segments(x[1]+4, y[1], x[2]+4, y[2], lwd=2)
    segments(x[1]+4, y[1], x[3]+4, y[3], lwd=2)

    segments(x[1]+4, y[1]-2, x[2]+4, y[2]-2, lwd=2)
    segments(x[4]+4, y[4]-2, x[3]+4, y[3]-2, lwd=2)

    segments(x[1]+6, y[1], x[2]+6, y[2], lwd=2)
    segments(x[1]+6, y[1], x[3]+6, y[3], lwd=2)
    segments(x[1]+6, y[1], x[4]+6, y[4], lwd=2)

    segments(x[1]+6, y[1]-2, x[2]+6, y[2]-2, lwd=2)
    segments(x[1]+6, y[1]-2, x[3]+6, y[3]-2, lwd=2)
    segments(x[2]+6, y[2]-2, x[4]+6, y[4]-2, lwd=2)

    segments(x[1]+6, y[1]-4, x[2]+6, y[2]-4, lwd=2)
    segments(x[1]+6, y[1]-4, x[3]+6, y[3]-4, lwd=2)
    segments(x[2]+6, y[2]-4, x[3]+6, y[3]-4, lwd=2)

    segments(x[1]+8, y[1], x[2]+8, y[2], lwd=2)
    segments(x[2]+8, y[2], x[4]+8, y[4], lwd=2)
    segments(x[3]+8, y[3], x[4]+8, y[4], lwd=2)
    segments(x[1]+8, y[1], x[3]+8, y[3], lwd=2)

    segments(x[1]+8, y[1]-2, x[2]+8, y[2]-2, lwd=2)
    segments(x[2]+8, y[2]-2, x[4]+8, y[4]-2, lwd=2)
    segments(x[3]+8, y[3]-2, x[2]+8, y[2]-2, lwd=2)
    segments(x[1]+8, y[1]-2, x[3]+8, y[3]-2, lwd=2)

    for(i in 1:3)
      for(j in (i+1):4)
        segments(x[i]+10, y[i], x[j]+10, y[j], lwd=2)

    abline(v=c(1,3,5,7,9), h=c(-1,-3))
  }

  else if(nvert==5) {
    plot(x,y, xaxt="n", yaxt="n", xlab="", ylab="",
         pch=16, cex=1.5, xlim=c(-1,maxedge*2-1), ylim=c(-5,1),
         xaxs="i", yaxs="i")
    for(i in (1:maxedge)*2) {
      points(x+i,y, pch=16, cex=1.5)
    }
    abline(v=c(1,3,5,7,9), h=c(-1,-3))
  }

  invisible()

}
  

