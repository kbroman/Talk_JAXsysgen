color <- c(rgb(0,0,0,maxColorValue=255), # black
           "gray40",
           "gray70",
           "gray70",
           rgb(0,  0,0,maxColorValue=255), # purple
           rgb(0,0,0,maxColorValue=255), # light purple
           rgb(0,0,0,maxColorValue=255), # yellow
           rgb(  0,0,0,maxColorValue=255)) # dark blue

bgcolor <- "white"

# text
color2 <- c(rgb(0, 0, 0, maxColorValue=255), # yellow
            rgb(0, 0, 0, maxColorValue=255), # light blue
            rgb(0, 0, 0, maxColorValue=255)) # pink

#par(mar=rep(0.1,4),las=1,fg="white",col="white",col.axis="white",col.lab="white",
#    bg=bgcolor, bty="n")
#plot(0,0,type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(0,90), ylim=c(0,90))
#
#for(i in 1:8)
#  abline(h=i*10,col=color[i],lwd=10)
#
#text(45, 85, "This is yellow", col=color2[1], cex=1.5)
#text(45, 75, "This is blue", col=color2[2], cex=1.5)
#text(45, 65, "This is pink", col=color2[3], cex=1.5)
