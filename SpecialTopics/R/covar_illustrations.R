source("colors.R")

# illustration for additive covariate
pdf("../Figs/addcovar.pdf", width=9.5, height=6.5, pointsize=20)

par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor)

me1 <- c(10,30,50,70)
g1 <- c(1,2,1,2)
g2 <- g1 + 3
me2 <- c(10,40,  25,90)

par(mar=c(3.6,4.1,0.1,0.1))
plot(g1, me1, lwd=2, xaxt="n", xlab="", las=1,
     xlim=c(0.5,4.5), ylim=c(0,80), ylab="Average phenotype",
     xaxs="i")
abline(v=2.5)
axis(side=1, at=1:2, labels=c("female", "male"))
lines(g1[1:2], me1[1:2], lwd=2)
lines(g1[3:4], me1[3:4], lwd=2, lty=2)
points(g1, me1, col="white", pch=16, lwd=2)
points(g1, me1, lwd=2)
u <- par("usr")
text(1.5, u[3]-diff(u[3:4])*0.18, "sex", xpd=TRUE, cex=1.1)
text(2.3, me1[c(2,4)], c("AA", "AB"))

arrows(1, me1[1]+2, 1, me1[3]-2, col=color[1], len=0.1, code=3)
text(1.1, mean(me1[c(1,3)]), expression(beta[q]), col=color[1])

arrows(2, me1[1]+1, 2, me1[2]-2, col=color[3], len=0.1, code=3)
segments(1.95, me1[1], 2.05, me1[1], col=color[3])
text(2.1, mean(me1[c(1,2)]), expression(beta[x]), col=color[3])

u <- par("usr")

text(4.25, 40, "AA")
text(4.25, 80, "AB")
thex <- seq(2.7, 4.3, len=5)
axis(side=1, at=thex, label=0:4)
text((2.5+u[2])/2, u[3]-diff(u[3:4])*0.18, "x", xpd=TRUE, cex=1.1)
arrows(2.7, (2.7-3)*20+10 + 1, 2.7, (2.7-3)*20+50 - 1, col=color[1],
       len=0.1, code=3)
text(2.8, mean(c((2.7-3)*20+10, (2.7-3)*20+50)), expression(beta[q]), col=color[1])
segments(thex[3], (thex[3]-3)*20+10, thex[4], (thex[3]-3)*20+10, col=color[3])
segments(thex[4], (thex[3]-3)*20+10, thex[4], (thex[4]-3)*20+10, col=color[3])
text(thex[4]+0.1, ((thex[3]-3)*20+10 + (thex[4]-3)*20+10)/2, expression(beta[x]), col=color[3])

segments(2.5, (2.5-3)*20+10, u[2], (u[2]-3)*20+10, lwd=2)
segments(2.5, (2.5-3)*20+50, u[2], (u[2]-3)*20+50, lwd=2, lty=2)


dev.off()


# illustration for model for y/x

pdf("../Figs/y_over_x.pdf", width=6.5, height=6.5, pointsize=20)

par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor)

par(mar=c(4.1,5.1,0.1,0.1))
plot(0, 0, xlab="x", las=1, type="n", yaxs="i",
     xlim=c(0,4.5), ylim=c(0,0.35), ylab="Average phenotype",
     xaxs="i", yaxt="n")
axis(side=2, at=seq(0, 0.3, by=0.1), las=1)
segments(2, 2*0.03, 3, 2*0.03, col=color[1])
segments(3, 2*0.03, 3, 3*0.03, col=color[1])
text(3.1, 2.5*0.03, expression(mu), col=color[1], adj=c(0,0.5))

segments(2, 2*0.07, 3, 2*0.07, col=color[3])
segments(3, 2*0.07, 3, 3*0.07, col=color[3])
text(3.1, 2.5*0.07, expression(mu+beta[q]), col=color[3], adj=c(0,0.5))

text(4, 0.14, "AA")
text(4, 0.31, "AB")

abline(0, 0.03, lwd=2)
abline(0, 0.07, lty=2, lwd=2)


dev.off()



# illustration for interactive covariate

pdf("../Figs/intcovar.pdf", width=9.5, height=6.5, pointsize=20)

par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor)

thegreen <- color[4]

me1 <- c(30,20,50,70)
g1 <- c(1,2,1,2)
g2 <- g1 + 3
me2 <- c(10,40,  25,90)

par(mar=c(3.6,4.1,0.1,0.1))
plot(g1, me1, lwd=2, xaxt="n", xlab="", las=1,
     xlim=c(0.5,4.5), ylim=c(10,80), ylab="Average phenotype",
     xaxs="i")
abline(v=2.5)
axis(side=1, at=1:2, labels=c("female", "male"))
lines(g1[1:2], me1[1:2], lwd=2)
lines(g1[3:4], me1[3:4], lwd=2, lty=2)
points(g1, me1, col="white", pch=16, lwd=2)
points(g1, me1, lwd=2)
u <- par("usr")
text(1.5, u[3]-diff(u[3:4])*0.18, "sex", xpd=TRUE, cex=1.1)
text(2.3, me1[c(2,4)], c("AA", "AB"))

arrows(1, me1[1]+2, 1, me1[3]-2, col=color[1], len=0.1, code=3)
text(1.05, mean(me1[c(1,3)]), expression(beta[q]), col=color[1],
     adj=c(0, 0.5))

arrows(1, me1[1]-2, 1, me1[2]+1, col=color[3], len=0.1, code=3)
segments(0.95, me1[2], 1.05, me1[2], col=color[3])
text(1.05, mean(me1[c(1,2)]), expression(beta[x]), col=color[3],
     adj=c(0, 0.5))

arrows(2, me1[2]+2, 2, me1[4]-2, col=thegreen, len=0.1, code=3)
text(2.05, mean(me1[c(2,4)]), expression(beta[q]+gamma), col=thegreen,
     adj=c(0, 0.5))


u <- par("usr")

text(4.25, 22.5, "AA")
text(4.25, 80, "AB")
thex <- seq(2.7, 4.3, len=5)
axis(side=1, at=thex, label=0:4)
text((2.5+u[2])/2, u[3]-diff(u[3:4])*0.18, "x", xpd=TRUE, cex=1.1)
arrows(2.7, -(2.7-3)*10+30 + 1, 2.7, (2.7-3)*20+50 - 1, col=color[1],
       len=0.1, code=3)
text(2.75, mean(c(-(2.7-3)*10+30, (2.7-3)*20+50)), expression(beta[q]), col=color[1],
     adj=c(0, 0.5))

segments(thex[3], -(thex[3]-3)*10+30, thex[4], -(thex[3]-3)*10+30, col=color[3])
segments(thex[4], -(thex[3]-3)*10+30, thex[4], -(thex[4]-3)*10+30, col=color[3])
text(thex[4]+0.05, (-(thex[3]-3)*10+30 - (thex[4]-3)*10+30)/2, expression(beta[x]), col=color[3],
     adj=c(0, 0.5))

segments(thex[3], (thex[3]-3)*20+50, thex[4], (thex[3]-3)*20+50, col=thegreen)
segments(thex[4], (thex[3]-3)*20+50, thex[4], (thex[4]-3)*20+50, col=thegreen)
text(thex[4]+0.05, ((thex[3]-3)*20+50 + (thex[4]-3)*20+50)/2, expression(beta[x]+gamma), col=thegreen,
     adj=c(0, 0.5))

segments(2.5, -(2.5-3)*10+30, u[2], -(u[2]-3)*10+30, lwd=2)
segments(2.5, (2.5-3)*20+50, u[2], (u[2]-3)*20+50, lwd=2, lty=2)


dev.off()
