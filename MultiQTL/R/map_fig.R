if(bw) source("colors_bw.R") else source("colors.R")
source("my_plot_map.R")


data(hyper)

#png(file="../Figs/geneticmap.png", width=1125, height=750, res=108,
#       pointsize=14)
if(bw) {
  pdf(file="../Figs/geneticmap_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)

  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
} else  {
  pdf(file="../Figs/geneticmap.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)#, cex.axis=1.5, cex.lab=1.5)
}
par(mar=c(5.1, 5.1, 1.1, 1.1))
plot.map(hyper, main="")
dev.off()

