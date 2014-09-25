source("colors.R")

file <- "Rcache/plotscantwo.RData"
if(file.exists(file)) {
  load(file)
} else {
  load("Rcache/alod.RData")
  iron <- calc.genoprob(iron, step=10, err=0.001)
  out2 <- scantwo(iron, phe=1, method="hk")
  save(out2, file=file, compress=TRUE)
}

pdf(file="../Figs/plot_scantwo.pdf", width=5, height=5, pointsize=8, onefile=TRUE)
par(fg="white",col="white",col.axis="white",col.lab="white",
    bg=bgcolor)
par(mar=c(5.1,4.1,1.1,1.1))

plot(out2) 

dev.off()

