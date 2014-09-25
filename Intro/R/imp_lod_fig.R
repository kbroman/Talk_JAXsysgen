if(bw) source("colors_bw.R") else source("colors.R")

if(bw) {
  pdf(file="../Figs/imp_lod_bw.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="black",col="black",col.axis="black",col.lab="black",
      bg=bgcolor)
} else {
  pdf(file="../Figs/imp_lod.pdf", width=9.75, height=6.5, pointsize=12, onefile=TRUE)
  par(fg="white",col="white",col.axis="white",col.lab="white",
      bg=bgcolor)
}

file <- "Rcache/ironimpillustration.RData"
if(file.exists(file)) {
  load(file)
} else {
  set.seed(12201969)
  library(qtlbook)
  data(iron)
  iron$pheno[,1] <- log2(iron$pheno[,1])
  n.imp <- 64
  iron <- subset(iron,chr="16")
  iron <- sim.geno(iron, step=1, n.draws=n.imp)
  out.imp <- scanone(iron, method="imp")
  n.draw <- 16
  temp <- as.data.frame(matrix(ncol=3+n.draw, nrow=nrow(out.imp)))
  temp[,1:3] <- out.imp
  names(temp)[1:3] <- names(out.imp)
  rownames(temp) <- rownames(out.imp)
  out.imp <- temp
  class(out.imp) <- c("scanone", "data.frame")
  temp <- iron
  for(i in 1:n.draw) {
    temp$geno[[1]]$data <- iron$geno[[1]]$draws[,,i]
    temp$geno[[1]]$map <- attr(iron$geno[[1]]$draws,"map")
    out.imp[,i+3] <- scanone(temp, method="mr")[,3]
  }
  save(out.imp, n.draw, n.imp, file=file, compress=TRUE)
}

if(bw) {
  par(mar=c(4.1,4.1,0.1,0.1))
  plot(out.imp, lod=2, col="black", ylab="LOD score",
       ylim=c(0,max(unlist(out.imp[,-(1:2)]))))
  for(i in 2:n.draw)
    plot(out.imp, lod=i, add=TRUE, col="gray70")
  plot(out.imp, add=TRUE)
} else {
  par(mar=c(4.1,4.1,0.1,0.1))
  plot(out.imp, lod=2, col=color[1], ylab="LOD score",
       ylim=c(0,max(unlist(out.imp[,-(1:2)]))))
  for(i in 2:n.draw)
    plot(out.imp, lod=i, add=TRUE, col=color[1])
  plot(out.imp, add=TRUE, col=color2[1])
}  



dev.off()

# end of imp_illustration.R
