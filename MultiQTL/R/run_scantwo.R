data(hyper)

hyper <- calc.genoprob(hyper, step=2.5, err=0.001)
out2 <- scantwo(hyper, chr=1:19)
save(out2, file="Rcache/out2_fine.RData", compress=TRUE)

hyper <- calc.genoprob(hyper, step=10, err=0.001)
out2.c <- scantwo(hyper, chr=1:19)
save(out2.c, file="Rcache/out2_coarse2.RData", compress=TRUE)
