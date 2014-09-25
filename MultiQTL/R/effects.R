
data(hyper)
hyper.sub <- sim.geno(subset(hyper, chr=c(1,4,6,15)), step=1,
                      n.draws=1024, err=0.001)
save(hyper.sub, file="Rcache/hyperimp_c14615.RData", compress=TRUE)

# chr  1: 48.3  (c1.loc45)
# chr  4: D4Mit164
# chr  6: 24.0  (c6.loc24)
# chr 15: 19.5  (c15.loc14)


effectplot(hyper.sub, mname1="c1.loc45", ylim=c(97, 106.5),
           main="Chr 1 @ 48 cM", ylab="blood pressure",
           xlab="Genotype")
effectplot(hyper.sub, mname1="D4Mit164", ylim=c(97, 106.5),
           main="Chr 4 @ 30 cM", ylab="blood pressure",
           xlab="Genotype")
effectplot(hyper.sub, mname1="c6.loc24", ylim=c(97, 106.5),
           main="Chr 6 @ 24 cM", ylab="blood pressure",
           xlab="Genotype")
effectplot(hyper.sub, mname1="c15.loc14", ylim=c(97, 106.5),
           main="Chr 15 @ 20 cM", ylab="blood pressure",
           xlab="Genotype")
