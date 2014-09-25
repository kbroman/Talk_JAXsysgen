set.seed(55033874+SUB)

n.perm <- 10000/10
load("Rcache/alod.RData")
operm <- scanone(hyper, n.perm=n.perm, chr=1:19)
save(operm, file="Rcache/perm1_SUB.RData", compress=TRUE)
