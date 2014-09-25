set.seed(121119+SUB)

load("Rcache/alod.RData")


operm <- scanone(iron, n.perm=100, perm.Xsp=TRUE)
save(operm, file="Rcache/perm_chrspSUB.RData", compress=TRUE)
