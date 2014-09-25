
data(hyper)
hyper <- sim.geno(hyper, step=1, err=0.001, n.draws=128)

qc <- c(1,4,6,15)
qp <- c(68.3,30,60,18)
qtl <- makeqtl(hyper, qc, qp)

#1 @ 68.3 ["c1.loc65"]; 4 @ 30 [D4Mit164]
#6 @ 60 [c6.loc60]; 15 @ 18 [D15Mit152]

source("multqtlfunc.R")
phe <- hyper$pheno[,1]
out.fq <- fitqtl(phe, qtl, formula= y~Q1+Q2+Q3*Q4)

out.rq <- refineqtl(hyper, chr=qc, pos=qp, formula=y~Q1+Q2+Q3*Q4)

qp <- c(76.3,30,60,18.5)

# drop one at a time
out.rq1 <- refineqtl(hyper, chr=qc[-1], pos=qp[-1], formula=y~Q1+Q2*Q3)
out.rq2 <- refineqtl(hyper, chr=qc[-2], pos=qp[-2], formula=y~Q1+Q2*Q3)
out.rq3 <- refineqtl(hyper, chr=qc, pos=qp, formula=y~Q1+Q2+Q3+Q4)
out.rq4 <- refineqtl(hyper, chr=qc[-3], pos=qp[-3], formula=y~Q1+Q2+Q3)
out.rq5 <- refineqtl(hyper, chr=qc[-4], pos=qp[-4], formula=y~Q1+Q2+Q3)

# scan for an additional interaction
out.rq6 <- refineqtl(hyper, chr=qc, pos=qp, formula=y~Q1*Q2+Q3*Q4)
out.rq7 <- refineqtl(hyper, chr=qc, pos=qp, formula=y~Q1+Q2+Q3+Q4+Q3:Q4+Q1:Q3)
out.rq8 <- refineqtl(hyper, chr=qc, pos=qp, formula=y~Q1+Q2+Q3+Q4+Q3:Q4+Q1:Q4)
out.rq9 <- refineqtl(hyper, chr=qc, pos=qp, formula=y~Q1+Q2+Q3+Q4+Q3:Q4+Q2:Q3)
out.rq10 <- refineqtl(hyper, chr=qc, pos=qp, formula=y~Q1+Q2+Q3+Q4+Q3:Q4+Q2:Q4)


out.sq <- vector("list", 19)
for(i in (1:19)[-c(1,4,6,15)]) {
  cat(i,"\n")
  out.sq[[i]] <- scanqtl(hyper, chr=c(i,1,4,6,15),
                         pos=list(c(-Inf,Inf), 76.3, 30, 60, 18.5),
                         formula=y~Q1+Q2+Q3+Q4*Q5)
}
out.sq[[1]] <- scanqtl(hyper, chr=c(1,1,4,6,15),
                       pos=list(c(-Inf,Inf), c(-Inf,Inf), 30, 60, 18.5),
                       formula=y~Q1+Q2+Q3+Q4*Q5)

out.sq[[4]] <- scanqtl(hyper, chr=c(4,1,4,6,15),
                       pos=list(c(-Inf,Inf), 76.3, c(-Inf,Inf), 60, 18.5),
                       formula=y~Q1+Q2+Q3+Q4*Q5)

out.sq[[6]] <- scanqtl(hyper, chr=c(6,1,4,6,15),
                       pos=list(c(-Inf,Inf), 76.3, 30, c(-Inf,Inf), 18.5),
                       formula=y~Q1+Q2+Q3+Q4*Q5)

out.sq[[15]] <- scanqtl(hyper, chr=c(15,1,4,6,15),
                       pos=list(c(-Inf,Inf), 76.3, 30, 60, c(-Inf,Inf)),
                       formula=y~Q1+Q2+Q3+Q4*Q5)

out.rq11 <- refineqtl(hyper, chr=c(qc,2), pos=c(qp,35.7), formula=y~Q1+Q2+Q3+Q4+Q5+Q3:Q4)
out.rq12 <- refineqtl(hyper, chr=c(qc,5), pos=c(qp,68), formula=y~Q1+Q2+Q3+Q4+Q5+Q3:Q4)

out.sq <- vector("list", 361)
k <- 1
for(i in 19:5) {
  for(j in 1:19) {
    cat(i,j,"\n")
    out.sq[[k]] <- scanqtl(hyper, chr=c(1,4,6,15,i,j),
                           pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf), c(-Inf,Inf)),
                           formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4+Q5:Q6)
    attr(out.sq[[k]], "chr") <- c(i,j)
    k <- k + 1
  }
}


out.sq1 <- vector("list", 19)
for(i in 1:19) {
  cat(i,"\n")
  out.sq1[[i]] <- scanqtl(hyper, chr=c(1,4,6,15,i),
                          pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf)),
                          formula=y~Q1+Q2+Q3+Q4+Q5+Q3:Q4+Q1:Q5)
}

out.sq2 <- vector("list", 19)
for(i in 1:19) {
  cat(i,"\n")
  out.sq2[[i]] <- scanqtl(hyper, chr=c(1,4,6,15,i),
                          pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf)),
                          formula=y~Q1+Q2+Q3+Q4+Q5+Q3:Q4+Q2:Q5)
}

out.sq3 <- vector("list", 19)
for(i in 1:19) {
  cat(i,"\n")
  out.sq3[[i]] <- scanqtl(hyper, chr=c(1,4,6,15,i),
                          pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf)),
                          formula=y~Q1+Q2+Q3+Q4+Q5+Q3:Q4+Q3:Q5)
}

out.sq4 <- vector("list", 19)
for(i in 1:19) {
  cat(i,"\n")
  out.sq4[[i]] <- scanqtl(hyper, chr=c(1,4,6,15,i),
                          pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf)),
                          formula=y~Q1+Q2+Q3+Q4+Q5+Q3:Q4+Q4:Q5)
}

######################################################################


######################################################################

# two linked qtl on a chromosome?

out.sq.linked <- vector("list", 19)
for(i in 19:1) {
  cat("chr", i,"\n")
  out.sq.linked[[i]] <- scanqtl(hyper, chr=c(1,4,6,15,i,i),
                         pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf), c(-Inf,Inf)),
                         formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4)
  cat(i, max(out.sq.linked[[i]])-23.109, "\n")
}

  
find.pseudomarker(hyper, 3, c(37.2,46.2))
effectplot(hyper, mname1="c3.loc35", mname2="c3.loc44")
g <- pull.geno(hyper)[,c("D3Mit11", "D3Mit14")]
plot(phe ~ I(g[,1] + (g[,2]-1)*2 + runif(length(phe), -0.2,0.2)))


######################################################################

t1 <- scanqtl(hyper, chr=c(1,4,6,15,1,2),
              pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf), c(-Inf,Inf)),
              formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4+Q5:Q6)
t2 <- scanqtl(hyper, chr=c(1,4,6,15,2,1),
              pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf), c(-Inf,Inf)),
              formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4+Q5:Q6)

tc <- c(1,4,6,15,1,2)
tp <- c(76.3,30,60,18.5,3.3,8.7)
qtl <- makeqtl(hyper, tc, tp)
tt1 <- fitqtl(phe, qtl, formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4+Q5:Q6, drop=FALSE)
tc <- c(1,4,6,15,1,2)
tp <- c(76.3,30,60,18.5,4.3,7.7)
qtl <- makeqtl(hyper, tc, tp)
tt2 <- fitqtl(phe, qtl, formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4+Q5:Q6, drop=FALSE)

######################################################################

hyper <- sim.geno(hyper, step=2.5, err=0.001, n.draws=64)
out.sq.2int <- vector("list", 190)
k <- 1
null <- scanqtl(hyper, chr=c(1,4,6,15),
                pos=list(76.3, 30, 60, 18.5),
                                formula=y~Q1+Q2+Q3+Q4+Q3:Q4)

for(i in 19:1)
  for(j in i:19) {
    cat(i,j,"\n")
    out.sq.2int[[k]] <- scanqtl(hyper, chr=c(1,4,6,15,i,j),
                                pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf), c(-Inf,Inf)),
                                formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4+Q5:Q6)
    attr(out.sq.2int[[k]], "chr") <- c(i,j)
    cat(i,j,max(out.sq.2int[[k]]), "\n")
    k <- k + 1
  }
}
    

out.sq.2add <- vector("list", 190)
k <- 1
for(i in 19:1)
  for(j in i:19) {
    cat(i,j,"\n")
    out.sq.2add[[k]] <- scanqtl(hyper, chr=c(1,4,6,15,i,j),
                                pos=list(76.3, 30, 60, 18.5, c(-Inf,Inf), c(-Inf,Inf)),
                                formula=y~Q1+Q2+Q3+Q4+Q5+Q6+Q3:Q4)
    attr(out.sq.2int[[k]], "chr") <- c(i,j)
    cat(i,j,max(out.sq.2int[[k]]), "\n")
    k <- k + 1
  }
}
