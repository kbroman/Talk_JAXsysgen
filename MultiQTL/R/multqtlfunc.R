######################################################################
# multqtlfunc.R: multiple QTL mapping functions
#
# developmental code, eventually to be placed in R/qtl
#
# copyright (c) 2006, Karl W Broman, Johns Hopkins University
#
# last modified Jan, 2007
# first written Jan, 2007
# Licensed under the GNU General Public License version 2 (June, 1991)
# 
# Contains: refineqtl, get.pos.scanqtl
# 
######################################################################

######################################################################
# refineqtl
# 
# this is like scanqtl, though the positions are all fixed loci;
# it calls scanqtl iteratively, trying to find the best positions for
# each QTL.
#
# the method is like that of that described by Zeng and colleagues
# regarding MIM; each QTL is slid from one end of the chromosome to
# the other, or to the next flanking QTLs, if there are linked ones
#
# maxit is the maximum number of iterations (going through all QTLs
# in each iteration) to perform
######################################################################
refineqtl <-
function(cross, pheno.col=1, chr, pos, covar=NULL, formula, method=c("imp"),
         incl.markers=FALSE, verbose=TRUE, maxit=10)
{
  chrnam <- names(cross$geno[chr]) # turn chr into names

  cross <- subset(cross, chr=unique(chr)) # pull out just those chromosomes

  start <- sapply(cross$geno, function(a) min(a$map))
  end <- sapply(cross$geno, function(a) max(a$map))

  curpos <- pos
  if(verbose) cat("pos:", curpos, "\n")
  converged <- FALSE

  oldo <- NULL
  lc <- length(chr)

  for(i in 1:maxit) {
    if(verbose) cat("Iteration", i, "\n")
    o <- sample(lc)

    # make sure the first here was not the last before
    if(!is.null(oldo)) 
      while(o[1] != oldo[lc])
        o <- sample(lc)
    oldo <- o

    newpos <- curpos
    for(j in o) {
      otherchr <- chrnam[-j]
      otherpos <- newpos[-j]

      thispos <- as.list(newpos)
      if(any(otherchr == chrnam[j])) { # linked QTLs
        linkedpos <- otherpos[otherchr==chr[j]]
        if(any(linkedpos < newpos[j]))
          low <- max(linkedpos[linkedpos < newpos[j]])
        else low <- start[chrnam[j]]
        if(any(linkedpos > newpos[j]))
          high <- max(linkedpos[linkedpos > newpos[j]])
        else high <- end[chrnam[j]]
        
        thispos[[j]] <- c(low, high)
      }
      else 
        thispos[[j]] <- c(start[chrnam[j]], end[chrnam[j]])

      out <- scanqtl(cross, pheno.col, chrnam, thispos, covar,
                     formula, method, incl.markers, verbose=verbose)

      newpos[j] <- as.numeric(strsplit(names(out)[out==max(out)],"@")[[1]][2])

      print(rbind(curpos, newpos))
    }
    if(verbose) cat("pos:", newpos, "\n")

    if(all(curpos == newpos)) {
      converged <- TRUE
      break
    }
    curpos <- newpos
  }

  if(!converged) warning("Didn't converge.")
  
  result <- data.frame(chr=chr, pos=newpos)
  attr(result, "lod") <- max(out)
  result
}


######################################################################
# get.pos.scanqtl
#
# get the positions from the dimnames of scanqtl output
######################################################################
get.pos.scanqtl <-
function(results, give.chr=FALSE)
{
  if(class(results) != "scanqtl")
    stop("This function is only for scanqtl output.")

  dn <- dimnames(results)
  n <- length(dn)
  pos <- vector("list", n)
  for(i in 1:n) 
    pos[[i]] <- as.numeric(matrix(unlist(strsplit(dn[[i]],"@")),byrow=TRUE,ncol=2)[,2])
  
  if(give.chr) {
    chr <- vector("list", n)
    for(i in 1:n) {
      temp <- matrix(unlist(strsplit(dn[[i]],"@")),byrow=TRUE,ncol=2)[,1]
      chr[[i]] <- data.frame(chr=substr(temp, 4, nchar(temp)), pos=pos[[i]])
    }
    
    if(length(chr)==1) return(chr[[1]])
    return(chr)
  }
  if(length(pos)==1) return(pos[[1]])

  pos
}


######################################################################
# convert.scanqtl
######################################################################
convert.scanqtl <-
function(results, nulllod, results.add)
{
  if(class(results) != "scanqtl")
    stop("This function is only for scanqtl output.")

  if(!missing(nulllod) && (class(nulllod)=="scanqtl" || class(nulllod)=="fitqtl"))
    nulllod <- nulllod[[1]][1,4]

  n <- length(dim(results))
  if(n > 2) stop("This function works only for 1d and 2d scans.")

  if(missing(results.add)) {
    if(n != 1) 
      stop("For 2d scans, provide 2d results with full and add've models")

    pos <- get.pos.scanqtl(results, TRUE)
    results <- as.numeric(results)

    if(missing(nulllod))
       warning("Need the nulllod to get real LOD scores.")
    else results <- results - nulllod

    out <- data.frame(chr=pos$chr, pos=pos$pos, lod=results)
    rownames(out) <- paste("c", pos$chr, ".", "loc", 1:length(pos$chr), sep="")
    class(out) <- c("scanone", "data.frame")
    return(out)
  }

  results.full <- results
  results <- diag(results)
  pos.full <- get.pos.scanqtl(results.full, TRUE)
  pos.add <- get.pos.scanqtl(results.add, TRUE)
  if(!all(pos.full[[1]] == pos.full[[2]]) ||
     !all(pos.add[[1]] == pos.add[[2]]))
    stop("This currently works only for 2d scans within a chromosome.")
  if(!all(pos.full[[1]] == pos.add[[1]]))
    stop("Incomptabilities among the scans.")

  pos <- pos.full[[1]]
  n <- nrow(pos)
  map <- data.frame(chr=pos$chr, pos=pos$pos, eq.spacing=rep(1, n),
                    xchr=rep(FALSE, n))

  rownames(map) <- paste("c", pos$chr, ".", "loc", 1:n, sep="")

  results.add <- t(results.add)

  if(missing(nulllod))
    warning("Need the nulllod to get real LOD scores.")
  else {
    results <- as.numeric(results) - nulllod
    results.full <- as.numeric(results.full) - nulllod
    results.add <- as.numeric(results.add) - nulllod
  }

  lod <- matrix(ncol=n, nrow=n)
  diag(lod) <- results
  lod[upper.tri(lod)] <- results.add[upper.tri(lod)]
  lod[lower.tri(lod)] <- results.full[lower.tri(lod)]
  out <- list(lod=lod, map=map, scanoneX=NULL)
  class(out) <- "scantwo"
  out
}

# end of multqtlfunc.R
