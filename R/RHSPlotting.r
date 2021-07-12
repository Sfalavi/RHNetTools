
# N is a list of 2 matrices
# x is a data.frame of 3 columns from SIDInAnyWave-style object, id, t1 and t2
# results in set of codes indicating entering and leaving of nodes and edges

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> indicateChange <<
#FFFFFFFFFFFFFFFFFFFFFFF
#' @export
indicateChange <- function(N, x) {
  if(length(N) != 2 | all(dim(N[[1]]) != dim(N[[2]])))
    stop("N must contain two matrices of equal dimensionality")
  rownames(x) <- x[,1]
  x <- x[rownames(N[[1]]),]
  for(i in 2:ncol(x)) x[,i] <- !is.na(x[,i])
  # difference (add or remove)
  dN <- N[[2]] - N[[1]]
  dx <- x[,3] - x[,2]
  dN <- ifelse(dN == 0 & N[[1]] == 1, "2", dN)
  dN <- ifelse(dN == 0 & N[[1]] == 0, "-2", dN)
  dx <- ifelse(dx == 0 & x[,2] == T, "2", dx)
  dx <- ifelse(dx == 0 & x[,2] == F, "-2", dx)
  
  list(dN=dN, dx=dx)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> dynPlot <<
#FFFFFFFFFFFFFFFFFFFFFFF
#' @export
dynPlot <- function(N, A, waves, filename="plot.png", 
                    coords=NULL, height=6000, width=6000, 
                    plotargs=NULL, label=F, lab.cex=1,
                    res=300) {
  require(sna)
  for(i in seq(N)) N[[i]][is.na(N)] <- 0
  or <- N[[1]]
  for(i in 2:length(N)) or <- or + N[[i]]
  or[or>0] <- 1
  if(is.null(coords)) {
    coords <- gplot(or)
    dev.off()
  }
  d <- indicateChange(N[waves],active[,c(1,waves+1)])
  net <- ifelse(d[[1]] == "-2", 0, 1)
  ECOLMAP <- setNames(c("white",rgb(.75,0,0),rgb(0,.75,0),rgb(.3,.3,.3)),c(-2,-1,1,2))
  VCOLMAP <- setNames(c("white",rgb(.75,0,0),rgb(0,.75,0),rgb(.3,.3,.3)),c(-2,-1,1,2))
  d[[1]] <- apply(d[[1]], 2, function(x) ECOLMAP[x])
  d[[2]] <- VCOLMAP[d[[2]]]
  if(is.null(plotargs))
    plotargs <- list(vertex.cex=.4, arrowhead.cex=.4, edge.lwd=.5)
  plotargs$label.cex <- lab.cex
  plotargs$coord <- coords
  plotargs$dat <- net
  plotargs$vertex.col <- d[[2]]
  plotargs$edge.col <- d[[1]]
  if(label) plotargs$label <- paste0(A[,waves[1]+1],"-",A[,1])
  png(filename, width=width, height=height, res=res)
  do.call(gplot, plotargs)
  dev.off()
  invisible(coords)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> staticPlot <<
#FFFFFFFFFFFFFFFFFFFFFFF
#' @export
staticPlot <- function(N, A, wave, filename="plot.png",
                       coords=NULL, height=6000, width=6000, 
                       plotargs=NULL, label=F, lab.cex=1,
                       res=300) {
  require(sna)
  for(i in seq(N)) N[[i]][is.na(N)] <- 0
  or <- N[[1]]
  for(i in 2:length(N)) or <- or + N[[i]]
  or[or>0] <- 1
  if(is.null(coords)) {
    coords <- gplot(or)
    dev.off()
  }
  vcol <- ifelse(is.na(A[,wave+1]), "white", rgb(.3,.3,.3))
  if(is.null(plotargs))
    plotargs <- list(vertex.cex=.4, arrowhead.cex=.4, edge.lwd=.5)
  plotargs$label.cex <- lab.cex
  plotargs$vertex.col <- vcol
  plotargs$coord <- coords
  plotargs$edge.col <- rgb(.3,.3,.3)
  plotargs$dat <- N[[wave]]
  if(label) plotargs$label <- paste0(A[,wave+1],"-",A[,1])
  png(filename, width=width, height=height, res=res)
  do.call(gplot, plotargs)
  dev.off()
  invisible(coords)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> anim <<
#FFFFFFFFFFFFFFFFFFFFFFF
#' @export
anim <- function(N, A, waves, animname="anim", folder="", plotargs=NULL,
                 width=6000, height=6000, coords=NULL, label=F, lab.cex=1,
                  res=300) {
  for(i in waves[1]:waves[length(waves)-1]) {
    coords <- staticPlot(N, A, waves[i], paste0(folder, animname, i, ".png"), 
      plotargs=plotargs, coord=coords, label=label, lab.cex=lab.cex,
      res=res)
    dynPlot(N, A, waves[i:(i+1)], paste0(folder, animname, i, "-", i+1, ".png"), 
      plotargs=plotargs, coords=coords, label=label, lab.cex=lab.cex,
      res=res)
  }
  staticPlot(N, A, waves[length(waves)], paste0(folder, animname, length(waves), ".png"),
    plotargs=plotargs, coords=coords, label=label, lab.cex=lab.cex,
    res=res)
}
