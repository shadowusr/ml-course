euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

kNN <- function(dset, labels, point, k, dist = euclidean) {
  distance <- array(dim=dim(dset)[1])
  for (j in 1:dim(dset)[1]) {
    distance[j] <- dist(dset[j,], point)
  }
  sortedDataset <- dset[order(distance),]
  sortedLabels <- labels[order(distance)]
  
  classScores <- array(0L, dim = length(unique(labels)))
  names(classScores) = unique(labels)
  for (j in 1:k) {
    classScores[sortedLabels[j]] <- classScores[sortedLabels[j]] + 1
  }
  classScores <- classScores / k
  
  return (classScores)
}

gaussKernel <- function(dist, h) exp(sqrt(h / pi * 0.5) * -0.5 * (dist * h) ^ 2)

parzen <- function(dset, labels, point, h, kernel, dist = euclidean) {
  dsetLength <- length(dset[,1])
  distances <- array(dim = c(dsetLength))
  
  uLabels <- unique(labels)
  classScores <- array(0L, dim = c(length(uLabels)))
  names(classScores) = uLabels;
  # classScores["undefined"] <- 0.000001
  
  for (i in 1:dsetLength) {
    classScores[labels[i]] <- classScores[labels[i]] + kernel(dist(point, dset[i,]), h)
  }
  
  return(classScores)
}

computeMargins <- function(dset, labels) {
  dsetLen <- dim(dset)[1]
  margins <- array(0L, dim = dsetLen)
  for (i in 1:dsetLen) {
    cat("\rProcessing", i, "of", dsetLen)
    # classScores <- kNN(dset[-i,], labels[-i], dset[i,], 6)
    classScores <- parzen(dset[-i,], labels[-i], dset[i,], 1.6, gaussKernel)
    # cat(paste(labels[i]), names(classScores[which.max(classScores)]), "\n")
    
    margins[i] <- classScores[labels[i]] - max(classScores[-as.integer(labels[i])])
    
    if (paste(labels[i]) != names(classScores[which.max(classScores)])) {
      #cat(classScores, labels[i], classScores[labels[i]], max(classScores[-which.max(classScores)]), classScores[-labels[i]], margins[i], "\n")
    }
  }
  cat("\n")

  return (margins)
}

# tt <- kNN(iris[3:4], iris$Species, c(2.5, 1.1), 6)
# tt <- parzen(iris[3:4], iris$Species, c(2.5, 1.1), 1.6, kernel = gaussKernel)

margins <- computeMargins(iris[3:4], iris$Species)

margins <- margins / max(abs(margins))
colors <- array(dim = length(margins))

for (i in 1:length(margins)) {
  colors[i] <- if (margins[i] > 0) rgb(1 - margins[i], 1, 0) else rgb(1, 1 - abs(margins[i]), 0)
}

par(mfcol = c(1,1))
plot(iris[,3], iris[,4], col = colors, pch = 16)


# 1 => rgb(0, 1, 0)
# 0.5 => rgb()
# 0 => rgb(1, 1, 0)
# -1 => rgb(1, 0, 0)

