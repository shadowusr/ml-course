euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

gaussKernel <- function(dist, h) exp(sqrt(h / pi * 0.5) * -0.5 * (dist * h) ^ 2)

parzen <- function(dset, labels, point, h, kernel, dist = euclidean) {
  dsetLength <- length(dset[,1])
  
  uLabels <- unique(labels)
  classScores <- array(0L, dim = c(length(uLabels)))
  names(classScores) = uLabels;
  # classScores["undefined"] <- 0.000001 # not needed for gaussian kernel
  
  for (i in 1:dsetLength) {
    classScores[paste(labels[i])] <- classScores[paste(labels[i])] + kernel(dist(point, dset[i,]), h)
  }
  
  return(classScores)
}

computeMargins <- function(dset, labels) {
  dsetLen <- dim(dset)[1]
  margins <- array(0L, dim = dsetLen)
  for (i in 1:dsetLen) {
    cat("\rProcessing", i, "of", dsetLen)
    classScores <- parzen(dset[-i,], labels[-i], dset[i,], 1.6, gaussKernel)
    
    margins[i] <- classScores[labels[i]] - max(classScores[-as.integer(labels[i])])
  }
  cat("\n")
  
  return (margins)
}

removeNoise <- function(dset, margins) { # returns indices of entries that we should get rid of
  negativeMarginIndices <- which(margins < 0)
  negativeMargins <- margins[negativeMarginIndices]
  negativeMargins <- sort(negativeMargins)
  
  dsetLen <- length(margins)
  diff <- abs(negativeMargins[1:(dsetLen - 1)] - negativeMargins[2:dsetLen])
  
  boundary <- negativeMargins[which.max(diff) + 1]
  
  return (which(margins < boundary))
}

stolp <- function(dset, labels, maxErrors, reduceNoise = TRUE) {
  margins <- computeMargins(dset, labels)
  
  # selecting one element with max margin from each class
  result <- data.frame()
  resultLabels <- c()
  indexesToExclude <- c()
  if (reduceNoise) {
    indexesToExclude <- removeNoise(dset, margins)
  }
  for (label in unique(labels)) {
    margins <- computeMargins(dset, labels)
    currentBest <- -1
    for (i in 1:length(margins)) {
      if (labels[i] == label && (currentBest == -1 || margins[currentBest] < margins[i])) {
        currentBest <- i
      }
    }
    
    result <- rbind(result, dset[currentBest,])
    resultLabels <- c(resultLabels, paste(labels[currentBest]))
    indexesToExclude <- c(indexesToExclude, currentBest)
  }
  
  n = length(labels)
  # iteratively reducing the error rate
  while (TRUE) {
    minIndex <- 1
    minValue <- 0
    negativeCount <- 0
    for (i in 1:n) {
      scores <- parzen(result, resultLabels, dset[i,], 1.6, gaussKernel)
      
      margin <- scores[paste(labels[i])] - max(scores[names(scores) != paste(labels[i])])
      if (margin < 0) {
        negativeCount <- negativeCount + 1
      }
      if (!(i %in% indexesToExclude) && margin < minValue) {
        minValue <- margin
        minIndex <- i
      }
    }
    
    cat("Errors: ", negativeCount, "\n")
    
    if (negativeCount <= maxErrors) break
    
    indexesToExclude <- c(indexesToExclude, minIndex)
    result <- rbind(result, dset[minIndex,])
    resultLabels <- c(resultLabels, paste(labels[minIndex]))
  }
  
  return (list("dataset" = result, "labels" = resultLabels))
}

drawPlots <- function() {
  colors <- c("setosa" = "blue", "virginica" = "red", "versicolor" = "green", "undefined" = "grey")
  opd <- stolp(iris[3:4], iris$Species, 6)
  
  png(sprintf("stolp.png"), width = 1024, height = 700, unit = "px", pointsize = 20)
  
  plot(iris[3:4], col = colors[paste(iris$Species)])
  points(opd[['dataset']], pch = 21, bg = colors[opd[['labels']]], col = colors[opd[['labels']]])
  
  dev.off()
  
  xs <- seq(from = 0.5, to = 7.5, by = 0.1)
  ys <- seq(from = -1, to = 3.5, by = 0.1)
  
  kernel <- gaussKernel
  png(sprintf("stolp-map.png"), width = 1024, height = 700, unit = "px", pointsize = 20)
  plot(iris[, 3:4], col = colors[paste(iris$Species)], pch=10, asp=1)
  points(opd[['dataset']], pch = 16, col = colors[opd[['labels']]])
  
  all <- length(xs) * length(ys)
  progress = 1
  for (i in xs) {
    for (j in ys) {
      cat("\rProcessing point", progress, "of", all)
      progress <- progress + 1
      result <- parzen(opd[['dataset']], opd[['labels']], c(i, j), 1.6, kernel)
      result <- names(result)[which.max(result)]
      points(i, j, col = colors[result], pch = 21)
    }
  }
  cat("\rDone.                      \n")
  dev.off()
}

timeMeasurements <- function () {
  opd <- stolp(iris[3:4], iris$Species, 6)
  startTime <- Sys.time()
  n <- dim(iris)[1]
  for (i in 1:n) {
    parzen(opd[['dataset']], opd[['labels']], iris[i, 3:4], 1.6, gaussKernel)
  }
  finishTime <- Sys.time()
  print(paste("Source dataset time: ", finishTime - startTime))
}

# num <- 56
# tt <- parzen(iris[-num,3:4], iris$Species[-num], iris[num,3:4], 1.6, gaussKernel)

# print(tt)
# print(iris$Species[num])
# opd <- stolp(iris[3:4], iris$Species, 5)

# plot(iris[3:4], col = colors[paste(iris$Species)])
# points(opd[['dataset']], pch = 21, bg = colors[opd[['labels']]], col = colors[opd[['labels']]])

# drawPlots()
# timeMeasurements()




