euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

kNN <- function(dataset, points, k, dist = euclidean) {
  answer <- array(dim=c(length(points[,1])))
  
  for (i in 1:length(points[,1])) {
    distance <- array(dim=dim(dataset)[1])
    for (j in 1:dim(dataset)[1]) {
      distance[j] <- dist(dataset[j, 3:4], points[i,])
    }
    sortedDataset <- dataset[order(distance),]
    
    classesCount <- c(0, 0, 0)
    names(classesCount) = unique(iris$Species)
    for (j in 1:k) {
      classesCount[sortedDataset$Species[j]] <- classesCount[sortedDataset$Species[j]] + 1
    }
    answer[i] = names(which.max(classesCount))[1]
  }
  
  return (answer)
}

fast_cvloo_knn <- function() {
  maxK <- length(iris[,1]) - 1
  classes = unique(iris$Species)
  
  ans <- rep(x = 0, times = maxK)
  for (i in 1:length(iris[,1])) {
    # for (i in 1:10) {
    cat("\r", "Processing sample ", i, " of ", length(iris[,1]))
    dataset <- iris[-i,]
    # print(dataset)
    distance <- array(dim=c(length(dataset[,1])))
    for (j in 1:(length(dataset[,1]))) {
      distance[j] <- euclidean(dataset[j, 1:4], iris[i, 1:4])
    }
    sortedDataset <- dataset[order(distance),]
    
    classesCount <- rep(x = 0, times = length(classes))
    names(classesCount) = classes
    for (j in 1:maxK) {
      classesCount[sortedDataset$Species[j]] <- classesCount[sortedDataset$Species[j]] + 1
      
      classForCurrentK = names(which.max(classesCount))[1]
      if (classForCurrentK == iris$Species[i]) {
        ans[j] = ans[j] + 1
      }
    }
    
  }
  cat("\n")
  # for (i in 1:length(ans)) {
  #   
  #   ans[i] = ans[i] / length(iris[,1] - 1)
  # }
  
  return (ans)
}



fast_cvloo_kwnn <- function() {
  maxK <- 149
  classes = unique(iris$Species)
  qs <- seq(from = 0.0, to = 1.0, by = 0.05)
  
  ans <- array(0L, dim = c(maxK, length(qs)))
  for (i in 1:length(iris[,1])) { # samples
    cat("\r", "Processing sample ", i, " of ", length(iris[,1]))
    dataset <- iris[-i,]
    distance <- array(dim=c(length(dataset[,1])))
    for (j in 1:(length(dataset[,1]))) {
      distance[j] <- euclidean(dataset[j, 1:4], iris[i, 1:4])
    }
    sortedDataset <- dataset[order(distance),]
    distance <- sort(distance)
    
    classesCount <- array(0L, dim = c(length(classes), length(qs)))
    dimnames(classesCount) = list(classes)
    for (j in 1:maxK) { # k's
      index <- 1
      for (q in qs) {
        classesCount[sortedDataset$Species[j],index] <- classesCount[sortedDataset$Species[j],index] + (q ^ j)
        classForCurrentK = names(which.max(classesCount[,index]))[1]
        
        if (classForCurrentK == paste(iris$Species[i])) {
          ans[j,index] = ans[j,index] + 1
        }
        index <- index + 1
      }
    }
    index <- index + 1
  }
  cat("\n")
  
  # for (i in 1:length(ans)) {
  #   
  #   ans[i] = ans[i] / (length(iris[,1]) - 1)
  # }
  
  return (ans)
}

knn_accuracy = fast_cvloo_knn()
kwnn_accuracy = fast_cvloo_kwnn()

cat("Best accuracy for knn: ", max(knn_accuracy) / length(iris[,1]), ";\nBest accuracy for weighted knn:", max(kwnn_accuracy) / length(iris[,1]))

