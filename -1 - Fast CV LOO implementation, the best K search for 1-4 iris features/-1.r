euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
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
    sortedDataset <- irisSubset[order(distance),]
    
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
  for (i in 1:length(ans)) {
    
    ans[i] = ans[i] / length(iris[,1] - 1)
  }
  
  return (ans)
}


stats <- fast_cvloo_knn()

print(stats)
print(paste("The best K: ", which.max(stats)))

par(mfrow=c(1,1))
plot(1:(length(iris[,1]) - 1), stats, type="l", xlab="k", ylab="Accuracy")