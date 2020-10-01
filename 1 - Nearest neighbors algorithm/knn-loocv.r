source('./knn.r', chdir = T)

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
      distance[j] <- euclidean(dataset[j, 3:4], iris[i, 3:4])
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
  for (i in 1:length(ans)) {
    
    ans[i] = ans[i] / length(iris[,1] - 1)
  }
  
  return (ans)
}

par(mfrow=c(1,1), pty="s")
# LOO CV, looking for the best k
stats <- fast_cvloo_knn()
plot(1:(length(iris[,1]) - 1), stats, type="l", xlab="k", ylab="Accuracy")

maxPoint = which(stats == max(stats))
points((1:(length(iris[,1]) - 1))[maxPoint], stats[maxPoint], pch = 19, col = "red")

bestK <- which.max(stats)
cat("Best K =", bestK, "\n")