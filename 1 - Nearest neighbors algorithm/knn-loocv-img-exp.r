euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

fast_cvloo_knn <- function(dset) {
  maxK <- length(dset[,1]) - 1
  classes = unique(dset$class)
  
  ans <- rep(x = 0, times = maxK)
  for (i in 1:length(dset[,1])) {
    # for (i in 1:10) {
    cat("\r", "Processing sample ", i, " of ", length(dset[,1]))
    dataset <- dset[-i,]
    # print(dataset)
    distance <- array(dim=c(length(dataset[,1])))
    for (j in 1:(length(dataset[,1]))) {
      distance[j] <- euclidean(dataset[j, 1:2], dset[i, 1:2])
    }
    sortedDataset <- dataset[order(distance),]
    
    classesCount <- rep(x = 0, times = length(classes))
    names(classesCount) = classes
    for (j in 1:maxK) {
      classesCount[sortedDataset$class[j]] <- classesCount[sortedDataset$class[j]] + 1
      
      classForCurrentK = names(which.max(classesCount))[1]
      if (classForCurrentK == dset$class[i]) {
        ans[j] = ans[j] + 1
      }
    }
    
  }
  cat("\n")
  for (i in 1:length(ans)) {
    
    ans[i] = ans[i] / length(dset[,1] - 1)
  }
  
  return (ans)
}

kNN <- function(dataset, points, k, dist = euclidean) {
  answer <- array(dim=c(length(points[,1])))
  
  for (i in 1:length(points[,1])) {
    distance <- array(dim=dim(dataset)[1])
    for (j in 1:dim(dataset)[1]) {
      distance[j] <- dist(dataset[j, 1:2], points[i,])
    }
    sortedDataset <- dataset[order(distance),]
    
    classesCount <- c(0, 0, 0)
    names(classesCount) = unique(dataset$class)
    for (j in 1:k) {
      classesCount[sortedDataset$class[j]] <- classesCount[sortedDataset$class[j]] + 1
    }
    answer[i] = names(which.max(classesCount))[1]
  }
  
  return (answer)
}

df = data.frame(x = double(), y = double(), class = character())

df <- rbind(df, data.frame(x = 122, y = 2208, class = "class-1"))
df <- rbind(df, data.frame(x = 148, y = 1136, class = "class-2"))
df <- rbind(df, data.frame(x = 270, y = 2186, class = "class-1"))
df <- rbind(df, data.frame(x = 271, y = 2016, class = "class-1"))
df <- rbind(df, data.frame(x = 306, y = 1023, class = "class-2"))
df <- rbind(df, data.frame(x = 413, y = 1153, class = "class-2"))
df <- rbind(df, data.frame(x = 503, y = 2013, class = "class-1"))
df <- rbind(df, data.frame(x = 541, y = 971, class = "class-2"))
df <- rbind(df, data.frame(x = 706, y = 1018, class = "class-2"))
df <- rbind(df, data.frame(x = 837, y = 867, class = "class-2"))
df <- rbind(df, data.frame(x = 875, y = 1011, class = "class-2"))
df <- rbind(df, data.frame(x = 927, y = 1915, class = "class-1"))
df <- rbind(df, data.frame(x = 997, y = 897, class = "class-2"))
df <- rbind(df, data.frame(x = 1005, y = 529, class = "class-2"))
df <- rbind(df, data.frame(x = 1028, y = 1176, class = "class-2"))
df <- rbind(df, data.frame(x = 1050, y = 1429, class = "class-1"))
df <- rbind(df, data.frame(x = 1125, y = 781, class = "class-2"))
df <- rbind(df, data.frame(x = 1144, y = 1061, class = "class-2"))
df <- rbind(df, data.frame(x = 1146, y = 1751, class = "class-1"))
df <- rbind(df, data.frame(x = 1192, y = 1601, class = "class-1"))
df <- rbind(df, data.frame(x = 1264, y = 888, class = "class-2"))
df <- rbind(df, data.frame(x = 1275, y = 1412, class = "class-1"))
df <- rbind(df, data.frame(x = 1329, y = 576, class = "class-2"))
df <- rbind(df, data.frame(x = 1366, y = 1586, class = "class-1"))
df <- rbind(df, data.frame(x = 1405, y = 386, class = "class-2"))
df <- rbind(df, data.frame(x = 1432, y = 1226, class = "class-1"))
df <- rbind(df, data.frame(x = 1597, y = 279, class = "class-2"))
df <- rbind(df, data.frame(x = 1615, y = 1604, class = "class-1"))
df <- rbind(df, data.frame(x = 1631, y = 1200, class = "class-1"))
df <- rbind(df, data.frame(x = 1638, y = 1365, class = "class-1"))
df <- rbind(df, data.frame(x = 1765, y = 1262, class = "class-1"))
df <- rbind(df, data.frame(x = 1806, y = 1444, class = "class-1"))
df <- rbind(df, data.frame(x = 1821, y = 1056, class = "class-1"))
df <- rbind(df, data.frame(x = 1896, y = 877, class = "class-1"))
df <- rbind(df, data.frame(x = 1993, y = 695, class = "class-1"))
df <- rbind(df, data.frame(x = 2027, y = 1415, class = "class-1"))
df <- rbind(df, data.frame(x = 2087, y = 889, class = "class-1"))
df <- rbind(df, data.frame(x = 2189, y = 674, class = "class-1"))

z = array(c(c(1164, 1232)), dim = c(1,2))

par(mfrow=c(1,2), pty="s")
# LOO CV, looking for the best k
stats <- fast_cvloo_knn(df)
plot(1:(length(df[,1]) - 1), stats, type="l", xlab="k", ylab="Accuracy")

maxPoint = which(stats == max(stats))
points((1:(length(df[,1]) - 1))[maxPoint], stats[maxPoint], pch = 19, col = "red")

bestK <- which.max(stats)
cat("Best K =", bestK, "\n")


colors <- c("class-1" = "blue", "class-2" = "red")
plot(df[, 1:2], bg = colors[paste(df$class)], pch=23, asp=1)

result <- kNN(df, z, 5)
points(z[,1], z[,2], bg = colors[result], pch = 22)
