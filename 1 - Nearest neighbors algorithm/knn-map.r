euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

kNN <- function(dataset, points, k, dist = euclidean) {
  answer <- array(dim=c(length(points[,1])))
  
  for (i in 1:length(points[,1])) {
    cat("\rPoint", i, "of", length(points[,1]))
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

par(mfrow=c(1,1), pty="s")

xs <- seq(from = 0.5, to = 7.5, by = 0.2)
ys <- seq(from = -2, to = 5, by = 0.2)

z <- array(dim = c(length(xs) * length(ys), 2))

index <- 1
for (i in xs) {
  for (j in ys) {
    z[index,] <- c(i, j)
    index <- index + 1
  }
}

result <- kNN(iris, z, 6)

colors <- c("setosa" = "blue", "virginica" = "red", "versicolor" = "green")
plot(iris[, 3:4], bg = colors[paste(iris$Species)], pch=23, asp=1)
points(z[,1], z[,2], col = colors[result], pch = 21)