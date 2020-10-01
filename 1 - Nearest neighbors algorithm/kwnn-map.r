euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

w1 <- function(k, dist) {
  return (0.952381 ^ k)
}

kwNN <- function(dataset, points, k, dist = euclidean, weight = w1) {
  answer <- array(dim = c(length(points[,1])))
  classes = unique(iris$Species)
  
  for (i in 1:length(points[,1])) {
    cat("\rPoint", i, "of", length(points[,1]))
    distance <- array(dim = c(length(dataset[,1])))
    for (j in 1:length(dataset[,1])) {
      distance[j] = dist(points[i, ], dataset[j, 3:4])
    }
    sortedDataset <- dataset[order(distance),]
    distance <- sort(distance)
    
    scores <- rep(x = 0, times = length(classes))
    names(scores) = classes
    for (j in 1:k) {
      scores[sortedDataset$Species[j]] = scores[sortedDataset$Species[j]] + weight(j, distance[j])
    }
    
    answer[i] = names(which.max(scores))[1]
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

result <- kwNN(iris, z, 30)

colors <- c("setosa" = "blue", "virginica" = "red", "versicolor" = "green")
plot(iris[, 3:4], bg = colors[paste(iris$Species)], pch=23, asp=1)
points(z[,1], z[,2], col = colors[result], pch = 21)