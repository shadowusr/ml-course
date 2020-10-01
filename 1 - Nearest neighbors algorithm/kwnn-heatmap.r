library(rgl)

euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

w1 <- function(k, dist) {
  return (1 ^ k)
}

kwNN <- function(dataset, points, k, dist = euclidean, weight = w1) {
  answer <- array(dim = c(length(points[,1])))
  classes = unique(iris$Species)
  
  for (i in 1:length(points[,1])) {
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

kwNN_point <- function(dataset, point, k, dist = euclidean, weight = w1) {
  distances <- array(dim = c(length(dataset[,1])))
  for (i in 1:length(distances)) {
    distances[i] <- dist(point[,3:4], dataset[i,3:4])
  }
  
  sortedDataset <- dataset[order(distances),]
  distances <- sort(distances)
  scores <- array(0L, length(unique(dataset$Species)))
  names(scores) = unique(dataset$Species)
  for (i in 1:k) {
    scores[sortedDataset$Species[i]] = scores[sortedDataset$Species[i]] + weight(i, distances[i])
  }

  return (names(which.max(scores)))
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
      distance[j] <- euclidean(dataset[j, 3:4], iris[i, 3:4])
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

m <- fast_cvloo_kwnn()

# Library
library(ggplot2)

qs <- seq(from = 0.0, to = 1.0, by = 0.05)

data <- expand.grid(
  X = dim(m)[1] %>% seq, 
  Y = qs)
data$Z <- m %>% as.vector()

mx <- which(max(m) == m, arr.ind = T)
mx[,"col"] <- mx[,"col"] / 21
print(mx)
ggplot(data, aes(X, Y, fill = Z)) +
  geom_tile() +
  geom_rect(fill = "white", data = data.frame(start = mx[,"row"], end = mx[,"row"] + 1), inherit.aes = FALSE, aes(xmin = mx[,"row"], xmax = mx[,"row"] + 1, ymin=mx[,"col"], ymax=mx[,"col"] + 0.05))