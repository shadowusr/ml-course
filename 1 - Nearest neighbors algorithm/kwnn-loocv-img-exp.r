# library(rgl)

euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

w1 <- function(k, dist) {
  return (1 ^ k)
}

kwNN <- function(dataset, points, k, dist = euclidean, weight = w1) {
  answer <- array(dim = c(length(points[,1])))
  classes = unique(dataset$class)
  
  for (i in 1:length(points[,1])) {
    distance <- array(dim = c(length(dataset[,1])))
    for (j in 1:length(dataset[,1])) {
      distance[j] = dist(points[i, ], dataset[j, 1:2])
    }
    sortedDataset <- dataset[order(distance),]
    distance <- sort(distance)
    
    scores <- rep(x = 0, times = length(classes))
    names(scores) = classes
    for (j in 1:k) {
      scores[sortedDataset$class[j]] = scores[sortedDataset$class[j]] + weight(j, distance[j])
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

kwnn_loo <- function(k, dset) {
  
  classes = unique(dset$class)
  qs <- seq(from = 0.0, to = 1.0, by = 0.05)
  result <- array(0L, dim = c(length(qs)))
  for (i in 1:length(dset[,1])) {
    cat("\rProcessing sample", i, "of", length(dset[,1]))
    dataset <- dset[-i,]
    distance <- array(dim=c(length(dataset[,1])))
    for (j in 1:(length(dataset[,1]))) {
      distance[j] <- euclidean(dataset[j, 1:2], dset[i, 1:2])
    }
    sortedDataset <- dataset[order(distance),]
    distance <- sort(distance)
    
    classesCount <- array(0L, dim = c(length(classes)))
    dimnames(classesCount) = list(classes)
    
    classesCount <- array(0L, dim = c(length(classes), length(qs)))
    dimnames(classesCount) = list(classes)
    for (j in 1:k) { # k's
      index <- 1
      for (q in qs) {
        classesCount[sortedDataset$class[j],index] <- classesCount[sortedDataset$class[j],index] + (q ^ j)
        classForCurrentK = names(which.max(classesCount[,index]))[1]
        
        index <- index + 1
      }
    }
    for (j in 1:length(qs)) {
      ans = names(which.max(classesCount[,j]))[1]
      
      
      if (ans == paste(dset$class[i])) {
        result[j] <- result[j] + 1
      }
    }
  }
  cat("\n")
  
  for (i in 1:length(result)) {
    result[i] <- result[i] / length(dset[,1])
  }
  
  return (result)
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

# m <- fast_cvloo_kwnn()
k <- 6

qs <- (seq(from = 0.0, to = 1.0, by = 0.05))
acc = kwnn_loo(6, df)

par(mfrow=c(1,2), pty="s")

print(qs[which.max(acc)])

plot(qs, acc, type = "l", xlab = "q", ylab="Accuracy")
points(qs[which.max(acc)], max(acc), pch = 19, col = "red")

bestQ <- qs[which.max(acc)]

cat("Best accuracy:", max(acc), "for q =", qs[which.max(acc)])

colors <- c("class-1" = "blue", "class-2" = "red")
plot(df[, 1:2], bg = colors[paste(df$class)], pch=23, asp=1)

result <- kwNN(df, z, 5, euclidean, function(k, dist) { return(bestQ ^ k) })
points(z[,1], z[,2], bg = colors[result], pch = 22)