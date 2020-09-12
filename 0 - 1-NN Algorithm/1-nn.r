euclidean <- function(a, b) {
  return (sqrt(sum((a - b) ^ 2)))
}

# The oneNN function categorizes points based on the 1 nearest neighbour method.
# Returns: a vector of class names for corresponding points.
# Params:
#   dataset -- a data frame, must have exactly 2 columns.
#   points -- a list of points to categorize. Each point is a vector of length 2.
#   dist -- optional. A metric function to use in the 1-NN method. Defaults to the Euclidean distance.
oneNN <- function(dataset, points, dist = euclidean) {
  answer <- data.frame()
  
  for (i in 1:length(points[1,])) {
    currAns <- dataset[1,]
    
    
    for (j in 1:dim(dataset)[1]) {
      if (dist(currAns[3:4], points[,i]) > dist(dataset[j, 3:4], points[,i])) {
        currAns <- dataset[j,]
      }
    }
    answer <- rbind(answer, data.frame(currAns))
  }
  
  return (answer)
}

colors <- c("setosa" = "blue", "virginica" = "red", "versicolor" = "green")
plot(iris[, 3:4], bg = colors[iris$Species], pch=23)

z <- array(c(runif(5, 1, 7), runif(5, 0, 2.5)), dim = c(2, 5))

print(z[1,])
print(z[2,])

result <- oneNN(iris, z)
points(z[1,], z[2,], bg = colors[result$Species], pch = 22)