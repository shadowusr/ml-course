euclidean <- function(a, b) sqrt(sum((a - b) ^ 2))

rectKernel <- function(dist) 0.5 * (abs(dist) <= 1)
trKernel <- function(dist) (1 - abs(dist)) * (abs(dist) <= 1)
epKernel = function(dist) (3 / 4) * (1 - dist ^ 2) * (abs(dist) <= 1)
gaussKernel = function(dist) dnorm(dist) * (abs(dist) <= 1)

classify <- function (dset, point, labels, potentials, h, dist = euclidean, kernel = trKernel) {
  #cat(length(labels), dim(dset))
  distances <- apply(dset, 1, dist, point)
  #print(length(distances))
  
  weights <- potentials * kernel(distances / h)
  #print(length(weights))
  #print(potentials)
  names(weights) <- labels
  
  # print(potentials)
  # print(kernel(distances / h))
  # print(weights)
  
  classes <- unique(labels)
  classScores <- sapply(classes, function(class, w) sum(w[names(w) == class]), weights)
  classScores["undefined"] <- 0.000001
  
  #print(classScores)
  
  # if (length(unique(classScores)) == 1) {
  #   return(classes[sample(1:length(classes), 1)])
  # }
  
  #cat("CL RES", classes[which.max(classScores)], classScores, "\n")
  return (names(which.max(classScores)))
}

generateMaps <- function() {
  xs <- seq(from = 0.5, to = 7.5, by = 0.1)
  ys <- seq(from = -1, to = 3.5, by = 0.1)
  
  colors <- c("setosa" = "blue", "virginica" = "red", "versicolor" = "green", "undefined" = "grey")
  
  pp <- matrix(c(
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  ), nrow = 4)
  # print(pp)
  kernels <- c("rectangular" = rectKernel, "triangular" = trKernel, "epachinnikov's" = epKernel, "gaussian" = gaussKernel)
  
  # par(mfrow = c(length(kernels) / 2, 2), pty="s", mar=c(1,1,1,1))
  
  index <- 1
  for (kernelName in names(kernels)) {
    kernel <- kernels[[kernelName]]
    png(sprintf("plot-%03d.png", index), width = 1024, height = 700, unit = "px", pointsize = 20)
    cat("Kernel:", kernelName, index, "of", length(kernels), "\n")
    plot(iris[, 3:4], col = colors[paste(iris$Species)], pch=16, asp=1)
    title(main = paste("Kernel:", kernelName))
    
    h <- rep(1, 150)
    #print(pp)
    p <- pp[index,]
    #potentials <- potentials(Util.petals, Util.classes, h = h, 5, kernel)
    
    all <- length(xs) * length(ys)
    progress = 1
    for (i in xs) {
      for (j in ys) {
        cat("\rProcessing point", progress, "of", all)
        progress <- progress + 1
        result <- classify(iris[3:4], c(i, j), paste(iris$Species), p, h, kernel = kernel)
        #print(paste(i, j, result))
        points(i, j, col = colors[result], pch = 21)
      }
    }
    cat("\rDone.                      \n")
    dev.off()
    index <- index + 1
  }
}

generateMaps()


