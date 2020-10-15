# 3 - Potential functions method

### Assignment objective
- Implement the potential functions method with fixed window width.
- Make a plot showcasing peculiarities of the method (for instance, make objects with higher potential larger and brighter).
- For each kernel type, generate a classification map and estimate accuracy.
- Compare quality of the considered algorithms.

### Assignment implementation
You can think of this approach as of a generalized parzen window method. Here, instead of looking around the object we want to classify, we look around each point in the dataset. This fact itself changes nothing since we consider the distance function to be symmetric.
 
However, such approach enables us to tune the algorithm to our needs by introducing two parameters for each point in the dataset: window width (determines how far the potential of this point spread) and strength or potential (determines how strong the effect of this point is).

For now we keep window width the same across all points in the dataset.

###### Algorithm
1. Find distances between the object to classify and all other objects in the dataset.
2. Find a weight for each point in the dataset using the formulae <a href="https://www.codecogs.com/eqnedit.php?latex=\gamma_i&space;K\left(\frac{\rho(u,&space;x_i)}{h_i}\right)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\gamma_i&space;K\left(\frac{\rho(u,&space;x_i)}{h_i}\right)" title="\gamma_i K\left(\frac{\rho(u, x_i)}{h_i}\right)" /></a>.
3. Sum up weights for each class.
4. The class with the biggest score wins.

The only thing we haven't discussed yet is how we're going to find the potentials vector. It's a multi-step process:
1. Initialize the potentials vector `p` with zeros.
2. While errors count isn't small enough:
    1. Take a random object from the dataset.
    2. If the algorithm makes a mistake on this object, increase its potential by one.
    
###### Code
```R
euclidean <- function(a, b) sqrt(sum((a - b) ^ 2))

rectKernel <- function(dist) 0.5 * (abs(dist) <= 1)
trKernel <- function(dist) (1 - abs(dist)) * (abs(dist) <= 1)
epKernel = function(dist) (3 / 4) * (1 - dist ^ 2) * (abs(dist) <= 1)
gaussKernel = function(dist) dnorm(dist) * (abs(dist) <= 1)

classify <- function (dset, point, labels, potentials, h, dist = euclidean, kernel = trKernel) {
  distances <- apply(dset, 1, dist, point)
  
  weights <- potentials * kernel(distances / h)
  names(weights) <- labels
  
  classes <- unique(labels)
  classScores <- sapply(classes, function(class, w) sum(w[names(w) == class]), weights)
  
  if (length(unique(classScores)) == 1) {
    return(classes[sample(1:length(classes), 1)])
  }
  
  return (classes[which.max(classScores)])
}

computePotentials <- function (dset, labels, h, maxError) {
  dsetLength <- dim(dset)[1]
  potentials <- integer(dsetLength)
  
  errorCount <- maxError + 1
  while (errorCount > maxError) {
    repeat {
      randIndex <- sample(1:dsetLength, 1)
      tmpResult <- classify(dset, dset[randIndex,], labels, potentials, h)
      
      cat("\rSearching...", randIndex, labels[randIndex], tmpResult)
      
      if (tmpResult != paste(labels[randIndex])) {
        potentials[randIndex] <- potentials[randIndex] + 1
        break
      }
    }
    cat("\n")
    
    errorCount <- 0
    for (i in 1:dsetLength) {
      cat("\rProcessing", i, "of", dsetLength)
      tmpResult <- classify(dset, dset[i,], labels, potentials, h, kernel = epKernel)
      
      if (tmpResult != paste(labels[i])) {
        errorCount <- errorCount + 1
      }
    }
    cat("\n", errorCount, potentials, "\n")
  }
  
  return (potentials)
}
```

###### Demonstration of the method specifics
![pt-demo](https://i.imgur.com/dpsmwNq.png)

###### Classification maps
| ![](https://i.imgur.com/IWErRSE.png) | ![](https://i.imgur.com/oIqiZr8.png) |
| - | - |
| ![](https://i.imgur.com/84UOLUI.png) | ![](https://i.imgur.com/1v37lo2.png) |

###### Algorithms comparison
The comparison table is available on a [separate page](../metric-algorithms-comparison.md).