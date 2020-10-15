# 2 - Parzen window method

### Assignment objective

- Implement the parzen window method.
- For each kernel type, generate a classification map, estimate accuracy and find the optimal window width `h` utilizing LOO CV.
- Compare quality of the considered algorithms.

### Assignment implementation
Parzen window approach is one of the distance-based algorithms. Instead of dealing with ranks of neighbors as we did in the kwNN method, now we use distance as a primary factor of the object "importance". The logic is straightforward: the further an object is, the less it affects the final decision.

The exact relationship between the distance and the weight is defined by a special function, called kernel. A kernel is a non-increasing on [0, inf) function of the form <a href="https://www.codecogs.com/eqnedit.php?latex=K(\frac{\rho(u,&space;x_i)}{h})" target="_blank"><img src="https://latex.codecogs.com/gif.latex?K(\frac{\rho(u,&space;x_i)}{h})" title="K(\frac{\rho(u, x_i)}{h})" /></a>.

Here we review 5 kernels, specifically - triangle, rectangle, epachinnikov, gaussian and quadric kernels.

###### Algorithm
1. Find distances between the object to classify and all other objects in the dataset.
2. Iterate over all objects in the dataset and compute the kernel function value for each of them. Add this value to a corresponding class.
3. The class with the biggest score wins.

###### Code
```R
euclidean <- function(a, b) sqrt(sum((a - b) ^ 2))

rectKernel <- function(dist, h) if (dist <= h) 0.5 else 0

trKernel <- function(dist, h) if (dist <= h) (h - dist) / h else 0

epKernel <- function(dist, h) if (dist <= h) (1 - dist ^ 2) * 0.75 else 0

gaussKernel <- function(dist, h) exp(sqrt(h / pi * 0.5) * -0.5 * (dist * h) ^ 2)

kvKernel <-function(dist, h) if (dist <= h) 15 / 16 * (1 - (dist / h) ^ 2) ^ 2 else 0

parzen <- function(dset, labels, point, h, kernel, dist = euclidean) {
  dsetLength <- length(dset[,1])
  distances <- array(dim = c(dsetLength))
  
  uLabels <- unique(labels)
  
  classScores <- array(0L, dim = c(length(uLabels)))
  names(classScores) = uLabels;
  classScores["undefined"] <- 0.000001
  
  for (i in 1:dsetLength) {
    classScores[labels[i]] <- classScores[labels[i]] + kernel(dist(point, dset[i,]), h)
  }
  
  return(names(which.max(classScores)))
}
```

###### Classification maps
The rectangle kernel:
![rect](https://i.imgur.com/rkt5rFh.png)

The triangle kernel:
![tr](https://i.imgur.com/MQKmr2B.png)

The epachinnikov kernel:
![ep](https://i.imgur.com/bYL3bkE.png)

The gaussian kernel:
![gauss](https://i.imgur.com/HQ0SKRE.png)

The quadric kernel:
![kv](https://i.imgur.com/yNELftF.png)

###### Best h search with leave-one-out CV
The rectangle kernel (minErrors = 6):
![rect](https://i.imgur.com/z5rjf6U.png)

The triangle kernel (minErrors = 6):
![tr](https://i.imgur.com/S0IdT8G.png)

The epachinnikov kernel (minErrors = 6):
![ep](https://i.imgur.com/bo21doN.png)

The gaussian kernel (minErrors = 6):
![gauss](https://i.imgur.com/86glHX8.png)

The quadric kernel (minErrors = 6):
![kv](https://i.imgur.com/RIgWdBf.png)

###### Algorithms comparison
The comparison table is available on a [separate page](https://github.com/shadowusr/ml-course/blob/master/metric-algorithms-comparison.md).