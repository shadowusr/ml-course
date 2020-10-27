# 4 - Selection of reference objects

### Assignment objective
- Generate a plot of margins utilizing arbitrary metric classifier.
- Get rid of noise in the dataset with use of the "steepest descent" algorithm.
- Implement the STOLP algorithm
- Compare the quality and speed of the classification algorithm before and after applying STOLP to the dataset.

### Assignment implementation
###### Margins
The margin of an object relative to some dataset and classification algorithm denotes how typical this object is. The larger the value, the more usual the sample is. To compute the margin, the following formulae can be used:
<a href="https://www.codecogs.com/eqnedit.php?latex=M(x_i)=W_{y_i}(x_i,&space;X^l)-\max_{y\in&space;Y\backslash\{y_i\}}W_y(x_i,&space;X^l)." target="_blank"><img src="https://latex.codecogs.com/gif.latex?M(x_i)=W_{y_i}(x_i,&space;X^l)-\max_{y\in&space;Y\backslash\{y_i\}}W_y(x_i,&space;X^l)." title="M(x_i)=W_{y_i}(x_i, X^l)-\max_{y\in Y\backslash\{y_i\}}W_y(x_i, X^l)." /></a>
That is, margin is a difference between the weight of an actual class and max weight among all other classes, except the actual one. Hence, negative margin means that classifiction algortihm makes an error on this object.

Below is a margins plot for the iris dataset. Dots are colored from green to red, where green is largest positive margin and red - lowest negative.
 ![margins](https://i.imgur.com/QiDsbqY.png)
 
 ###### The steepest descend algorithm for noise elimination
 The key idea of this algorithm can be described as follows. Let's select all object which have negative margin and sort them. Now among these sorted objects we can find such pair that the absolute difference between margins will be the largest. We will consider this pair as a boundary and reject all objects that go before it. Here's the code:
 ```R
removeNoise <- function(dset, margins) { # returns indices of entries that we should get rid of
  negativeMarginIndices <- which(margins < 0)
  negativeMargins <- margins[negativeMarginIndices]
  negativeMargins <- sort(negativeMargins)
  
  dsetLen <- length(margins)
  diff <- abs(negativeMargins[1:(dsetLen - 1)] - negativeMargins[2:dsetLen])
  
  boundary <- negativeMargins[which.max(diff) + 1]
  
  return (which(margins < boundary))
} 
```
###### STOLP algorithm
There are several types of objects in the dataset:
- Reference objects, which are typical class representatives.
- Uninformative, which almost don't affect the classification result.
- Outliers, which make classification results worse.

The STOLP algorithm helps us to select reference objects out of the dataset, leaving all other types aside. After applying the STOLP algorithm, the overall quality and execution time may improve and the dataset size will drastically decrease. Hence, STOLP is a sort of data compression algorithm.

Algorithm:
1. Get rid of the noise (see previous section).
2. Pick one object with the largest positive margin from each class and add it to the result.
3. While we aren't satisfied with accuracy:
    1. Compute margins for each object in the initial dataset relative to the resulting dataset.
    2. Pick the object with the lowest negative margin and add it to the resulting dataset.
    
That's virtually it. Here's the code:
```R
stolp <- function(dset, labels, maxErrors) {
  margins <- computeMargins(dset, labels)
  
  # selecting one element with max margin from each class
  result <- data.frame()
  resultLabels <- c()
  indexesToExclude = removeNoise(dset, margins)
  for (label in unique(labels)) {
    margins <- computeMargins(dset, labels)
    currentBest <- -1
    for (i in 1:length(margins)) {
      if (labels[i] == label && (currentBest == -1 || margins[currentBest] < margins[i])) {
        currentBest <- i
      }
    }
    result <- rbind(result, dset[currentBest,])
    resultLabels <- c(resultLabels, paste(labels[currentBest]))
    
    indexesToExclude <- c(indexesToExclude, currentBest)
  }
  
  n = length(labels)
  # iteratively reducing the error rate
  while (TRUE) {
    minIndex <- 1
    minValue <- 0
    negativeCount <- 0
    for (i in 1:n) {
      scores <- parzen(result, resultLabels, dset[i,], 1.6, gaussKernel)      
      margin <- scores[paste(labels[i])] - max(scores[names(scores) != paste(labels[i])])
      
      if (margin < 0) {
        negativeCount <- negativeCount + 1
      }
      if (!(i %in% indexesToExclude) && margin < minValue) {
        minValue <- margin
        minIndex <- i
      }
    }
    
    if (negativeCount <= maxErrors) break
    
    indexesToExclude <- c(indexesToExclude, minIndex)
    result <- rbind(result, dset[minIndex,])
    resultLabels <- c(resultLabels, paste(labels[minIndex]))
  }
  
  return (list("dataset" = result, "labels" = resultLabels))
}
```

We are using **parzen windows** classification algorithm with gaussian kernel and `h = 1.6`. 

| ![](https://i.imgur.com/qS8M7SB.png) | ![](https://i.imgur.com/tDpI8TR.png) |
| :-: | :-: |
| STOLP without noise reduction. Errors = 5 | STOLP with noise reduction. Errors = 6 |
| ![](https://i.imgur.com/yv02xUy.png) | ![](https://i.imgur.com/kIkq1wZ.png) |
| STOLP without noise reduction map. | STOLP with noise reduction map. |

###### Before and after applying STOLP comparison table
| Dataset | Errors | Dataset size | Time |
| - | - | - | - |
| Source dataset | 6 | 150 | 12.038s |
| STOLP without noise reduction | 5 | 12 | 1.246s |
| STOLP with noise reduction | 6 | 3 | 0.241s |