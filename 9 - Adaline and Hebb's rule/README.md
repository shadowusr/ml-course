# 9 - Adaline and Hebb's rule. Stochastic gradient descent
### Assignment objective
- Implement stochastic gradient descent method for arbitrary loss function
- Implement the linear classification algorithm ADALINE
- Imlement the linear classifier with use of Hebb's rule and stochastic gradient descent
- Make the plots depicting these approaches in action
- Compare the two methods

### Assignment implementation
###### Gradient descent
The gradient descent technique is a powerful way of optimising stuff. Specifically, let's introduce the concept of a loss function: it's a function that takes all points from our dataset and our predicted values and returns some numeric characteristic of how bad our prediction was.

Obviously, we want the loss to be the least possible, ideally zero. In some simple cases we can just take a derivative of a loss function, solve the equation and find the most optimal weights set right away. However, it's not always possible. That's when gradient descent comes in handy.
 
Instead of solving the equation we launch an iterative process, at each step optimising our weights further by calculating the loss function gradient and making step towards the most optimal point. The step size depends on the gradient and learning rate - a special number that usually changes over iterations. We end this process once the step becomes small enough or we've reached the limit of steps.

###### Stochastic gradient descent
Remember the loss function from the previous section? Well, at each step we had to plug our whole dataset in it. Now imagine we have some really huge dataset and it takes us a lot of steps to reach the optimal point. That's when the regular gradient descent becomes too slow.

The approach can be sped up if instead of considering the whole dataset on each step, we took only a single random point and calculated the loss based on it. That's exactly what stochastic gradient descent does.

###### Algorithm
1. Initialize weights with some random small numbers.
2. Calculate the initial loss.
3. Until the step size is not too small and the steps limit is not reached, do:
    1. Pick a random sample from the dataset.
    2. Calculate the algorithm loss.
    3. Update the weights.
    4. Calculate the current loss.
    
###### Code
```R
computeWeightsWithStochasticGradient <- function(dset, classes, lossFunction, updateRule) {
  dsetLength <- dim(dset)[1]
  featuresCount <- dim(dset)[2]
  
  w <- runif(featuresCount, -1 / (2 * featuresCount), 1 / (2 * featuresCount))
  lambda <- 1 / dsetLength
  
  currentLoss <- 0
  for (i in 1:dsetLength) {
    margin <- sum(w * dset[i, ]) * classes[i]
    currentLoss <- currentLoss + lossFunction(margin)
  }
  prevLoss <- currentLoss
  
  step <- 1
  repeat {
    margins <- array(0, dim = dsetLength)
    for (i in 1:dsetLength) {
      margins[i] <- sum(w * dset[i, ]) * classes[i]
    }
    errorIndices <- which(margins <= 0)
    
    if (length(errorIndices) == 0) break
    
    randomErrorSampleIndex <- sample(errorIndices, 1)
    x <- dset[randomErrorSampleIndex, ]
    class <- classes[randomErrorSampleIndex]
    
    margin <- sum(w * x) * class
    loss <- lossFunction(margin)
    
    learningRate <- 1 / step
    w <- updateRule(w, learningRate, x, class)
    
    currentLoss <- (1 - lambda) * currentLoss + lambda * loss
    
    if (abs(prevLoss - currentLoss) / abs(max(prevLoss, currentLoss)) < 1e-5) break
    
    if (step > 25000) break
    
    prevLoss <- currentLoss
    step <- step + 1
  }

  return(w)
}
```

###### Adaline
Adaline is a single layer neural network. It has multiple noes, each node accepts multiple inputs and generates one output. Essentially we have a set of weights and to train/adjust them we can use SGD, using the following loss function and update rule:
```R
adalineLossFunction <- function(margin) (margin - 1) ^ 2

adalineUpdateRule <- function(w, learningRate, sample, class) w - learningRate * (sum(w * sample) - class) * sample 
```

![](https://i.imgur.com/YT59Wka.png)

###### Hebb's rule / perceptron
The perceptron is an algorithm for supervised learning of binary classifiers. Can be trained using SGD. The main differences from the adaline algorithm are the loss function and the Hebb's update rule:
```R
lossFunction <- function(margin) max(-margin, 0)

hebbUpdateRule <- function(w, learningRate, sample, class) w + learningRate * sample * class
```

![](https://i.imgur.com/VqA1ikN.png)

###### Algorithms comparison
| Algorithm | Average steps count (based on 100 launches) | Average error (based on 100 launches) |
| - | - | - |
| Adaline | 2996.93 | 11.36 of 500 (2.272%) |
| Perceptron / Hebb's rule | 3058.99 | 8.92 of 500 (1.784%) |

Adaline:
| ![](https://i.imgur.com/roz6IeZ.png) | ![](https://i.imgur.com/Y2jWaOs.png) | ![](https://i.imgur.com/so9tKPx.png) | ![](https://i.imgur.com/3I0wFUZ.png) |
| - | - | - | - |
| Step: 10 | Step: 50 | Step: 100 | Step: 500 |

Perceptron / Hebb's rule:
| ![](https://i.imgur.com/5UwpYHh.png) | ![](https://i.imgur.com/e80JSo9.png) | ![](https://i.imgur.com/Av632t6.png) | ![](https://i.imgur.com/fOu62jG.png) |
| - | - | - | - |
| Step: 10 | Step: 50 | Step: 100 | Step: 500 |