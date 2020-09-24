# -1 - Fast CV LOO, the best K search

### Assignment objective
The goal of this task is to implement fast leave-one-out cross validation using iris dataset (features 1-4) and k-NN algorithm. Results should be presented in a plot. The best K should be found and displayed.

### Implementation description
Implementation utilizes the idea of per-sample computation, meaning that we are processing each sample for all possible K's at once, thus having exactly n sortings, where n is the number of samples in the dataset.

Turns out that the best K for this task is 19.
Results are presented on the image below.

![Result of a sample algorithm run](https://i.imgur.com/Rj6lqdu.png)
 
