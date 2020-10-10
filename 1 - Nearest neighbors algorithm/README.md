# 1 - Nearest neighbors algorithm

### Assignment objective

- Implement the k-NN algorithm
- Pick the optimal value k using leave-one-out cross validation and draw a corresponding plot
- Implement the weighted k-NN algorithm
- Pick the optimal values k and q utilizing LOO CV and draw a plot
- Compare accuracies of k-NN and weighted k-NN
- Build a classification maps
- Give an example proving the advantage of weighted k-NN over k-NN

### Assignment implementation
###### k-NN
k nearest neighbors algortihm implementation can be found in the `knn.r` file. A sample run of this program looks as follows.
![Result of a sample algorithm run](https://i.imgur.com/AOtvlBP.png)
The best K is 6. The plot on the left shows dependency between k and overall accuracy computed with use of leave-one-out cross validation. The diagram on the right exhibits practical method application: diamonds denote the dataset data, while squares were picked randomly and classified by the algorithm.

###### Weighted k-NN
The `kwnn-heatmap.r` showcases the algorithm implementation along with a heat map which shows a dependency between k, q and accuracy. Weight function used is q^i. The heat map is shown below.
![Result of a sample algorithm run](https://i.imgur.com/NDXGOUO.png)
Brighter color means better accuracy. White spots denote the best results. The best results were achieved with the following pairs of params: `K = 30, q = 0.952381`, `K = 32, q = 0.952381`, `K = 6, q = 1.000000`.

###### Classification maps
Classification maps for k-NN (k = 6) and weighted k-NN (k = 30, q = 0.952381), respectively:

| ![Result of a sample algorithm run](https://i.imgur.com/ngF5Nei.png) | ![Result of a sample algorithm run](https://i.imgur.com/KYUCOyU.png) |
| - | - |

###### Algorithms accuracy comparison
Code can be found in the `knn-vs-kwnn.r` file. Comparing these two algorithms on the iris dataset (features 1-4) revealed that the accuracies are identical (0.98).

###### Example of the advantage of weighted k-NN over k-NN
The main difference of the weighted k-NN algorithm from the plain k-NN is that the closer points are the more important they become. Because of this fact these algorithms can give different answers as depicted on the image below. The k-NN would put the grey dot in the blue class, but weighted k-NN would put it in the green one.

Now let's write a s1mple python script for recognition of this points and generating the R code of the dataset: [source file](recognition.py).

| ![Result of a sample algorithm run](https://i.imgur.com/cWwo992.png) | ![Result of a sample script run](https://i.imgur.com/1oqZYBa.png) |
| - | - |
| Image drawn in PS | Points recognized by our Python script |

Generated dataset in R:
```R
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

points = array(c(c(1164, 1232)), dim = c(1,2))```

Now we are running a LOO CV on this dataset to find the best k for this dataset:
![LOO CV](https://i.imgur.com/qtG4te2.png)

The kwnn algorithm, on the contrary, assigns the dot to the red class (k = 6, q = 0.05):
![LOO CV](https://i.imgur.com/bhfkdKk.png)

