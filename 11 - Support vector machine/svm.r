# mu1 <- c(0, 0)
# mu2 <- c(4, 4)
# sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
# sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
# dset1 <- mvrnorm(50, mu1, sig1)
# dset2 <- mvrnorm(50, mu2, sig2)
# dset <- rbind(cbind(dset1, 1),cbind(dset2, -1))
# dset <- cbind(dset, -1)

m <- matrix(data = c(
  0, 0,
  0.2, 0.1,
  -0.7, 0.2,
  -0.5, -0.3,
  0.4, -0.2,
  
  2, 2,
  -2, 2,
  2, -2,
  -2, -2,
  -3, 0,
  -5, 0,
  4, 1,
  3, -4
), ncol = 2, byrow = TRUE)
cl <- c(rep(1, 5), rep(-1, 8))

library(kernlab)

# svp <- ksvm(dset[,1:2],dset[,3],type="C-svc", kernel = "vanilladot")
svp <- ksvm(m,cl,type="C-svc")
plot(svp, data=m)
svp