# 2
library(class)
?knn
?knn.cv

# a)
knn.heldout <- function(train, test, k=1) {
  classTrain <- train[, "CLASS"]
  classTest  <- test[, "CLASS"]
  
  pred <- knn(train[,1:dim(train)[2]-1], test[,1:dim(test)[2]-1], cl = classTrain, k=k)
  return(1 - sum(pred == classTest) / length(classTest))
}

# b)
knn.leave1out <- function(data, k=1) {
  #sapply(data, function(x) knn.heldout(data[-x,],data[x,]), k=k)
  
  s <- 0
  for(i in 1:dim(data)[1]) {
    s <- s + knn.heldout(data[-i,], data[i,],k=k)
  }
  return(s / dim(data)[1])
}

# c)
#load(./path/to/file/diabetes.rda)
cast <- function(x) {
  x <- x*100
  x <- paste(as.character(round(x, digits=2)), "%", sep="")
  return(x)
}

K <- c(1,2,3,5,7,10,14,19,25)
m <- matrix(ncol = 9, nrow = 6)
j <- 1
a <- rbind(diabetes.test, diabetes.lern)
for(i in K) {
  m[1,j] <- paste("k=",i)
  m[2,j] <- cast(knn.heldout(diabetes.lern, diabetes.test, k=i)); 
  # d)
  m[3,j] <- cast(knn.heldout(diabetes.test, diabetes.lern, k=i));
  # e)
  m[4,j] <- cast(knn.leave1out(a, k=i))
  # f)
  m[5,j] <- cast(knn.leave1out(a, k=i))
  m[6,j] <- cast(knn.leave1out(a, k=i))
  
  j <- j+1 
}
m

# g)
#load(./path/to/file/letter.rda)
g <- integer(12)
for(e in 0:13) {
  g[e] <- knn.heldout(letter.lern[1:(2^(e)),], letter.test) 
}
g

# h)
total <- rbind(germany.lern, germany.test)
h <- integer(24)
h2 <- integer(24)
for(i in 1:dim(total)[2]) {
  h[i] <- knn.leave1out(total[,-i]) 
  h2[i] <- knn.heldout(germany.lern[,-i], germany.test[,-i]) 
}
h; h2

# i) (nur zur besseren Darstellung)
par(mfrow=c(2,1));
plot(h*100, main = "leave1out", xlab = "alle Merkmale außer i-tem", ylab = "Testfehler (%)" )
plot(h2*100, main = "heldout", xlab = "alle Merkmale außer i-tem", ylab = "Testfehler (%)" )


# 
#j <- integer(10)
#for(i in 1:10) {       #Berechnung dauert länger, daher ausgeklammert
#  all <- knn.leave1out(total)
#  best23 <- knn.leave1out(total[,-4][,-4]) # ohne schlechteste Merkmale Nr 4 und 5
#  j[i] <- all-best23
#}
#j