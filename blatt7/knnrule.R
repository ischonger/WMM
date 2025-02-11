# Ausf�hrung dauert lange!! => einzeln ausf�hren
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
m <- matrix(ncol = 9, nrow = 5)
j <- 1
a <- rbind(diabetes.test, diabetes.lern)
for(i in K) {
  m[1,j] <- cast(knn.heldout(diabetes.lern, diabetes.test, k=i)); 
  # d)
  m[2,j] <- cast(knn.heldout(diabetes.test, diabetes.lern, k=i));
  # e)
  m[3,j] <- cast(knn.leave1out(a, k=i))
  # f)
  m[4,j] <- cast(knn.leave1out(a, k=i))
  m[5,j] <- cast(knn.leave1out(a, k=i))
  
  j <- j+1 
}
colnames(m) <- c("k=1", "k=2", "k=3", "k=5", "k=7", "k=10", "k=14", "k=19", "k=25")
rownames(m) <- c("c)", "d)", "e", "f1)", "f2)")
m


# g)
#load(./path/to/file/letter.rda)
g <- matrix(ncol=14, nrow=2)
rownames(g) <- c("errors", "enumeration")
colnames(g) <- c("e=2^0", "e=2^1", "e=2^2", "e=2^3", "e=2^4", "e=2^5", "e=2^6", "e=2^7", "e=2^8", "e=2^9", "e=2^10", "e=2^11", "e=2^12", "e=2^13")
for(e in 1:14) {
  g[1,e] <- knn.heldout(letter.lern[1:(2^(e-1)),], letter.test) * 100
  g[2,e] <- e
}
g

# h)
total <- rbind(germany.lern, germany.test)
germanyErrors <- matrix(ncol = 24, nrow = 3)
colnames(germanyErrors) <- colnames(germanyErrors, do.NULL = FALSE, prefix = "fehlt ")
rownames(germanyErrors) <- c("1-nn", "cross-val", "1-nn/c-v")
for(i in 1:dim(total)[2]) {
  germanyErrors[1,i] <- knn.heldout(germany.lern[,-i], germany.test[,-i])
  germanyErrors[2,i] <- knn.leave1out(total[,-i]) 
  germanyErrors[3,i] <- germanyErrors[1,i] / germanyErrors[2,i] - 1
}
germanyErrors

# i) (nur zur besseren Darstellung)
par(mfrow=c(2,1));
plot(h*100, main = "leave1out", xlab = "alle Merkmale au�er i-tem", ylab = "Testfehler (%)" )
plot(h2*100, main = "heldout", xlab = "alle Merkmale au�er i-tem", ylab = "Testfehler (%)" )

#j <- integer(10)
#for(i in 1:10) {       #Berechnung dauert l�nger, daher ausgeklammert
#  all <- knn.leave1out(total)
#  best23 <- knn.leave1out(total[,-4][,-4]) # ohne schlechteste Merkmale Nr 4 und 5
#  j[i] <- all-best23
#}
#j

# Baue errors.rda
#cdef <- m
#h <- germanyErrors
#save(cdef, g, h, file = "errors.rda")