# 2
library(class)
?knn
?knn.cv

# a)
knn.heldout <- function(train, test, k=1) {
  knn(train, test, k=1) -> v
  
}