# Aufg1
#filter2D <- load("filter2D.rda");
plot.array(Toni)
# a
translate <- function(x, dr, dc) {
  r <- nrow(x);
  c <- ncol(x);
  res <- matrix(nrow=r, ncol=c);
  
  #for(i in 1:r) res[i,] <- x[i+dr]
  #for(i in 1:c) res[,i] <- x[i+dc]
  #herausfinden, wie ich x und y einer Matrix direkt ansprechen kann (m[m.x+15,m.y+30])...
  x[x[x+dr,x+dc]] -> res
  
  return(res)
}
# Anwendung:
a <- function() {
  
}
plot.array(translate(Toni,15,30))