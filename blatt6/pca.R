#(a)
#load('pca.rda')
#data(iris)

#(b)
plot_lfd= function(x, subset=(1:length(x)-1),heading=""){
  k=ncol(x)
  nrOfLabels=length(levels(x[[k]]))
  plot(x[subset], col=(1:nrOfLabels)[x[[k]]],main =heading)
}

#(c)
plot_lfd(iris,subset = 1:4)
plot_lfd(iris,subset = 2:4)
plot_lfd(iris,subset = 3:4)
plot_lfd(iris,subset = 4:4)

#(d)
PCA=function (x, train=x, n, center=TRUE, scale=TRUE){
  k=length(x)
  if(n<1 | n>=k){
    print("wahl von n nochmal überdenken")
    return(NA)
  }
  
  if(center){
    for (i in 1:(k-1)){
      x[i]=x[[i]]-mean(x[[i]])
    }
  }
  datamatrix=data.matrix(x[1:(k-1)])
  covarianzmatrix=cov(data.matrix(train[1:(k-1)]))
  eig=eigen(covarianzmatrix)
  D=diag(k-1)
  if(scale){
    D=diag(eig[[1]])
  }
  gedrehtematrix=datamatrix%*%eig[[2]]%*%D
  
  output=x[1:(n+1)]
  for (i in 1:n){
    output[i]=gedrehtematrix[,i]
  }
  output[(n+1)]=x[k]
  return(output)
}

#(e)
par (mfrow = c(2,2))
plot_lfd (PCA (iris, n=2),heading = "skaliert und zentriert")
plot_lfd (PCA (iris, n=2,center = FALSE),heading = "skalier")
plot_lfd (PCA (iris, n=2,scale = FALSE),heading = "zentriert")
plot_lfd (PCA (iris, n=2,center = FALSE,scale = FALSE),heading = "nichts von beiden")

#(f)
attach(iris)
par (mfrow = c(2,2))
plot_lfd (PCA (iris,train= iris, n=2),heading = "iris mit sich selbst trainiert")
plot_lfd (PCA (iris,train = iris [Species=='setosa',], n=2),heading = "iris mit setosas trainiert")
plot_lfd (PCA (iris [Species=='setosa',],train = iris, n=2),heading = "setosas mit iris trainiert")
plot_lfd (PCA (iris [Species=='setosa',],train = iris [Species=='setosa',], n=2),heading = "setosas mit sich selbst trainiert")
detach(iris)

#(g)
daten=list(ADIDAS,diabetes,dna,FIAT,heart,iris,vehicle,watermark)
for (datensatz in daten) {
  l=length(datensatz)-1
  par (mfrow = c(2,2))
  plot_lfd(datensatz,subset = 1:2,heading = "ersten 2 , original")
  plot_lfd(datensatz,subset = (l-1):l,heading = "letzten 2 , original")
  plot_lfd (PCA (datensatz,n=l),subset = 1:2,heading = "ersten 2 , PCA")
  plot_lfd (PCA (datensatz,n=l),subset = (l-1):l,heading = "letzten 2 , PCA")
}
