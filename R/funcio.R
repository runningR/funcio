#'Simple addition
#'
#'This function makes a simple additive operation.
#' @param x is a numerical value
#' @export
add<-function(x) Reduce("+",x)


#'Order
#'
#'returns the ordered descending numerical vector.
#' @param x is a numerical vector
#' @export
order<-function(x){
  rdec<-vector()
  rcre<-vector()
  max<-as.numeric()
  while(length(x)!=1){
    max<-max(x)
    rdec<-c(rdec,max)
    rcre<-c(max,rcre)
    x<-x[x!=max]
  }
  rdec[length(rdec)+1]<-x
  rcre<-c(x,rcre)
  print(rdec)
}

#'Frequency
#'
#'returns the absolute frequency of the elements of a given vector x.
#' @param x is a numerical vector
#' @export
freq<-function(x){
  sort(x); g<-vector(); d<-vector()
  i<-2; j<-1; g[1]<-x[1]; d<-c(1);s<-0
  while(i<=length(x)){
    while(j<=length(g)){
      if(x[i]==g[j]){
        d[j]<-d[j]+1
        s<-s+1
      }
      j<-j+1
    }
    if(s==0){g[length(g)+1]<-x[i]; d[j]<-1}
    s<-0
    j<-1
    i<-i+1
  }
  for(k in 1:length(g)){cat("\n",g[k],"|",d[k])}
}
