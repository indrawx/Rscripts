Eratosthen <- function(data){
  x <- read.table(data, header = F)$V1 
  x <- sort(x) 
  x <- x[-1]
  o <- 1 
  n <- length(x) 
  m <- length(x) 
  
  while (o <= sqrt(n)){ 
    f <- 2*x[o] 
    p <- o+1 
    
    while (p <= m){
      if (x[p] == f){
        f <- f + x[o] 
        x <- x[-1*p] 
        m <- length(x)} 
      else if (x[p] > f){ 
        f <- f + x[o] 
      }
      else { 
        p <- p + 1 
      }
    }
    o <- o + 1 
  }
  write.csv(x, file = 'vystupni_soubor.csv', row.names=FALSE, na="") 
  }

