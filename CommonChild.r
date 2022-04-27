CommonChild <- function(slovo1, slovo2) {    
slovo1 <- strsplit(slovo1[1], "")
slovo2 <- strsplit(slovo2[1], "")

slovo1 <- strsplit(slovo1[[1]], "")
slovo2 <- strsplit(slovo2[[1]], "") 

slovo1 <- unlist(slovo1)
slovo2 <- unlist(slovo2)

delka1 <- length(slovo1)
delka2 <- length(slovo2)

  
mat <- matrix(0, delka1+1, delka2+1)

for (i in 2:(delka1+1)){
  for (j in 2:(delka2+1)){
    if(slovo1[i-1] == slovo2[j-1]){ 
      mat[i,j] <- mat[i-1,j-1]+1
    }
    else {
      mat[i,j] <- max(mat[i-1,j], mat[i,j-1]) 
    }
  }
}


index1 <- delka1+1 
index2 <- delka2+1
a <- mat[index1, index2]

vystup <- c()

while (a != 0) { 

  if (a == mat[index1-1, index2]) {
    index1 <- index1-1
  }
  else if (a == mat[index1,index2-1]) {
    index2 <- index2-1
  }
  else if (a == mat[index1-1, index2-1]+1) {
    index1 <- index1-1
    index2 <- index2-1
    
    a <- a-1
    
    vystup <- c(vystup, slovo1[index1])
  }
}

vystup <- rev(vystup)
vystup <- paste(vystup, collapse = "")

return(vystup)

}
