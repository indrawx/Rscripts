PolymerReact <- function(pol) {
  delka <- nchar(pol)
  vektor2 <- strsplit(pol, '')
  vektor <- c()
  
  for (i in 1:delka){
    vektor <- c(vektor, vektor2[[1]][i])
  }
  
  novy_vektor <- c()
  for (i in 1:(delka-1)){
    if (vektor[i] != vektor[i+1]){
      if (((toupper(vektor[i])) == (vektor[i+1]))){
        vektor[i] <- '0'
        vektor[i+1] <- '0'
        novy_vektor <- vektor[!vektor %in% 0]
        pol <- paste(novy_vektor, collapse = "")
        pol <- PolymerReact(pol)
      }
      else {
        i <- i+1
      }
    }
  }
  
  for (i in 1:(delka-1)){
    if (vektor[i] != vektor[i+1]){
      if (((tolower(vektor[i])) == (vektor[i+1]))){
        vektor[i] <- '0'
        vektor[i+1] <- '0'
        novy_vektor <- vektor[!vektor %in% 0]
        pol <- paste(novy_vektor, collapse = "")
        pol <- PolymerReact(pol)
      }
      else {
        i <- i+1
      }
    }
  }
  return(pol)
}


