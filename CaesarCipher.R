CaesarCipher <- function(retezec, posunuti){
#ENKRYPCE
if (posunuti > 0){
abeceda <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k','l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v','w', 'x', 'y', 'z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k','l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v','w', 'x', 'y', 'z')

delka_retezce <- nchar(retezec)
retezec <- strsplit(retezec, '')
novy_retezec <- c()

for (i in 1:delka_retezce){
  novy_retezec <- c(novy_retezec, retezec[[1]][i])
}

kod <- c()
for (i in 1:delka_retezce){
  for (j in 1:26){
   if (novy_retezec[i] == abeceda[j]){
     kod <- c(kod, abeceda[j+posunuti])
   } 
  }
}

zobrazit <- paste(kod, collapse = "")
print(zobrazit)

}

#DEKRYPCE
if (posunuti < 0){
  posunuti <- abs(posunuti)
  abeceda <- c('z', 'y', 'x', 'w', 'v', 'u', 't', 's', 'r', 'q', 'p', 'o', 'n', 'm', 'l', 'k', 'j', 'i', 'h', 'g', 'f', 'e', 'd', 'c', 'b', 'a', 'z', 'y', 'x', 'w', 'v', 'u', 't', 's', 'r', 'q', 'p', 'o', 'n', 'm', 'l', 'k', 'j', 'i', 'h', 'g', 'f', 'e', 'd', 'c', 'b', 'a')
  
  delka_retezce <- nchar(retezec)
  retezec <- strsplit(retezec, '')
  novy_retezec <- c()
  
  for (i in 1:delka_retezce){
    novy_retezec <- c(novy_retezec, retezec[[1]][i])
  }
  
  kod <- c()
  for (i in 1:delka_retezce){
    for (j in 1:26){
      if (novy_retezec[i] == abeceda[j]){
        kod <- c(kod, abeceda[j+posunuti])
      } 
    }
  }
  
  zobrazit <- paste(kod, collapse = "")
  print(zobrazit)
  
  return(zobrazit) 
}
}


