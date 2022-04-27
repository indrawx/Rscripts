# Vigenérova šifra má dva vstupy, oba textového charakteru. Prvním vstupem je text, který chceme kódovat.
# Druhým vstupem je text, který slouží jako klíè ke kodóvaní.

Vigenere <- function(text, klic){
text <- tolower(text)
klic <- tolower(klic)

delka_1 <- nchar(text)
delka_2 <- nchar(klic)

slovo_1 <- strsplit(text, '')
slovo_2 <- strsplit(klic, '')

slovo_11 <- c()
slovo_22 <- c()

for (i in 1:delka_1){
  slovo_11 <- c(slovo_11, slovo_1[[1]][i])
}
for (j in 1:delka_2){
  slovo_22 <- c(slovo_22, slovo_2[[1]][j])
}

if (delka_1 > delka_2){
  for (i in (delka_2+1):delka_1){
    slovo_22[i] <- slovo_22[i-delka_2]
  }
}

if (delka_2 > delka_1){
  for (i in (delka_1+1):delka_2){
    slovo_11[i] <- slovo_11[i-delka_1]
  }
}

tabulka <- matrix(,27,27)
radek_1_sloupec_1 <- c('0','a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k','l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v','w', 'x', 'y', 'z')
tabulka[1,1:27] <- radek_1_sloupec_1 
tabulka[1:27,1] <- radek_1_sloupec_1 
abeceda <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k','l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v','w', 'x', 'y', 'z')

for (i in 2:27) {
  tabulka[i,2:27] <- abeceda
  temp<-abeceda[1]
  abeceda<- setdiff(abeceda,temp)
  abeceda[26]<-temp
}

text_sifry_temp <- c()

for (i in 1:delka_1){
    for (k in 1:27){
      if (tabulka[1,k] == slovo_11[i]){
        index_sloupce <- k
        index_sloupce <- as.numeric(index_sloupce)
        for (l in 1:27){
          if (tabulka[l,1] == slovo_22[i]){
            index_radku <- l
            index_radku <- as.numeric(index_radku)
            text_sifry_temp <- c(text_sifry_temp, tabulka[index_radku,index_sloupce])
        }
      }
    }   
  }
}

text_sifry <- paste(text_sifry_temp, collapse = '')
return(text_sifry)

}
