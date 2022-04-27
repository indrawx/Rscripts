MergeSort <- function(vstupni_vektor) {
  # vstupem funkce je neseøazený vektor èísel
  # funkce vektor seøadí a výstupem je seøazený vektor vzestupnì
  # promìnné "vek" oznaèují vektory èísel, se kterými funkce pracuje
  # poc = poèítadlo
  
delka_vek <- length(vstupni_vektor)
if (delka_vek < 2) return(vstupni_vektor)
 vek1 <- vstupni_vektor[1:(delka_vek %/% 2)]
 vek2 <- vstupni_vektor[(delka_vek %/% 2 + 1):delka_vek]
  vek1 <- MergeSort(vek1) # rek.
  vek2 <- MergeSort(vek2) # rek.
  vystup <- c()
  poc <- 0
  while (length(vek1) > 0 && length(vek2) > 0) {
     if (vek1[1] < vek2[1]) {
     vystup[poc + 1] <- vek1[1]
     vek1 <- vek1[-1]
     } else {
     vystup[poc + 1] <- vek2[1]
     vek2 <- vek2[-1]
 } 
    poc <- poc + 1
 } 
  vystup <- c(vystup, vek1, vek2)
   vystup
} 
MergeSort(vstupni_vektor)