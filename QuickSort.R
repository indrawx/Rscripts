# vstupem funkce je neseøazený vektor èísel, výstupem vektor seøazený
# funkce rozdìlí vstupní vektor na pøibližnì dvì stejné èásti (èísla vìtší a menší - než zvolená hodnota (pivot))
# èásti se pak postupnì (rekurzivnì) øadí a zmenšují

QuickSort <- function(vstupni_vektor) {
  
  leva_strana <- c()
  prava_strana <- c()
  
  hodnota_stred <- sample(seq_along(vstupni_vektor), 1); stred <- vstupni_vektor[hodnota_stred]
  vstupni_vektor <- vstupni_vektor[-hodnota_stred]
  
  leva_strana <- vstupni_vektor[which(vstupni_vektor <= stred)]
  prava_strana <- vstupni_vektor[which(vstupni_vektor > stred)]
  
  if (length(leva_strana) > 1) {
    leva_strana <- QuickSort(leva_strana) # rek.
  }
  
  if (length(prava_strana) > 1) {
    prava_strana <- QuickSort(prava_strana) # rek.
  }
  
  return(c(leva_strana, stred, prava_strana))
}