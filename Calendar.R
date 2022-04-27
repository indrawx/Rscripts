Calendar <- function(datum){

# vstupem funkce Calendar je datum ve formátu DD.MM.YYYY /jako textový øetìzec)
# výstupem funkce je promìnná 'list', v nìmž je den vstupního data (anglicky) a hodnota TRUE/FALSE, která urèí, zda-li se jedná o pøestupný rok nebo ne
# vysvìtlivky k promìnným jsou uvedeny níže
# funkce využívá Zellerova algoritmu
# ve funkci je ošetøena i korekce Gregoriánského kalendáøe uvedena níže:

# Uvedenou korekci pøinesl gregoriánský kalendáø, který stanovil, že roky dìlitelné 100 jsou pøestupné jenom tehdy, jsou-li dìlitelné také 400. Pøestupnými roky jsou proto napøíklad roky 1600, 2000, 2400 apod., zatímco roky 1700, 1800, 1900, 2100 atd. pøestupné nejsou.

  
# PROMÌNNÉ
# K = rok (minus 1 pro leden nebo únor!)
# k = 2 poslední èíslice z K
# J = 2 první èíslice z K
# q = den v mìsíci (1 až 31)
# m = mìsíc - leden-13, únor-14, bøezen-3, duben-4...
# d = den v týdnu (výsledek)
# PR = pøestupný rok (TRUE=ano, FALSE=ne)


datum <- strsplit(datum, "[.]")

q <- datum[[1]][1]
q <- as.numeric(q)

m <- datum[[1]][2]
m <- as.numeric(m)

K <- strsplit(datum[[1]][3],'')
K2 <- K #uložení roku do nové promìnné pro øešení pøestupného roku (níže)

#PRO LEDEN A ÚNOR - V ALGORITMU: ROK = ROK-1 (dále návrat promìnné K do typu 'list')
if (m == 1){
  K <- K[[1]]
  K <- paste(K, collapse = "")
  K <- as.numeric(K)
  K <- K-1
  K <- as.character(K)
  K <- strsplit(K,'') 
} else if (m == 2){
  K <- K[[1]]
  K <- paste(K, collapse = "")
  K <- as.numeric(K)
  K <- K-1
  K <- as.character(K)
  K <- strsplit(K,'')
}

#ZÍSKÁNÍ STOLETÍ A ROKU V NÌM
J <- c()
for (i in 1:2){
  J <- c(J, K[[1]][i])
}
J <- paste(J, collapse = '')
J <- as.numeric(J)

k <- c()
for (i in 3:4){
  k <- c(k, K[[1]][i])
}
k <- paste(k, collapse = '')
k <- as.numeric(k)

#PØESTUPNÝ ROK - ØEŠENÍ
K2 <- K2[[1]]
K2 <- paste(K2, collapse = "")
K2 <- as.numeric(K2)

if (K2/4 != round(K2/4)){
  PR <- FALSE
} else {
  PR <- TRUE
}

#KOREKCE! roky dìlitelné 100 jsou pøestupné jen tehdy, jsou-li dìlitelné 400
neni_pr <- seq(from = 100, to = 10000, by = 100)
odstran <- seq(from = 400, to = 10000, by = 400)
neni_pr <- neni_pr[!neni_pr %in% odstran]

for (i in 1:length(neni_pr)){
  if (K2 == neni_pr[i]){
    PR <- FALSE
  }
}

#OŠETØENÍ LEDEN/ÚNOR - pro zpracování v Zell. alg.
if (m == 1){
  m <- 13
} else if (m == 2){
  m <- 14
} else if (m > 12){
  print('CHYBNÌ ZADANÉ DATUM')
  break
}

#OŠETØENÍ DNE
if (q > 31){
  print('CHYBNÌ ZADANÉ DATUM')
  break
}

#ZELLERÙV ALGORITMUS (s úpravou)
h <- (q + floor((13*(m+1))/5) + k + floor(k/4) + J/4 + 5*J)%%7
d <- ((h+5)%%7)+1
d <- floor(d)

#PØEVOD NA DEN
if (d == 1){
  den <- 'Monday'
} else if (d == 2){
  den <- 'Tuesday'
} else if (d == 3){
  den <- 'Wednesday'
} else if (d == 4){
  den <- 'Thursday'
} else if (d == 5){
  den <- 'Friday'
} else if (d == 6){
  den <- 'Saturday'
} else if (d == 7){
  den <- 'Sunday'
} 

#ZOBRAZENÍ - pokud by se nejednalo o funkci, ODKOMENTOVAT!
#print(den)

#if (PR == TRUE){
#  print('Pøestupný rok!')
#} else {
#  print('Nepøestupný rok!')
#}

#VÝSTUP
vystup <- list()
vystup[[1]] <- den
vystup[[2]] <- PR

return(vystup)

}