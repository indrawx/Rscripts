Calendar <- function(datum){
datum <- strsplit(datum, "[.]")

q <- datum[[1]][1]
q <- as.numeric(q)

m <- datum[[1]][2]
m <- as.numeric(m)

K <- strsplit(datum[[1]][3],'')
K2 <- K #uložení roku do nové proměnné pro řešení přestupného roku (níže)

#PRO LEDEN A ÚNOR - V ALGORITMU: ROK = ROK-1 (dále návrat proměnné K do typu 'list')
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

#ZÍSKÁNÍ STOLETÍ A ROKU V NĚM
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

#PŘESTUPNÝ ROK - ŘEŠENÍ
K2 <- K2[[1]]
K2 <- paste(K2, collapse = "")
K2 <- as.numeric(K2)

if (K2/4 != round(K2/4)){
  PR <- FALSE
} else {
  PR <- TRUE
}

#KOREKCE! roky dělitelné 100 jsou přestupné jen tehdy, jsou-li dělitelné 400
neni_pr <- seq(from = 100, to = 10000, by = 100)
odstran <- seq(from = 400, to = 10000, by = 400)
neni_pr <- neni_pr[!neni_pr %in% odstran]

for (i in 1:length(neni_pr)){
  if (K2 == neni_pr[i]){
    PR <- FALSE
  }
}

#OŠETŘENÍ LEDEN/ÚNOR - pro zpracování v Zell. alg.
if (m == 1){
  m <- 13
} else if (m == 2){
  m <- 14
} else if (m > 12){
  print('CHYBNĚ ZADANÉ DATUM')
  break
}

#OŠETŘENÍ DNE
if (q > 31){
  print('CHYBNĚ ZADANÉ DATUM')
  break
}

#ZELLERŮV ALGORITMUS (s úpravou)
h <- (q + floor((13*(m+1))/5) + k + floor(k/4) + J/4 + 5*J)%%7
d <- ((h+5)%%7)+1
d <- floor(d)

#PŘEVOD NA DEN
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
#  print('Přestupný rok!')
#} else {
#  print('Nepřestupný rok!')
#}

#VÝSTUP
vystup <- list()
vystup[[1]] <- den
vystup[[2]] <- PR

return(vystup)

}
