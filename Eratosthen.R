# "Run" a poté zavolat fci v konzoli jako: Eratosthen('01_data.csv') 
# x je vstupem funkce, naètený z csv souboru 01_data.csv
# výstupem je zápis seøazených prvoèísel nalezených v 01_data.csv do nového souboru vystupni_soubor.csv

Eratosthen <- function(data){
  x <- read.table(data, header = F)$V1 # naètení dat do promìnné x
  x <- sort(x) # seøazení èísel od nejmenšího po nejvìtší
  x <- x[-1] # odstranìní 1 z vektoru x (není prvoèíslo)
  o <- 1 
  n <- length(x) # n je délka vektoru x
  m <- length(x) # m je délka vektoru x
  
  while (o <= sqrt(n)){ # cyklus probíhá až k odmocninì z délky vstupního vektoru
    f <- 2*x[o] # f je hledaná pozice
    p <- o+1 # p je pozice, ke které je pøièten poèáteèní index o, navyšován v dalších cyklech o 1
    
    while (p <= m){
      if (x[p] == f){ # pokud pozice èísla na vstupu odpovídá hledané...
        f <- f + x[o] # k vektoru hledaných pozic se pøipíše indexace vstupního vektoru
        x <- x[-1*p] # pøepis vstupního vektoru na výstupní
        m <- length(x)} # délka vst. vektoru
      else if (x[p] > f){ 
        f <- f + x[o] # skok na další možnou hledanou pozici
      }
      else { 
        p <- p + 1 # ochrana
      }
    }
    o <- o + 1 # navýšení indexu o
  }
  write.csv(x, file = 'vystupni_soubor.csv', row.names=FALSE, na="") # zapsání dat do nového souboru
  }

