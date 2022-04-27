TimeToWords <- function(cas){
rozdeleny_cas <- strsplit(cas, ':')
hodiny <- rozdeleny_cas[[1]][1]
minuty <- rozdeleny_cas[[1]][2]
hodiny <- as.numeric(hodiny)
minuty <- as.numeric(minuty)
clock <- "o'clock"

hodinovy_vektor <- c(0:24)
names(hodinovy_vektor) <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twentyone", "twenty two", "twenty three", "zero") 
minutovy_vektor <- c(1:59)
names(minutovy_vektor) <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "quarter", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "twenty one", "twenty two", "twenty three", "twenty four", "twenty five", "twenty six", "twenty seven", "twenty eight", "twenty nine", "half", "twenty nine", "twenty eight", "twenty seven", "twenty six", "twenty five", "twenty four", "twenty three", "twenty two", "twenty one", "twenty", "nineteen", "eighteen", "seventeen", "sixteen", "quarter", "fourteen", "thirteen", "twelve", "eleven", "ten", "nine", "eight", "seven", "six", "five", "four", "three", "two", "one")

#PODMÍNKY K VYTVOØENÍ SLOV
if (minuty == 0){
      vystup <- c(names(hodinovy_vektor[hodiny+1]), clock)
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty == 1) {
      vystup <- c(names(minutovy_vektor)[minuty], "minute past", names(hodinovy_vektor)[hodiny+1])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty == 15) {
      vystup <- c(names(minutovy_vektor)[minuty], "past", names(hodinovy_vektor)[hodiny+1])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty < 15) {
      vystup <- c(names(minutovy_vektor)[minuty], "minutes past", names(hodinovy_vektor)[hodiny+1])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty == 30) {
      vystup <- c(names(minutovy_vektor)[minuty], "past", names(hodinovy_vektor)[hodiny+1])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty < 31) {
      vystup <- c(names(minutovy_vektor)[minuty], "minutes past", names(hodinovy_vektor)[hodiny+1])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty < 45) {
      vystup <- c(names(minutovy_vektor)[minuty], "minutes to", names(hodinovy_vektor)[hodiny+2])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty == 45) {
      vystup <- c(names(minutovy_vektor)[minuty], "to", names(hodinovy_vektor)[hodiny+2])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty < 59) {
      vystup <- c(names(minutovy_vektor)[minuty], "minutes to", names(hodinovy_vektor)[hodiny+2])
      zobrazit <- paste(vystup, collapse = " ")
} else if (minuty == 59) {
      vystup <- c(names(minutovy_vektor)[minuty], "minute to", names(hodinovy_vektor)[hodiny+2])
      zobrazit <- paste(vystup, collapse = " ")
}

if (hodiny > 23 || hodiny < 0){
  zobrazit <- ("Nesprávný údaj hodin!")
}
if (minuty > 60 || minuty <0){
  zobrazit <- ("Nesprávný údaj minut!")
}

print(zobrazit)
}
