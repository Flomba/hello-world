#####################################################
# ispirato alle lezioni del corso HarvardX: PH525.1x
#####################################################

# osservo una differenza tra controlli e trattati.
# per quanto mi faccia piacere osservare questa differenza
# devo sempre mettermi al riparo dal fatto che tale differenza possa
# essere dovuta al caso
# E' raro avere accesso a popolazioni controllo grandi ma 
# supponiamo che questo sia il nostro caso

# read the dataset della popolazione controllo in "dat" as dataframe
n <- 10000
nulls <- vector ("numeric", n) #questo serve a creare un vettore vuoto

# adesso simulo delle osservazioni prendendo dei valori random
# dalla popolazione controllo. Deve essere una popolazione grande
# faccio finta che siano due gruppi diversi perche li chiamo ctrls (controlli)
# e trattati (trmnt) ma in realta' sono gruppi della stessa poplazione
# sono gruppi pre
for(i in 1:n){
   ctrls <- sample(dat, 12)
   trmnt <- sample (dat,12)
   nulls[i] <- mean(ctrls)-mean(trmnt)
   # metto nel vettore nulls la differenza tra le medie 
}

# supponiamo che nello studio che ho condotto, osservo una differenza
# tra le medie di controllo e trattato di 3
