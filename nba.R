library(readr)
library(dplyr)
library(ggplot2)
library(modelr)
library(tidyverse)
library(lubridate)

#Import
#In questa fase creo le tibble di cui avrò bisogno per lavorare con i dati relativi alla realtà presa in cosiderazione
#nba_cards rappresenta la raccolta di dati che rappresentano le varie carte all'interno del gioco in un lasso di tempo ben specifico
nba_cards = read_csv("../nba/series_info_final.csv")

#Ho bisogno che la colonna riguardante la data della partita svolta sia strettamente una data per poterci lavorare
nba_cards = nba_cards %>% mutate(`Moment Date` = mdy(`Moment Date`)) 

#Oltre a questa tibble, è utile generare una tibble per poter capire le varie qualità dei giocatori e successivamente capire se c'è qualche relazione con le varie carte
nba_players = read_csv("../nba/giocatori.CSV")

#Avendo un file csv per ogni mese in cui si sono disputate varie partite, applico la funzione read.csv a tutti gli stessi presenti all'interno di una lista

files_to_read = list.files(
  path = "../nba/partite/",        # directory to search within
  pattern = ".*csv$", # regex pattern, some explanation below
  recursive = FALSE,          # search subdirectories
  full.names = TRUE          # return the full path
)
data_lst = lapply(files_to_read, read.csv)

#in questo ciclo svolgo delle operazioni per riuscire ad avere un unico dataset che mi permetta di avere tutte le partite disputate
partite = data_lst[[1]]
for(i in 2:length(data_lst)){
  partite = bind_rows(partite,data_lst[[i]])
}

#molti dei dati al suo interno non sono particolarmente interessanti, di conseguenza li elimino
partite = partite %>% select(-X,-Attend.,-X.1,-Notes,-Notes.......)

#A questo punto ho bisogno di una data che sia normalizzata, quindi prima la porto in formato normale e poi la trasformo grazie alla libreria lubridate
for(i in 1:nrow(partite)){
  partite$Date[i] = str_sub(partite$Date[i],start = 5)
}
partite = partite %>% mutate(Date = mdy(Date))

#Ci sono dei nomi che rappresentano le colonne che non sono particolarmente esaustivi: li modifico
partite = partite %>% rename(Start = Start..ET.,Away=Visitor.Neutral,APTS=PTS,Home=Home.Neutral,HPTS=PTS.1)
View(partite)
#normalizzazione del dataset:si presentava una sottostringa obsoleta che è stata rimossa grazie all'uso di un for
for(i in 1:nrow(nba_players)){
  nba_players$Player[i] = strsplit(nba_players$Player[i],"\\\\")[[1]][1]
}

#Oltre a questi dataset voglio avere a disposizione anche un tibble che riguarda principlamente le squadre della stagione 2020-2021
nba_squads = read_csv("../nba/squadre.csv",skip=1)

#nba_cards rappresenta già una situazione regolare secondo il riordinamento dei dati mentre nba_player no
#prima di tutto vanno eliminate le colonne che non interessano alla nostra analisi: bisogna capire quali sono i dati di cui ho bisogno
#dato che voglio capire che relazioni ci possono essere tra il prezzo della figurina e il giocatore bisogna capire: le sue caratteristiche tecniche, la squadra in cui gioca, le sue capacità legate alle varie partite

nba_players = select(nba_players,-Rk)

#Per poter relazionare i due dataset insieme ho bisogno che i dati riguardanti il nome siano della stessa forma -> in uppercase
nba_players$Player = toupper(nba_players$Player)

#Avendo un dataset con tutti i giocatori di basket e le varie statistiche che li rappresentano, voglio ottenere un criterio grazie al quale stabilire
#qual'è il giocatore più forte di tutti e poter visualizzare, per esempio, la variazione di prezzo in base alla potenza del giocatore stesso.
#Io assumo che più forte è il giocatore secondo il criterio che andrò a stabilire ora, e più costosa sarà una sua carta, seppur di rarità bassa. (Common)

#Secondo la formula di John Hollinger per calcolare il PER (Player Efficiency Rating) ci sono varie statistiche che bisogna tenere in considerazione
#ma grazie ad una semplificazione non tutte hanno lo stesso peso. Terrò conto del PER  come una sorta di ELO che mi permette di distinguere i vari giocatori

nba_players = mutate(nba_players,PER = ((as.numeric(nba_players$FG) *85.91)+(as.numeric(nba_players$STL) *53.897)+(as.numeric(nba_players$`3P`) * 51.757)+(as.numeric(nba_players$FT) *46.845)
                                        +(as.numeric(nba_players$BLK) *39.19)+(as.numeric(nba_players$ORB) *39.19)+(as.numeric(nba_players$AST) *34.677)+(as.numeric(nba_players$DRB) *14.707)
                                        -(as.numeric(nba_players$PF) *17.174)-((as.numeric(nba_players$FTA) - as.numeric(nba_players$FT))*20.091)-((as.numeric(nba_players$FGA) - as.numeric(nba_players$FG))*(39.190))
                                        -(as.numeric(nba_players$TOV) *53.897))*(1/as.numeric(nba_players$MP)))

nba_players = mutate(nba_players, EFF = (PTS+DRB+ORB+AST+STL+BLK-(FGA-FG)-(FTA-FT)-(FGA-TOV))*(1/G))

#ho bisogno di conoscere i mesi e l'anno che si presentano maggiormente per analizzare in maniera efficiente le partite disputate 
View(nba_cards %>%
  mutate(month = month(`Moment Date`), year = year(`Moment Date`)) %>%
  group_by(month, year) %>%
  arrange(-year) %>% 
  summarise(count = n())
)
#Transform
#Visualize
#Model
#Communicate

#ho una tibble con i tipi di momenti rappresentati dai vari nft
moment_type = nba %>%  group_by(`Moment Type`) %>% summarise()

#ottengo una tibble con i vari tipi di rarità che contraddistinguono le carte
rarity_type = nba %>%  group_by(`Moment Rarity`) %>% summarise()

#evidentemente ogni carta fa parte di una particolare edizione, una tra quelle contenute
#da questa tibble
series = nba %>%  group_by(`Series`) %>% summarise()

#interessante sarebbe, capire per ciascuna rarità quali sono le offerte minime dei giocatori
nba_common = nba %>%  filter(nba$`Moment Rarity`=="Common")
ggplot(data=nba,aes(x=`Moment Rarity`,y=`Lowest Ask`)) +
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 10000))

#Ci sono più giocatori dello stesso tipo perchè nella stagione si sono trasferiti in una nuova squadra: mantengo solamente le statistiche di tali che hanno giocato più minuti
nba_players =nba_players %>%
  group_by(nba_players$Player) %>%
  arrange(-nba_players$MP) %>%
  filter(row_number() == 1)
View(nba_players %>%  group_by(nba_players$Player,nba_players$MP) %>%  summarise(count=n()) )
View(nba_cards %>%  group_by(nba_cards$`Player Name`) %>%  summarise(count=n()) )

View(nba_prova)

#gente con nomi strani
View(nba_players %>% filter(grepl('\\?',Player)))

nba_common_cards = nba_cards %>% filter(nba_cards$`Moment Rarity`=="Common")
nba_rare_cards   = nba_cards %>% filter(nba_cards$`Moment Rarity`=="Rare")
nba_legend_cards = nba_cards %>% filter(nba_cards$`Moment Rarity`=="Legendary")
#Tabella importantissima, dati i giocatori di nba 2020-2021 normalizzati, ottengo per ognuno quali e quante carte ci sono
prova= filter(nba_players,PER < 37 && PER > 10) %>% inner_join(nba_cards, c("Player" = "Player Name"))
prova_common= filter(nba_players,PER < 33 && PER > 10) %>% inner_join(nba_common_cards, c("Player" = "Player Name"))
prova_rare=filter(nba_players,PER < 37 && PER > 10) %>% inner_join(nba_rare_cards, c("Player" = "Player Name"))
prova_legend=filter(nba_players,PER < 37 && PER > 10) %>% inner_join(nba_legend_cards, c("Player" = "Player Name"))

ggplot(data=nba_cards,aes(x=nba_cards$`Num Listings`,y=nba_cards$`Lowest Ask`)) +
  geom_point()+
  coord_cartesian(ylim = c(0, 10000))

ggplot(data=nba_common_cards,aes(x=nba_common_cards$Series,y=nba_common_cards$`Lowest Ask`)) +
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 1000))

View(prova)
#Regressione lineare che lega il PER e il prezzo delle varie carte (Senza distinzioni)
ggplot(data=prova,aes(x=prova$EFF,y=prova$`Lowest Ask`)) +
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0, 20000))

#Regressione lineare che lega il PER e il prezzo delle varie carte (Comuni)
ggplot(data=prova_common,aes(x=prova_common$EFF,y=prova_common$`Lowest Ask`)) +
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0, 2000))
#Regressione lineare che lega il PER e il prezzo delle varie carte (Rare)
ggplot(data=prova_rare,aes(x=prova_rare$EFF,y=prova_rare$`Lowest Ask`)) +
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0, 20000))
#Regressione lineare che lega il PER e il prezzo delle varie carte (Legend)
ggplot(data=prova_legend,aes(x=prova_legend$EFF,y=prova_legend$`Lowest Ask`)) +
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0, 100000))

#stop

#provo una regressione lineare sul per e prezzo per le comuni

carte_squad = inner_join(prova_common,nba_squads,c("Tm"="Abbr"))

ggplot(data=nba_common_cards,aes(x=nba_common_cards$`Edition Size`,y=nba_common_cards$`Lowest Ask`)) +
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0, 5000))

reg <- lm(prova_common$`Lowest Ask` ~ prova_common$EFF + prova_common$`Edition Size`, data = prova_common)
summary(reg)

ggplot(data=prova_rare,aes(x=prova_rare$PER,y=prova_rare$`Lowest Ask`)) +
  geom_point()+
  coord_cartesian(ylim = c(0, 1000))

ggplot(data=prova_legend,aes(x=prova_legend$PER,y=prova_legend$`Lowest Ask`)) +
  geom_point()+
  coord_cartesian(ylim = c(0, 1000))


View(partite %>% group_by(Home) %>%  summarise(count = n()))
View(nba_squads)
View(partite %>% group_by(Home) %>%  summarise(count = n()) %>% left_join(nba_squads,c("Home"="Team")))


carte_partite_Home = (nba_cards %>% inner_join(nba_squads,c("Team"="Abbr"))) %>%  inner_join(partite,c("Moment Date"="Date","Team.y"="Home")) 

ggplot(data=carte_partite_Home) +
  geom_freqpoly(aes(x=carte_partite_Home$HPTS), color="blue")+
  geom_freqpoly(aes(x=carte_partite_Home$APTS),color = "red")

carte_partite_Away = (nba_cards %>% inner_join(nba_squads,c("Team"="Abbr"))) %>%  inner_join(partite,c("Moment Date"="Date","Team.y"="Away")) 


ggplot(data=carte_partite_Away) +
  geom_freqpoly(aes(x=carte_partite_Away$HPTS), color="blue")+
  geom_freqpoly(aes(x=carte_partite_Away$APTS),color = "red")

carte_partite = bind_rows(carte_partite_Home,carte_partite_Away)

View(carte_partite)

donut <- nba_cards %>% group_by("Rarity Type" = nba_cards$`Moment Rarity`) %>%  summarise(count=n())
# Create test data.
data <- data.frame(
  category= donut$`Rarity Type`,
  count=donut$count
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

#Grafico a ciambella delle presenze del database basato sulle rarità delle carte

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=3)+
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))+
  theme_void()+
  theme(legend.position = "none")


#come si può notare per la rarità "Comune" il prezzo medio di una carta è di 250€ ma si presentano
#vari outliers fino a figurine dal valore di 7500.
#Perchè ci sono delle figurine di rarità minima che si aggirano ad un prezzo che supera di gran lunga
#la mediana del prezzo delle rare e va a toccare il prezzo di partenza di qualche carta leggendaria?
#potrebbe essere di una serie particolare? è la figurina di un giocatore emblematico?
#gioca in una squadra particolarmente seguita?

#particolare ma difficilmente rappresentabile sarebbe capire se è dato da un fattore estetico o meno

#ci sono delle correlazioni tra giocatori di una squadra tra le prime in classifica e giocatori
#che appartengono a squadre non particolarmente forti in termini di prezzo?
