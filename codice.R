library(readr)
library(dplyr)
library(ggplot2)
library(modelr)
library(tidyverse)
library(lubridate)
library(gganimate)
library(reactable)
library(ggdark)

#Import
#importazione carte
nba_cards = read_csv("../nba/series_info_final.csv")
#importazione giocatori
nba_players = read_csv("../nba/giocatori.CSV")

#importazione squadre
nba_squads = read_csv("../nba/datasets/squadre.csv",skip=1)

#Tutte le partite
files_to_read = list.files(
  path = "../nba/partite/",  # directory in cui cercare
  pattern = ".*csv$",        # pattern che devo ritrovare in ogni file
  full.names = TRUE          # restituisco il percorso intero
)
data_lst = lapply(files_to_read, read.csv)

#Tidy

#Ho bisogno che la colonna riguardante la data della partita svolta sia strettamente una data per poterci lavorare
nba_cards = nba_cards %>% mutate(`Moment Date` = mdy(`Moment Date`))

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

#Si presentava una sottostringa obsoleta all'interno del nome dei giocatori, che è stata opportunamente rimossa
for(i in 1:nrow(nba_players)){
  nba_players$Player[i] = strsplit(nba_players$Player[i],"\\\\")[[1]][1]
}

#Ci sono più giocatori dello stesso tipo perchè nella stagione si sono trasferiti in una nuova squadra: mantengo solamente le statistiche di tali che hanno giocato più minuti
nba_players =nba_players %>%
  group_by(Player) %>%
  arrange(-MP) %>%
  filter(row_number() == 1)
########################################################################### Analisi Giocatori

nba_players = mutate(nba_players,PER = ((as.numeric(FG) *85.91)+(as.numeric(STL) *53.897)+
                                          (as.numeric(`3P`) * 51.757)+(as.numeric(FT) *46.845)+
                                          (as.numeric(BLK) *39.19)+(as.numeric(ORB) *39.19)+
                                          (as.numeric(AST)*34.677)+(as.numeric(DRB) *14.707)-
                                          (as.numeric(PF) *17.174)-((as.numeric(FTA)-
                                                                       as.numeric(FT))*20.091)-((as.numeric(FGA)-
                                                                                                   as.numeric(FG))*(39.190)) -(as.numeric(TOV) *53.897))*(1/as.numeric(MP)))
# è il momento di separare le varie carte in base alla rarità che le contraddistinguono in modo da non essere squilibrate
nba_common_cards = nba_cards %>% filter(nba_cards$`Moment Rarity`=="Common")
#nba_rare_cards   = nba_cards %>% filter(nba_cards$`Moment Rarity`=="Rare")
#nba_legend_cards = nba_cards %>% filter(nba_cards$`Moment Rarity`=="Legendary")


full_common_cards= filter(nba_players,PER < 37 && PER > 10) %>% inner_join(nba_common_cards, c("Player" = "Player Name"))
#prova_rare=filter(nba_players,PER < 37 && PER > 10) %>% inner_join(nba_rare_cards, c("Player" = "Player Name"))
#prova_legend=filter(nba_players,PER < 37 && PER > 10) %>% inner_join(nba_legend_cards, c("Player" = "Player Name"))


#Prendo i 20 giocatori più forti ordinati e valuto il loro prezzo

topplayers <- full_common_cards %>% group_by(Player = full_common_cards$Player) %>% summarise(PER = max(PER),"Lowest Ask" = mean(`Lowest Ask`))

top20players <- topplayers %>% arrange(-topplayers$PER) %>% head(20)

options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)")
))

reactable(top20players,columns = list(PER = colDef(format= colFormat(digits = 2)),
                                      `Lowest Ask` = colDef(format = colFormat(digits = 2))),
          striped = TRUE,
          highlight = TRUE,
          style = list(fontFamily = "Roboto, sans-serif", fontSize = "14px")
)

data_common <- data.frame(
  x=top20players$Player,
  y=top20players$`Lowest Ask`,
  PER = top20players$PER
)
#Scrivo la x che andrò a plottare come fattore, per poter ordinare i livelli in base al PER
data_common$x <- factor(data_common$x, levels = data_common$x)
data_common$x <- factor(data_common$x , levels = data_common$x [order(data_common$PER)])

ggplot(data_common, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="white") +
  geom_point( color="white", size=4, alpha=1) +
  theme_light() +
  coord_flip() +
  dark_theme_gray()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#grafico relazione tra PER e prezzo
players_by_PER <- topplayers %>% arrange(-topplayers$PER)
ggplot(data=players_by_PER,aes(x=PER,y=`Lowest Ask`)) +
  geom_point(
    color="blue",
    fill="#69b3a2",
    shape=20,
    alpha=0.3,
    size=2,
    stroke = 2
  )+
  labs(y="Lowest Ask")+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0, 4000))


reg <- lm(`Lowest Ask` ~ PER, data = players_by_PER)
summary(reg)
########################################################################### 

########################################################################### Analisi Squadre
#A Regime seleziono i giocatori che costano di più,
players <- nba_common_cards %>% group_by(Player=`Player Name`,Team=Team) %>% summarise("Lowest Ask" = max(`Lowest Ask`))
#Essendo le squadre di NBA divise in due Confederazioni, andrò a valutarle in maniera separata
squads_e <- nba_squads %>%  filter(Conf == "E") %>% arrange(Rk)
squads_w <- nba_squads %>%  filter(Conf == "W") %>% arrange(Rk)

#Con un join ordino le squadre in base alla classifica e ne ottengo tutti i giocatori che ne fanno parte
players_squads <- squads_w %>% inner_join(players,by=c("Abbr" = "Team")) %>%   arrange(Rk)

#Ora raggruppo le squadre e calcolo la media del prezzo dei giocatori appartenenti a ciascuna squadra della conf ovest
rank_squadre <- players_squads %>% group_by(Abbr,Rk) %>% summarise("media Prezzo" = mean(`Lowest Ask`)) %>%  arrange(Rk)


# In base alla posizione in classifica
data_squadre <- data.frame(
  id=rank_squadre$Rk,
  individual=rank_squadre$Abbr,
  value=rank_squadre$`media Prezzo`
  
)

data_squadre$individual <- factor(data_squadre$individual, levels = data_squadre$individual)
data_squadre$individual <- factor(data_squadre$individual , levels = data_squadre$individual [order(data_squadre$id)])

ggplot(data_squadre, aes(x=individual, y=value)) + 
  geom_bar(stat = "identity",colors="White")+
  labs(x="West Conference Teams")+
  labs(y="Lowest Ask")+
  geom_text(aes(label=individual), position=position_dodge(width=0.9), vjust=-0.25)+
  coord_cartesian(ylim = c(0, 3500))+
  dark_theme_gray()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#Con un join ordino le squadre in base alla classifica e ne ottengo tutti i giocatori che ne fanno parte
players_squads <- squads_e %>% inner_join(players,by=c("Abbr" = "Team")) %>%   arrange(Rk)

#Ora raggruppo le squadre e calcolo la media del prezzo dei giocatori appartenenti a ciascuna squadra della conf ovest
rank_squadre <- players_squads %>% group_by(Abbr,Rk) %>% summarise("media Prezzo" = mean(`Lowest Ask`)) %>%  arrange(Rk)

data_squadre <- data.frame(
  id=rank_squadre$Rk,
  individual=rank_squadre$Abbr,
  value=rank_squadre$`media Prezzo`
  
)

data_squadre$individual <- factor(data_squadre$individual, levels = data_squadre$individual)
data_squadre$individual <- factor(data_squadre$individual , levels = data_squadre$individual [order(data_squadre$id)])

ggplot(data_squadre, aes(x=individual, y=value)) + 
  geom_bar(stat = "identity")+
  labs(x="East Conference Teams")+
  labs(y="Lowest Ask")+
  geom_text(aes(label=individual), position=position_dodge(width=0.9), vjust=-0.25)+
  coord_cartesian(ylim = c(0, 3500))+
  dark_theme_gray()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#La rappresentazione di tutte le partite in cui il giocatore militava nella squadra di casa
carte_partite_Home = (nba_cards %>% inner_join(nba_squads,c("Team"="Abbr"))) %>%  inner_join(partite,c("Moment Date"="Date","Team.y"="Home"))  %>% select("Player Name",`Moment Date`,Home = "Team.y",Away,HPTS,APTS)
#Le partite in cui invece la squadra del giocatore era quella in trasferta, se le unisco le ottengo tutte.
carte_partite_Away = (nba_cards %>% inner_join(nba_squads,c("Team"="Abbr"))) %>%  inner_join(partite,c("Moment Date"="Date","Team.y"="Away")) %>% select("Player Name",`Moment Date`,Away = "Team.y",Home,HPTS,APTS)

#trovo le partite in cui il giocatore ha vinto con la sua squadra e quelle in cui a perso per creare il grafico a ciambella
w<- count(carte_partite_Home %>% filter(HPTS>APTS))+count(carte_partite_Away %>% filter(APTS>HPTS))
l<- count(carte_partite_Home)+count(carte_partite_Away) - w
match_wl <- data.frame(
  category=c("Won", "Lost"),
  count=c(w[[1]],l[[1]])
)

#Calcolo le percentuali precise
match_wl$fraction <- match_wl$count / sum(match_wl$count)
match_wl$ymax <- cumsum(match_wl$fraction)
match_wl$ymin <- c(0, head(match_wl$ymax, n=-1))

# Posizioni delle 2 label
match_wl$labelPosition <- (match_wl$ymax + match_wl$ymin) / 2
match_wl$label <- paste0(match_wl$category, "\n", match_wl$count)

# Creo il plot
ggplot(match_wl, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=5) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=7) +
  scale_color_brewer(palette=7) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x  = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  theme(legend.position = "none")

#Collezionabili che riguardano partite disputate in casa -> calcolo la differenza reti
carte_partite_Home = (nba_cards %>% inner_join(nba_squads,c("Team"="Abbr"))) %>%  inner_join(partite,c("Moment Date"="Date","Team.y"="Home") )
players_partite_Home <- data.frame(
  nome=carte_partite_Home$`Player Name`,
  Abbr=carte_partite_Home$Team.y,
  diff=carte_partite_Home$HPTS- carte_partite_Home$APTS,
  prezzo=carte_partite_Home$`Lowest Ask`
  
)
players_partite_Home<-players_partite_Home %>%  arrange(-diff)

#Collezionabili che riguardano partite disputate fuori casa -> calcolo la differenza reti
carte_partite_Away = (nba_cards %>% inner_join(nba_squads,c("Team"="Abbr"))) %>%  inner_join(partite,c("Moment Date"="Date","Team.y"="Away") )
players_partite_Away <- data.frame(
  nome=carte_partite_Home$`Player Name`,
  Abbr=carte_partite_Home$Team.y,
  diff=carte_partite_Home$APTS- carte_partite_Home$HPTS,
  prezzo=carte_partite_Home$`Lowest Ask`
  
)
players_partite_Away<-players_partite_Away %>%  arrange(-diff)


#plot sul prezzo relazionato alla differenza di punti
ggplot(union(players_partite_Home,players_partite_Away), aes(x=diff, y=prezzo)) + 
  geom_point(
    color="orange",
    fill="orange",
    shape=21,
    alpha=0.3,
    size=3,
    stroke = 1
  )+
  coord_cartesian(ylim = c(0, 15000))+
  dark_theme_gray()+
  labs(x="Differenza punti")+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
########################################################################### 

########################################################################### Analisi carta
ggplot(nba_cards, aes(x=`Moment Rarity`, y=`Lowest Ask`,fill=`Moment Rarity`)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2) +
  theme(legend.position="none")+
  coord_cartesian(ylim = c(0, 15000))+
  dark_theme_gray()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggplot(data=nba_cards,aes(x=`Edition Size`,y=`Lowest Ask`)) +
  geom_point()+
  coord_cartesian(ylim = c(0, 10000))+
  dark_theme_gray()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggplot(nba_cards, aes(x=`Num Listings`, y=`Lowest Ask`,color = `Moment Rarity`)) + 
  geom_point(alpha=0.5)+     coord_cartesian(ylim = c(0, 50000))+
  dark_theme_gray()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
########################################################################### 
