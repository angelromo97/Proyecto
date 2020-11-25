# load("./Proyecto/datos/stats.RData")
library(tidyverse)
library(ggplot2)
Jugadores = c()
i = 1
for(team in stats){
  j = 1
  for(season in team){
    ma = season %>%
      select(Nombre, MIN, PTS, AST, REB, STL, BLK, TO, 18, 21, 24) %>%
      filter(PTS==max(PTS[-length(PTS)])) %>%
      mutate(Temporada = names(team)[j], Equipo = names(stats)[i])
    
    if(length(ma[,1])!=1) ma = ma[1,]
    
    Jugadores = rbind(Jugadores, ma)
    
    j = j + 1
  }
  i = i + 1
}
max_anot = Jugadores %>%
  select(1,12,13,3,4,5,6,7,8,9,10,11,2) %>%
  group_by(Temporada) %>%
  filter(PTS == max(PTS)) %>%
  arrange(desc(Temporada))

max_asist = Jugadores %>%
  select(1,12,13,3,4,5,6,7,8,9,10,11,2) %>%
  group_by(Temporada) %>%
  filter(AST == max(AST)) %>%
  arrange(desc(Temporada))

max_reb = Jugadores %>%
  select(1,12,13,3,4,5,6,7,8,9,10,11,2) %>%
  group_by(Temporada) %>%
  filter(REB == max(REB)) %>%
  arrange(desc(Temporada))

max_stl = Jugadores %>%
  select(1,12,13,3,4,5,6,7,8,9,10,11,2) %>%
  group_by(Temporada) %>%
  filter(STL == max(STL)) %>%
  arrange(desc(Temporada))

max_tap = Jugadores %>%
  select(1,12,13,3,4,5,6,7,8,9,10,11,2) %>%
  group_by(Temporada) %>%
  filter(BLK == max(BLK)) %>%
  arrange(desc(Temporada))

max_per = Jugadores %>%
  select(1,12,13,3,4,5,6,7,8,9,10,11,2) %>%
  group_by(Temporada) %>%
  filter(TO == max(TO)) %>%
  arrange(desc(Temporada))

mvp = max_anot %>%
  group_by(Nombre) %>%
  summarise(n = n(), MediaPTS = signif(mean(PTS), digits = 4), .groups = 'drop') %>%
  arrange(desc(n)) #%>%
  #rename("Temporadas siendo MSP" = "n")

mva = max_asist %>%
  group_by(Nombre) %>%
  summarise(n = n(), MediaAST = signif(mean(AST), digits = 4), .groups = 'drop') %>%
  arrange(desc(n)) #%>%
#rename("Temporadas siendo MSP" = "n")

mvr = max_reb %>%
  group_by(Nombre) %>%
  summarise(n = n(), MediaREB = signif(mean(REB), digits = 4), .groups = 'drop') %>%
  arrange(desc(n)) #%>%
#rename("Temporadas siendo MSP" = "n")

mvs = max_stl %>%
  group_by(Nombre) %>%
  summarise(n = n(), MediaSTL = signif(mean(STL), digits = 4), .groups = 'drop') %>%
  arrange(desc(n)) #%>%
#rename("Temporadas siendo MSP" = "n")

mvt = max_tap %>%
  group_by(Nombre) %>%
  summarise(n = n(), MediaBLK = signif(mean(BLK), digits = 4), .groups = 'drop') %>%
  arrange(desc(n)) #%>%
#rename("Temporadas siendo MSP" = "n")

mvto = max_per %>%
  group_by(Nombre) %>%
  summarise(n = n(), MediaTO = signif(mean(TO), digits = 4), .groups = 'drop') %>%
  arrange(desc(n)) #%>%
#rename("Temporadas siendo MSP" = "n")

#Ejemplo gr�fica, luego la meto en el shiny
ggplot(mvp, aes(x = reorder(Nombre, n), y = n, fill = as.factor(n))) +
  geom_bar(stat="identity", color = "black") +
  labs(title = "Temporadas siendo máximo anotador", x = "Jugador", y = "Número de temporadas") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), plot.title = element_text(size = 14), legend.position="none", panel.grid.major.x = element_line(colour = "black",size=0.1)) +
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  scale_fill_manual(values=c("#ff8484","#ff7878","#ff6b6b","#ff5e5e","#ff5353","#ff4747","#ff3a3a","#ff3030","#ff1d1d")) + # Resto de colores (m?s claritos primero, por si me los pide): "#ffbebe","#ffb3b3","#ffa6a6","#ff9c9c","#ff9191",
  coord_flip() +
  geom_text(aes(label=as.factor(n)), hjust=1.6, color="white", size=4)

rm(list = "Jugadores", "ma", "season", "team", "i", "j")
