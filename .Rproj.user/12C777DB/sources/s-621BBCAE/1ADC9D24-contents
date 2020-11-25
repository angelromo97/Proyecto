library(tidyverse)
library(rvest)

# Enlace de la p?gina de inicio (obtenci?n de terminaciones de enlaces a estad?sticas de cada equipo)
url_inicio = 'https://espndeportes.espn.com/basquetbol/nba/estadisticas'
# Inicio de los enlaces a las p?ginas de estad?sticas de cada equipo
url_estadisticas = 'https://espndeportes.espn.com/basquetbol/nba/equipo/estadisticas'
# Url de p?gina mvps
url_mvps = "http://www.espn.com/nba/history/awards/_/id/33"
# Url de p?gina mvps finales
url_mvpspt = "http://www.espn.com/nba/history/awards/_/id/43"
# Inicio de la url del calendario de los equipos
url_cal = "https://espndeportes.espn.com/basquetbol/nba/equipo/calendario/_/nombre/"
# Final: bos/tipodetemporada/2

# Devuelve las terminaciones de los enlaces a las estad?sticas de cada equipo
get_teams_url = function(pag){
  nodo = pag %>%
    html_nodes('.dropdown__select')
  
  cod_equipos = 0
  for(i in 2:31){
    aux = xml_attrs(xml_child(nodo[[1]], i))[["data-url"]]
    auxs = str_split(aux, "/") %>%
      unlist()
    auxc = paste0('/', auxs[6], '/', auxs[7], '/', auxs[8])
    cod_equipos[i-1] = auxc
  }
  cod_equipos
}

# Devuelve la tabla de temporadas
get_seasons = function(pag){
  listas = pag %>%
    html_nodes('.dropdown__select')
  n = length(html_children(listas[[3]]))
  listas_temp = list()
  for(j in 1:n){
    listas_temp[[j]] = xml_child(listas[[3]],j)
  }
  params_seasons = 0
  for(i in 1:n){
    params_seasons[[i]] = xml_attrs(listas_temp[[i]])[["data-param-value"]]
  }  
  params_seasons
}

# Devuelve las tres tablas de inter?s de las p?ginas de estad?sticas de cada equipo (una con el nombre y la posici?n y las otras dos con estad?sticas sobre cada jugador)
get_table = function(pag){
  tablas = pag %>%
    html_nodes('.Table') %>%
      html_table()
  nombres = tablas[[1]]
  stats1 = tablas[[2]]
  stats2 = tablas[[4]]
  cbind(nombres, stats1, stats2)
}

# Obtengo las terminaciones de los enlaces a las estad?sticas de cada equipo
aux_equipos = get_teams_url(read_html(url_inicio))

# Obtengo los enlaces completos a las estad?sticas de cada equipo
url_equipos = paste0(url_estadisticas,aux_equipos)

# Guardo las temporadas
temporadas = list()
i = 1
for(enlace in url_equipos){
  temporadas[[i]] = get_seasons(read_html(enlace))
  i = i + 1
}

# Guardo en la lista tablas las estad?sticas de cada uno de los jugadores de cada equipo. Cada data frame contenido en la lista se identifica con el codigo de su equipo para una localizaci?n m?s sencilla
tablas = list()
i = 1
for(enlace in url_equipos){
  cod_team = strsplit(enlace[[1]],'/')[[1]][10]
  for(terminacion in temporadas[[i]]){
    termin = strsplit(terminacion, "[|]") %>% unlist()
    if(termin[2] == 2) term_aux = paste0(termin[1], '|tr')
    else term_aux = paste0(termin[1], '|pt')
    enlace_completo = paste0(enlace, '/temporada/', termin[1], '/tipodetemporada/', termin[2])
    tablas[[cod_team]][[term_aux]] = get_table(read_html(enlace_completo))
  }
  i = i + 1
}
# Formato de temporadas: a?o|tipodetemporada(2=temporada regular, 3=postemporada)

# Guardo las tablas en la variable stats
stats = tablas

# Exporto la variable stats a un fichero RData. La primera l?nea es la que utilizo en mi ordenador por la ruta especificada, ejecuta la otra.
#save(stats, file = "./Proyecto/stats.RData")
#save(stats, file = "stats.RData")

get_teams_names = function(pag){
  nodo = pag %>%
    html_nodes('.dropdown__option') %>%
    html_text()
  nodo[2:31]
}

teams_names = get_teams_names(read_html(url_inicio))

get_mvps = function(pag){
  tablas = pag %>%
    html_nodes('.tablehead') %>%
    html_table(fill = TRUE)
  tablas = tablas[[1]][2:36,-10]
  names(tablas) = tablas[1,]
  tablas = tablas[2:35,]
  rownames(tablas) = c(1:34)
  tablas
}

mvps = get_mvps(read_html(url_mvps))
mvpspt = get_mvps(read_html(url_mvpspt))

get_table_cal_tr = function(pag){
  tablas = pag %>%
    html_nodes('.Table') %>%
    html_table()
  if(length(tablas)!=0){
    calen = tablas[[1]]
    names(calen) = calen[1,]
    calen = calen[2:dim(calen)[1],]
    rownames(calen) = c(1:dim(calen)[1])
    calen
  }
}
get_table_cal_pt = function(pag){
  tablas = pag %>%
    html_nodes('.Table') %>%
    html_table()
  calen = data.frame()
  tabla_fin = list()
  if(length(tablas)!=0){
    calen = tablas[[1]]
    FinalesNBA = data.frame()
    FinalesConf = data.frame()
    SegundaRonda = data.frame()
    PrimeraRonda = data.frame()
    if(grep("Finales NBA",calen[,1]) != 0){
      FN = grep("Finales NBA",calen[,1])
      FinalesNBA = calen[(FN+2):(dim(calen)[1]),]
      names(FinalesNBA) = calen[FN+1,]
      FC = grep("Finales de Conferencia",calen[,1])
      FinalesConf = calen[(FC+2):(FN-1),]
      names(FinalesConf) = calen[FC+1,]
      SR = grep("Segunda ronda de Conferencia",calen[,1])
      SegundaRonda = calen[(SR+2):(FC-1),]
      names(SegundaRonda) = calen[SR+1,]
      PR = grep("Primera ronda de Conferencia",calen[,1])
      PrimeraRonda = calen[(PR+2):(SR-1),]
      names(PrimeraRonda) = calen[PR+1,]
    }
    else if(grep("Finales de Conferencia",calen[,1]) != 0){
      FC = grep("Finales de Conferencia",calen[,1])
      FinalesConf = calen[(FC+2):(dim(calen)[1]),]
      names(FinalesConf) = calen[FC+1,]
      SR = grep("Segunda ronda de Conferencia",calen[,1])
      SegundaRonda = calen[(SR+2):(FC-1),]
      names(SegundaRonda) = calen[SR+1,]
      PR = grep("Primera ronda de Conferencia",calen[,1])
      PrimeraRonda = calen[(PR+2):(SR-1),]
      names(PrimeraRonda) = calen[PR+1,]
    }
    else if(grep("Segunda ronda de Conferencia",calen[,1]) != 0){
      SR = grep("Segunda ronda de Conferencia",calen[,1])
      SegundaRonda = calen[(SR+2):(dim(calen)[1]),]
      names(SegundaRonda) = calen[SR+1,]
      PR = grep("Primera ronda de Conferencia",calen[,1])
      PrimeraRonda = calen[(PR+2):(SR-1),]
      names(PrimeraRonda) = calen[PR+1,]
    }
    else{
      PR = grep("Primera ronda de Conferencia",calen[,1])
      PrimeraRonda = calen[(PR+2):(dim(calen)[1]),]
      names(PrimeraRonda) = calen[PR+1,]
    }
    if(dim(FinalesNBA)[1] != 0) tabla_fin = list("Primera Ronda"=PrimeraRonda, "Segunda Ronda"=SegundaRonda, "Finales de Conferencia"=FinalesConf, "Finales NBA"=FinalesNBA)
    else if(dim(FinalesConf)[1] != 0) tabla_fin = list("Primera Ronda"=PrimeraRonda, "Segunda Ronda"=SegundaRonda, "Finales de Conferencia"=FinalesConf)
    else if(dim(SegundaRonda)[1] != 0) tabla_fin = list("Primera Ronda"=PrimeraRonda, "Segunda Ronda"=SegundaRonda)
    else tabla_fin = list("Primera Ronda"=PrimeraRonda)
  }
  tabla_fin
}

get_table_cal <- function(pag){
  tablas = pag %>%
    html_nodes('.Table') %>%
    html_table()
  if(length(tablas)!=0){
    calen = tablas[[1]]
    names(calen) = calen[2,]
    calen = calen[calen$FECHA!="FECHA",]
    rownames(calen) = c(1:dim(calen)[1])
    calen
  }
}

calendar = list()
i = 1
#teams_names_aux = list(teams_names[[22]],teams_names[[23]],teams_names[[24]],teams_names[[25]],teams_names[[26]],teams_names[[27]],teams_names[[28]],teams_names[[29]],teams_names[[30]])
for(team in teams_names){
  for(season in temporadas[[i]]){
    termin = strsplit(season, "[|]") %>% unlist()
    url_aux = paste0(url_cal,team,"/temporada/",termin[1],"/tipodetemporada/",termin[2])
    if(termin[2] == 2){
      term_aux = paste0(termin[1], '|tr')
      tab = get_table_cal_tr(read_html(url_aux))
    }
    else{
      term_aux = paste0(termin[1], '|pt')
      tab = get_table_cal(read_html(url_aux))
    }
    calendar[[team]][[term_aux]] = tab
  }
  i = i + 1
}


tabla_pt = get_table_cal_pt(read_html("https://espndeportes.espn.com/basquetbol/nba/equipo/calendario/_/nombre/lal"))

cal = list()
i = 1
for(team in teams_names){
  for(season in temporadas[[i]]){
    termin = strsplit(season, "[|]") %>% unlist()
    url_aux = paste0(url_cal,team,"/temporada/",termin[1],"/tipodetemporada/",termin[2])
    if(termin[2] == 2){
      term_aux = paste0(termin[1], '|tr')
      tab = get_table_cal_tr(read_html(url_aux))
    }
    else{
      term_aux = paste0(termin[1], '|pt')
      tab = get_table_cal_pt(read_html(url_aux))
    }
    if(length(tab)!=0){
      cal[[team]][[term_aux]] = tab
    }
  }
  i = i + 1
}

for(team in names(cal)){
  j = 1
  for(season in names(cal[[team]])){
    termin = strsplit(season, "[|]") %>% unlist()
    if(termin[2] == 2) term_aux = paste0(termin[1], '|tr')
    else term_aux = paste0(termin[1], '|pt')
    names(cal[[team]])[j] = term_aux
    j = j + 1
  }
}

# Elimino del entorno de trabajo las variables que no necesito
rm(list = "tablas", "cod_team", "aux_equipos", "enlace", "i", "url_equipos", "url_estadisticas", "url_inicio", "get_table", "get_teams_url", "get_seasons", "enlace_completo", "termin", "terminacion", "temporadas", "term_aux", "url_mvps", "url_mvpspt", "get_mvps", "get_teams_names")


