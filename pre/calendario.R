library(tidyverse)
library(rvest)

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
    if(dim(calen[calen[,1] == "Finales NBA",])[1] != 0){
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
    else if(dim(calen[calen[,1] == "Finales de Conferencia",])[1] != 0){
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
    else if(dim(calen[calen[,1] == "Segunda ronda de Conferencia",])[1] != 0){
      SR = grep("Segunda ronda de Conferencia",calen[,1])
      SegundaRonda = calen[(SR+2):(dim(calen)[1]),]
      names(SegundaRonda) = calen[SR+1,]
      PR = grep("Primera ronda de Conferencia",calen[,1])
      PrimeraRonda = calen[(PR+2):(SR-1),]
      names(PrimeraRonda) = calen[PR+1,]
    }
    else{
      PR = grep("Primera ronda de Conferencia",calen[,1])
      if(length(PR) == 0){
        PR = grep("Standard",calen[,1])
      }
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
