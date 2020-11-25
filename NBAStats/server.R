function(input, output) {
  
  equipTemp <- eventReactive(input$refresh,{
    c(input$equipo, input$temporada)
  })
  
  output$temporada_select <- renderUI({
    selectInput("temporada", "Temporada:", names(stats[[input$equipo]]))
  })
  
  output$cal_table <- DT::renderDataTable(
    expr = DT::datatable(calendar[[equipTemp()[1]]][[equipTemp()[2]]], options = list(
      lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15','All')),
      pageLength = -1
    ), escape = FALSE) %>% formatStyle(
      'FECHA',
      target = 'row',
      backgroundColor = styleEqual(c("Primera ronda de Conferencia", "Segunda ronda de Conferencia", "Finales de Conferencia", "Finales NBA"), c('gray', 'gray','gray','gray'))
    )
  )
  
  
  output$stats_table <- renderDataTable(
    stats[[equipTemp()[1]]][[equipTemp()[2]]],
    options = list(
      lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15','All')),
      pageLength = 10
    )
  )
  
  output$max <- renderPlot({
    var = 0
    tit = ""
    if(input$anot_asist == "Puntos"){
      var = mvp
      tit = "anotador"
    }
    else if(input$anot_asist == "Asistencias"){
      var = mva
      tit = "asistente"
    }
    else if(input$anot_asist == "Rebotes"){
      var = mvr
      tit = "reboteador"
    }
    else if(input$anot_asist == "Robos"){
      var = mvs
      tit = "stealer"
    }
    else if(input$anot_asist == "Tapones"){
      var = mvt
      tit = "blocker"
    }
    else if(input$anot_asist == "Pérdidas"){
      var = mvto
      tit = "perdedor de posesión"
    }
    ggplot(var, aes(x = reorder(Nombre, n), y = n, fill = as.factor(n))) +
      geom_bar(stat="identity") +
      labs(title = paste0("Temporadas siendo máximo ", tit), x = "Jugador", y = "Número de temporadas") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), plot.title = element_text(size = 14), legend.position="none", panel.grid.major.x = element_line(colour = "black",size=0.1)) +
      scale_y_continuous(breaks=seq(0, 10, 1)) +
      scale_fill_manual(values=c("#ff8484","#ff7878","#ff6b6b","#ff5e5e","#ff5353","#ff4747","#ff3a3a","#ff3030","#ff1d1d")) + # Resto de colores (m?s claritos primero, por si me los pide): "#ffbebe","#ffb3b3","#ffa6a6","#ff9c9c","#ff9191",
      coord_flip() +
      geom_text(aes(label=as.factor(n)), hjust=1.6, color="white", size=4)
  })
  output$mv_table <- renderDataTable({
    var = 0
    if(input$anot_asist == "Puntos"){
      var = mvp
    }
    else if(input$anot_asist == "Asistencias"){
      var = mva
    }
    else if(input$anot_asist == "Rebotes"){
      var = mvr
    }
    else if(input$anot_asist == "Robos"){
      var = mvs
    }
    else if(input$anot_asist == "Tapones"){
      var = mvt
    }
    else if(input$anot_asist == "Pérdidas"){
      var = mvto
    }
    var
  }, 
    options = list(
    lengthMenu = list(c(5, 10, -1), c('5', '10','All')),
    pageLength = 10
    )
  )
  output$mvps_table <- renderDataTable(
    mvps,
    options = list(
      lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15','All')),
      pageLength = 10
    )
  )
  output$mvps <- renderPlot({
    library(tidyverse)
    mvp = mvps %>%
      group_by(PLAYER) %>%
      summarise(n = n(), .groups = 'drop') %>%
      arrange(desc(n))
    ggplot(mvp, aes(x = reorder(PLAYER, n), y = n, fill = as.factor(n))) +
      geom_bar(stat="identity") +
      labs(title = "Temporadas siendo MVP", x = "Jugador", y = "Número de temporadas") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), plot.title = element_text(size = 14), legend.position="none", panel.grid.major.x = element_line(colour = "black",size=0.1)) +
      scale_y_continuous(breaks=seq(0, 10, 1)) +
      scale_fill_manual(values=c("#ff8484","#ff7878","#ff6b6b","#ff5e5e","#ff5353","#ff4747","#ff3a3a","#ff3030","#ff1d1d")) + # Resto de colores (m?s claritos primero, por si me los pide): "#ffbebe","#ffb3b3","#ffa6a6","#ff9c9c","#ff9191",
      coord_flip() +
      geom_text(aes(label=as.factor(n)), hjust=1.6, color="white", size=4)
  })
  
  output$mvpspt_table <- renderDataTable(
    mvpspt,
    options = list(
      lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15','All')),
      pageLength = 10
    )
  )
  
  output$mvpspt <- renderPlot({
    library(tidyverse)
    mvp = mvpspt %>%
      group_by(PLAYER) %>%
      summarise(n = n(), .groups = 'drop') %>%
      arrange(desc(n))
    ggplot(mvp, aes(x = reorder(PLAYER, n), y = n, fill = as.factor(n))) +
      geom_bar(stat="identity") +
      labs(title = "Finales siendo MVP", x = "Jugador", y = "Número de temporadas") +
      theme_classic() +
      theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), plot.title = element_text(size = 14), legend.position="none", panel.grid.major.x = element_line(colour = "black",size=0.1)) +
      scale_y_continuous(breaks=seq(0, 10, 1)) +
      scale_fill_manual(values=c("#ff8484","#ff7878","#ff6b6b","#ff5e5e","#ff5353","#ff4747","#ff3a3a","#ff3030","#ff1d1d")) + # Resto de colores (m?s claritos primero, por si me los pide): "#ffbebe","#ffb3b3","#ffa6a6","#ff9c9c","#ff9191",
      coord_flip() +
      geom_text(aes(label=as.factor(n)), hjust=1.6, color="white", size=4)
  })
  
  output$Estad <- renderText("Estadísticas globales por jugador:")
  
  output$Calen <- renderText("Calendario:")
  
}
