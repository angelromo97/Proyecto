navbarPage(
  title = 'Estadísticas NBA',
  tabPanel('Por equipos',
           wellPanel(
             selectInput("equipo", "Equipo:",teams_names),
             uiOutput("temporada_select"),
             fluidRow(column(width = 12, actionButton("refresh", "Recargar")))
           ),
           wellPanel(
             h3(textOutput("Calen")),
             dataTableOutput("cal_table")
           ),
           wellPanel(
             h3(textOutput("Estad")),
             dataTableOutput('stats_table')
           )
  ),
  tabPanel('Por jugadores',
           verticalLayout(
            radioButtons("anot_asist","Seleccione gráfica:", c("Puntos","Asistencias","Rebotes","Robos","Tapones","Pérdidas"), inline = TRUE),
            plotOutput("max"),
            dataTableOutput('mv_table')
            )
  ),
  tabPanel('MVPs TR',
           plotOutput("mvps"),
           dataTableOutput('mvps_table')        
  ),
  tabPanel('MVPs PT',
           plotOutput("mvpspt"),
           dataTableOutput('mvpspt_table')        
  )
)