# Situar el directorio de trabajo en la carpeta del proyecto
# Ejecutar para lanzar la aplicación
library(shiny)
library(ggplot2)
library(tidyr)
library(DT)
# Cargo el workspace con todas las variables desde un único fichero RData. Estas variables se han obtenido
# de los ficheros contenidos en el directorio pre y son necesarias para generar la aplicación Shiny. El web scraping
# propiamente dicho se realiza en los ficheros de pre, pero tardan bastante en ejecutarse. Para actualizar la información con
# los datos de la página web, se deberían ejecutar los ficheros de pre de nuevo. Esto se deja a libre elección
# de la persona que utilice la aplicación.
load("./datos/NBAData.RData")
runApp('./NBAStats')