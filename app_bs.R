## Etl
library(tidyverse)
## Gráfico
library(plotly)
#library(vistime)
## MAPAS
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(sf)
## Shiny
library(bslib)
library(shiny)
library(shinybrowser)
library(shinycssloaders)
library(shinyjs)
library(shinymanager)
library(shinyWidgets)
library(shinydashboard)

#### ETL ####
source('scripts/etl_estiagem.R')

#### FUNÇÕES ####
source('scripts/funcoes_basicas.R')

#### Autenticação ####
set_labels(
  language = "pt-BR",
  "Please authenticate" = "Informe usuário e senha",
  "Username:" = "Usuário:",
  "Password:" = "Senha:",
  "Login" = "Login"
)

create_db(
  credentials_data = credentials,
  sqlite_path = "../credentials.sqlite", 
  passphrase = "shiny_123"
  # passphrase = "passphrase_wihtout_keyring"
)

ui <- page_sidebar(
  title = "Estiagem",
  sidebar = sidebar(
    title = "Controles",
    varSelectInput(
      "var", "Select variable",
      dplyr::select_if(penguins, is.numeric)
    ),
    numericInput("bins", "Number of bins", 30)
  ),
  card(
    card_header("Histogram"),
    plotOutput("p")
  )
)

shinyApp(ui, function(input, output) {})