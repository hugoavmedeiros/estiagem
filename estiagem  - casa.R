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

#### Filtros ####

filtro1 <- dropdownButton(
  criarCheckboxGroup(
    'bt1', 
    'Situação', 
    choiceValues = unique(municipios_pe$Situação),
    choiceNames = unique(municipios_pe$Situação), 
    selected = unique(municipios_pe$Situação)
  ),  # fecha checkboxGroupInput
  circle = TRUE, 
  size = 'sm',
  status = "danger",
  icon = icon("list"), 
  width = "100%",
  right = T,
  tooltip = tooltipOptions(title = "Situação")
)

filtro2 <- dropdownButton(
  criarCheckboxGroup(
    'bt2', 
    'Seca', 
    choiceValues = unique(municipios_pe$`Seca Outubro`), 
    choiceNames = unique(municipios_pe$`Seca Outubro`), 
    selected = unique(municipios_pe$`Seca Outubro`)
  ),  # fecha checkboxGroupInput
  circle = TRUE, 
  size = 'sm',
  status = "danger",
  icon = icon("triangle-exclamation"), 
  width = "100%",
  right = T,
  tooltip = tooltipOptions(title = "Seca")
)

filtro3 <- dropdownButton(
  criarCheckboxGroup(
    'bt3', 
    'Secretaria', 
    choiceValues = unique(iniciativas_estiagem$SECRETARIA), 
    choiceNames = unique(iniciativas_estiagem$SECRETARIA), 
    selected = unique(iniciativas_estiagem$SECRETARIA)
  ),  # fecha checkboxGroupInput
  circle = TRUE, 
  size = 'sm',
  status = "danger",
  icon = icon("landmark"), 
  width = "100%",
  right = T,
  tooltip = tooltipOptions(title = "Secretaria")
) # fecha dropwdown

filtro4 <- dropdownButton(
  criarCheckboxGroup(
    'bt4', 
    'Fonte do recurso', 
    choiceValues = unique(iniciativas_estiagem$`FONTE RECURSO`), 
    choiceNames = unique(iniciativas_estiagem$`FONTE RECURSO`), 
    selected = unique(iniciativas_estiagem$`FONTE RECURSO`)
  ),  # fecha checkboxGroupInput
  circle = TRUE, 
  size = 'sm',
  status = "danger",
  icon = icon("filter-circle-dollar"), 
  width = "100%",
  right = T,
  tooltip = tooltipOptions(title = "Fonte do recurso")
) # fecha dropwdown

filtro5 <- dropdownButton(
  criarCheckboxGroup(
    'bt5', 
    'Tipo de Ação', 
    choiceValues = unique(iniciativas_estiagem$`TIPO DE AÇÃO`), 
    choiceNames = unique(iniciativas_estiagem$`TIPO DE AÇÃO`), 
    selected = unique(iniciativas_estiagem$`TIPO DE AÇÃO`)
  ),  # fecha checkboxGroupInput
  circle = TRUE, 
  size = 'sm',
  status = "danger",
  icon = icon("table-list"), 
  width = "100%",
  right = T,
  tooltip = tooltipOptions(title = "Tipo de Ação")
) # fecha dropwdown

filtro6 <- dropdownButton(
  criarCheckboxGroup(
    'bt6', 
    'Status', 
    choiceValues = unique(iniciativas_estiagem$STATUS), 
    choiceNames = unique(iniciativas_estiagem$STATUS), 
    selected = unique(iniciativas_estiagem$STATUS)
  ),  # fecha checkboxGroupInput
  circle = TRUE, 
  size = 'sm',
  status = "danger",
  icon = icon("bars-staggered"), 
  width = "100%",
  right = T,
  tooltip = tooltipOptions(title = "Tipo de Ação")
) # fecha dropwdown

filtro7 <- 
  sliderInput(
    "valor_estimado_f",
    h5("Valor Estimado"), 
    min = 1, 
    max = 119581000, 
    value = c(1,119581000),
    pre = "R$",
    ticks = F
    )

#### interface #### 
header <- dashboardHeader(
  title = "Painel de Estiagem",
  tags$li(class = "dropdown",
          
          tags$li(
            a(href = 'https://www.seplag.pe.gov.br/',
              "Desenvolvido por SEPLAG-PE",
              title = "Secretaria de Planejamento",
              style = "padding-top: 15px; padding-bottom: 15px;"
            ),
            class = "dropdown"
          ),
          
          tags$li(a(href = 'https://www.pe.gov.br/',
                    img(src = 'logo.png',
                        title = "Governo de Pernambuco", height = "30px"),
                    style = "padding-top:10px; padding-bottom:10px;"),
                  class = "dropdown")
  )
  )

sidebar <- dashboardSidebar(
  collapsed = TRUE,
  h5("Filtros"),
  
  fluidRow(
    column(12,
             fluidRow(
               column(6, filtro1),
               column(6, filtro2)),
            fluidRow(
               column(6, filtro3),
               column(6, filtro4)
             ),
           fluidRow(
             column(6, filtro5),
             column(6, filtro6)
             ),
           fluidRow(
             column(11, filtro7)
           )
           )
  ),
  
  h5("Painéis"),
  
  sidebarMenu(
    menuItem("Painel Geral", tabName = "geral", icon = icon('circle-info')),
    menuItem("Iniciativas", tabName = "iniciativas", icon = icon('person-digging'))
  )
)

### CORPO

body <- dashboardBody(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesShiny.css")),
  
  tabItems(
    tabItem(tabName = "geral",
  
  fluidRow(
  valueBoxOutput('municipios', width = '3'),
  valueBoxOutput('pessoas', width = '3'),
  valueBoxOutput('ocp', width = '3'),
  valueBoxOutput('seca1', width = '3')
  ),
       
  fluidRow(
  leafletOutput("mapa1", height = 400)
  ),
  
  absolutePanel(top = 180, right = "50%", uiOutput('municipios2'), draggable = T)
  
), # fecha tab "geral"

tabItem(
  tabName = "iniciativas",
  
  fluidRow(
    valueBoxOutput('total_iniciativa', width = '4'),
    valueBoxOutput('pessoas_iniciativa', width = '4'),
    valueBoxOutput('valor_estimado', width = '4'),
  ),
  
  fluidRow(
    plotOutput("timeline1", height = 500)
    )

) # fecha tab "geral"        

) # fecha tab tabItems

) # fecha corpo dashboardBody

ui <- dashboardPage(
  header,
  sidebar,
  body
)

#### servidor #### 

server <- function(input, output, session) {
  
  ### NAV BAR ###
  
  ### BASES ###
  
  municipio_pe_filtro <- reactive({
    municipios_pe %>% dplyr::filter(Situação %in% input$bt1, `Seca Outubro` %in% input$bt2)
  })
  
  iniciativas_estiagem_filtro <- reactive({
    iniciativas_estiagem  %>% dplyr::filter(
      SECRETARIA %in% input$bt3, 
      `FONTE RECURSO` %in% input$bt4, 
      `TIPO DE AÇÃO` %in% input$bt5,
      STATUS %in% input$bt6,
      `VALOR ESTIMADO` >= input$valor_estimado_f[1] & `VALOR ESTIMADO` <= input$valor_estimado_f[2]
      )
  })
  
  ### PAINEL GERAL
  
  output$municipios <- renderValueBox({
    infoBox(
      tags$p(div("Municípios", br(), "Reconhecidos",  style = "font-size: 90%;")),
      municipio_pe_filtro() %>% st_drop_geometry() %>% filter(Situação == "Reconhecido") %>% tally(), icon = icon("city"),
      color = "aqua",
      fill= T
    )
  })
  
  output$pessoas <- renderValueBox({
    infoBox(
      tags$p(div("Pessoas", br(), "Afetadas",  style = "font-size: 90%;")),
      sum(municipio_pe_filtro()$`Pessoas Afetadas`) %>% formatC(
        format = "f", 
        big.mark=".", 
        digits = 0, 
        decimal.mark = ','
        ), icon = icon("people-group"),
      color = "teal",
      fill= T
    )
  })
  
  output$ocp <- renderValueBox({
    infoBox(
      tags$p(div("Municípios", br(), "OCP",  style = "font-size: 90%;")),
      municipio_pe_filtro() %>% st_drop_geometry() %>% filter(OCP == "Sim") %>% tally(), 
      icon = icon("truck-droplet"),
      color = "navy",
      fill= T
    )
  })
  
  output$seca1 <- renderValueBox({
    infoBox(
      tags$p(div("Seca", br(), "Moderada",  style = "font-size: 90%;")),
      municipio_pe_filtro() %>% st_drop_geometry() %>% filter(`Seca Outubro` %in% c("Seca Moderada")) %>% tally(), 
      icon = icon("triangle-exclamation"),
      color = "orange",
      fill= T
    )
  })
  
  output$municipios2 <- renderUI({
    
    texto <- paste('<div class="custom-text"> Municípios', "Afetados",      municipio_pe_filtro() %>% st_drop_geometry() %>% filter(Situação != "Não afetado") %>% tally(), '</div>'
                   )
    
    HTML(texto)
    
  })
  
  output$mapa1 <- renderLeaflet({
    municipio_pe_filtro() %>%
      leaflet() %>%
      addTiles() %>% 
      addPolygons( # SITUAÇÃO POR REGIÃO
        fillColor = ~fpal_situacao_regiao(situacao_regiao),
        fillOpacity = 0.8,
        weight = 0.5,
        opacity = 1,
        color = "white",
        label = ~htmlEscape(
          paste(
            NM_MUN, " | ",
            Situação, " | ",
            formatC(`Pessoas Afetadas`, big.mark = '.', format = "f", digits = 0, decimal.mark = ','),
            " Pessoas Afetadas (Total)", sep ='')),
        labelOptions = labelOptions(
          noHide = F, 
          direction = "bottom",
          style = list(
            "font-family" = "serif",
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "20px",
            "border-color" = "rgba(0,0,0,0.5)")
        ),
        group = "Situação por Região"
      ) %>% 
      addLegend(
        pal = fpal_situacao_regiao,
        values = ~situacao_regiao,
        opacity = 0.7,
        title = 'Situação por Região',
        position = "bottomleft",
        layerId  = "Situação por Região"
      ) %>% 
      addPolygons( # PESSOAS AFETADAS TAXA
        fillColor = ~bpal_pessoas_afetadas_taxa(pessoas_afetadas_tx),
        fillOpacity = 0.8,
        weight = 0.5,
        opacity = 1,
        color = "white",
        label = ~htmlEscape(
          paste(
            NM_MUN, " | ",
            Situação, " | ",
            scales::percent(round(pessoas_afetadas_tx, 1), decimal.mark = ","), 
            ' Pessoas Afetadas (Taxa)',
            sep ='')),
        labelOptions = labelOptions(
          noHide = F, 
          direction = "bottom",
          style = list(
            "font-family" = "serif",
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "20px",
            "border-color" = "rgba(0,0,0,0.5)")
        ),
        group = "Pessoas Afetadas (Taxa)"
      ) %>% 
      addLegend(
        pal = bpal_pessoas_afetadas_taxa,
        values = ~pessoas_afetadas_tx,
        opacity = 0.7,
        title = 'Pessoas Afetadas (Taxa)',
        position = "bottomleft",
        layerId  = "Pessoas Afetadas (Taxa)"
      ) %>% 
      addPolygons( # OCP
        fillColor = ~fpal_ocp(OCP),
        fillOpacity = 0.8,
        weight = 0.5,
        opacity = 1,
        color = "white",
        label = ~htmlEscape(
          paste(
            NM_MUN, " | ",
            Situação, " | ", 
            scales::percent(round(pessoas_afetadas_tx, 1), decimal.mark = ","),
            ' Pessoas Afetadas (Taxa)',
            sep =': ')),
        labelOptions = labelOptions(
          noHide = F, 
          direction = "bottom",
          style = list(
            "font-family" = "serif",
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "20px",
            "border-color" = "rgba(0,0,0,0.5)")
        ),
        group = "OCP"
      ) %>% 
      addLegend(
        pal = fpal_ocp,
        values = ~OCP,
        opacity = 0.7,
        title = 'OCP',
        position = "bottomleft",
        layerId  = "OCP"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Situação por Região",
          "Pessoas Afetadas (Taxa)",
          'OCP'
        ),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      htmlwidgets::onRender("
    function(el, x) {
      var initialLegend = 'Situação por Região'
      var myMap = this;
      for (var legend in myMap.controls._controlsById) {
        var el = myMap.controls.get(legend.toString())._container;
        if(legend.toString() === initialLegend) {
          el.style.display = 'block';
        } else {
          el.style.display = 'none';
        };
      };
    myMap.on('baselayerchange',
      function (layer) {
        for (var legend in myMap.controls._controlsById) {
          var el = myMap.controls.get(legend.toString())._container;
          if(legend.toString() === layer.name) {
            el.style.display = 'block';
          } else {
            el.style.display = 'none';
          };
        };
      });
    }")  %>%
      addFullscreenControl()
  })
  
  ### Tab Iniciativas
  
  output$total_iniciativa <- renderValueBox({
    infoBox(
      tags$p(div("Total", br(), "Iniciativas",  style = "font-size: 90%;")),
      iniciativas_estiagem_filtro() %>% tally(), 
      icon = icon("person-digging"),
      color = "olive",
      fill= T
    )
  })
  
  output$pessoas_iniciativa <- renderValueBox({
    infoBox(
      tags$p(div("Pessoas", br(), "Beneficiadas",  style = "font-size: 90%;")),
      sum(iniciativas_estiagem_filtro()$`PESSOAS BENEFICIADAS`,na.rm = T) %>% formatC(
        format = "f", 
        big.mark=".", 
        digits = 0, 
        decimal.mark = ','
      ), 
      icon = icon("people-group"),
      color = "teal",
      fill= T
    )
  })
  
  output$valor_estimado <- renderValueBox({
    infoBox(
      tags$p(div("Valor", br(), "Estimado",  style = "font-size: 90%;")),
      MillionForm(sum(iniciativas_estiagem_filtro()$`VALOR ESTIMADO`,na.rm = T)), 
      icon = icon("money-bill-trend-up"),
      color = "blue",
      fill= T
    )
  })
  
  output$timeline1 <- renderPlot({
    iniciativas_estiagem_filtro() %>% vistime::gg_vistime(
      col.event = "content",
      col.group = "AGRUPAMENTO",
      col.color = 'cor',
      title = "Linha do Tempo",
      optimize_y = T
      , linewidth = 7
      ) + theme(
        text = element_text(size = 16),
        axis.text = element_text(size = 13),
        panel.background = element_rect(fill = 'NA'),
        panel.ontop = FALSE
      ) + scale_x_datetime(
        date_labels = "%Y %m",
        position = 'top',
        breaks = 'month')
  })
  
} # fecha o servidor

shinyApp(ui = ui, server = server)