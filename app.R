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

#### Autenticação ####
set_labels(
  language = "pt-BR",
  "Please authenticate" = "Informe usuário e senha",
  "Username:" = "Usuário:",
  "Password:" = "Senha:",
  "Login" = "Login"
)

if (!file.exists("../credentials.sqlite")) {
  create_db(
    credentials_data = credentials,
    sqlite_path = "../credentials.sqlite", 
    passphrase = "shiny_123"
  )
}

#### Filtros ####


#### interface #### 

    #### header #### 

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

#### sidebar ####

sidebar <- dashboardSidebar(
  div(style="overflow-y: scroll;"),
  collapsed = TRUE,
  
  h5("Painéis"),
  
  sidebarMenu(
    menuItem(
      "Painel Geral", 
      tabName = "geral", 
      icon = icon('circle-info')),
    menuItem(
      "Iniciativas", 
      tabName = "iniciativas", 
      icon = icon('person-digging')),
    menuItem(
      "OCP", 
      tabName = "tb_ocp", 
      icon = icon('truck-droplet'))
  )
)

#### corpo ####

body <- dashboardBody(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesShiny.css")),
  
  useShinyjs(),
  
  box(
    width = 12,
    height = 50,
    solidHeader = TRUE,
    div(
      style = "display: flex; justify-content: space-between;",
      div(
        style = "width: 200%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt8_todos", "Todos"),
            actionButton("bt8_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          criarCheckboxGroup(
            'bt8', 
            'Iniciativa', 
            choiceValues = unique(iniciativas_estiagem$iniciativa_nome), 
            choiceNames = unique(iniciativas_estiagem$iniciativa_nome), 
            selected = unique(iniciativas_estiagem$iniciativa_nome)
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Iniciativa',
          size = 'sm',
          status = "danger",
          icon = icon("person-digging"), 
          width = "120%",
          right = T,
          tooltip = tooltipOptions(title = "Iniciativa")
        ) # fecha dropdown
      ), # fecha div do 1º filtro
      
      div( # 2º filtro
        style = "width: 68%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt2_todos", "Todos"),
            actionButton("bt2_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          criarCheckboxGroup(
            'bt2', 
            'Seca', 
            choiceValues = unique(municipios_pe$`Seca Outubro`), 
            choiceNames = unique(municipios_pe$`Seca Outubro`), 
            selected = unique(municipios_pe$`Seca Outubro`)
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Seca',
          size = 'sm',
          status = "danger",
          icon = icon("triangle-exclamation"), 
          width = "100%",
          right = T,
          tooltip = tooltipOptions(title = "Seca")
        ) # fecha dropdown
      ), # fecha div do 2º filtro
      
      div( # 3º filtro
        style = "width: 68%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt3_todos", "Todos"),
            actionButton("bt3_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          criarCheckboxGroup(
            'bt3', 
            'Órgão', 
            choiceValues = unique(iniciativas_estiagem$SECRETARIA), 
            choiceNames = unique(iniciativas_estiagem$SECRETARIA), 
            selected = unique(iniciativas_estiagem$SECRETARIA)
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Órgão',
          size = 'sm',
          status = "danger",
          icon = icon("landmark"), 
          width = "100%",
          right = T,
          tooltip = tooltipOptions(title = "Órgão")
        ) # fecha dropdown
      ),
      
      div( # 4º filtro
        style = "width: 68%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt4_todos", "Todos"),
            actionButton("bt4_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          criarCheckboxGroup(
            'bt4', 
            'Fonte do recurso', 
            choiceValues = unique(iniciativas_estiagem$`FONTE RECURSO`), 
            choiceNames = unique(iniciativas_estiagem$`FONTE RECURSO`), 
            selected = unique(iniciativas_estiagem$`FONTE RECURSO`)
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Fonte',
          size = 'sm',
          status = "danger",
          icon = icon("filter-circle-dollar"), 
          width = "150%",
          right = T,
          tooltip = tooltipOptions(title = "Fonte do Recurso")
        ) # fecha dropdown
      ), # fecha 4º filtro
      
      div( # 5º filtro
        style = "width: 68%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt5_todos", "Todos"),
            actionButton("bt5_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          criarCheckboxGroup(
            'bt5', 
            'Ação', 
            choiceValues = unique(iniciativas_estiagem$`TIPO DE AÇÃO`), 
            choiceNames = unique(iniciativas_estiagem$`TIPO DE AÇÃO`), 
            selected = unique(iniciativas_estiagem$`TIPO DE AÇÃO`)
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Ação',
          size = 'sm',
          status = "danger",
          icon = icon("table-list"), 
          width = "150%",
          right = T,
          tooltip = tooltipOptions(title = "Ação")
        ) # fecha dropdown
      ), # fecha 5º filtro
      
      div( # 6º filtro
        style = "width: 68%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt6_todos", "Todos"),
            actionButton("bt6_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          criarCheckboxGroup(
            'bt6', 
            'Status', 
            choiceValues = unique(iniciativas_estiagem$STATUS), 
            choiceNames = unique(iniciativas_estiagem$STATUS), 
            selected = unique(iniciativas_estiagem$STATUS)
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Status',
          size = 'sm',
          status = "danger",
          icon = icon("bars-staggered"), 
          width = "150%",
          right = T,
          tooltip = tooltipOptions(title = "Status")
        ) # fecha dropdown
      ), # fecha 6º filtro
      
      div( # 7º filtro
        style = "width: 68%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt8_todos", "Todos"),
            actionButton("bt8_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          sliderInput(
            "valor_estimado_f",
            "Valor",
            min = 1, 
            max = 119581, 
            value = c(1, 119581),
            pre = "R$",
            sep = ".",
            ticks = F,
            width = "95%"
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Valor',
          size = 'sm',
          status = "danger",
          icon = icon("brazilian-real-sign"), 
          width = "100%",
          right = T,
          tooltip = tooltipOptions(title = "Valor")
        ) # fecha dropdown
      ), # fecha filtro 7
      
      div( # 8º filtro
        style = "width: 68%; display: inline-block;",
        dropdownButton(
          div(
            actionButton("bt1_todos", "Todos"),
            actionButton("bt1_limpar", "Limpar"),
            style = "display: flex; justify-content: space-between;"
          ),
          
          criarCheckboxGroup(
            'bt1', 
            'Situação', 
            choiceValues = unique(municipios_pe$Situação),
            choiceNames = unique(municipios_pe$Situação), 
            selected = unique(municipios_pe$Situação)
          ),  # fecha checkboxGroupInput
          circle = F,
          label = 'Situação',
          size = 'sm',
          status = "danger",
          icon = icon("list"), 
          width = "100%",
          right = T,
          tooltip = tooltipOptions(title = "Situação")
        ) # fecha dropdown
      )
      
      ) # fecha div geral do box
    ), # fecha box
  
  tabItems(
    tabItem(tabName = "geral",
  
  fluidRow(
  valueBoxOutput('municipios', width = '3'),
  valueBoxOutput('pessoas', width = '3'),
  valueBoxOutput('ocp', width = '3'),
  valueBoxOutput('seca1', width = '3')
  ),
       
  fluidRow(
    leafletOutput("mapa1")
  ),
  
  absolutePanel(
    class = "large-screen-only",
    fixed = F,
    top = "40%",
    left = "auto",
    right = "10%",
    bottom = "auto",
    width = 330, 
    height = "auto",
    uiOutput('municipios2'), 
    draggable = T)
  
), # fecha tab "geral"

tabItem(
  tabName = "iniciativas",
  
  fluidRow(
    valueBoxOutput('total_iniciativa', width = '4'),
    valueBoxOutput('pessoas_iniciativa', width = '4'),
    valueBoxOutput('valor_estimado', width = '4'),
  ),
  
  fluidRow(
    tabsetPanel(
      id = "tabset",
      tabPanel(
        "Linha do Tempo", 
        plotOutput("timeline1")),
      tabPanel(
        "Mapa", 
        leafletOutput("mapa2")),
    ))

), # fecha tab "iniciativas"        

  tabItem(tabName = "tb_ocp",
          
          fluidRow(
          plotlyOutput('dispersao1')
          )
  )

) # fecha tab tabItems

) # fecha corpo dashboardBody

ui <- dashboardPage(
  header,
  sidebar,
  body
)

#### secure_app #### 

ui <- shinymanager::secure_app(
  ui, 
  enable_admin = TRUE,
  language = "pt-BR")

#### servidor #### 

server <- function(input, output, session) {
  
  #### Autenticação ####
  
  ### SQLite ###
  
  secure_server(
    check_credentials = check_credentials(
      "../credentials.sqlite",
      passphrase = "shiny_123"
      )
  )
  
  # secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  
  ### Planilha ###
  
  # shinymanager::secure_server(
  #   check_credentials = shinymanager::check_credentials(credentials)
  # )
  
  #### Navbar ####
  
  #### Sidebar ####
  
  observeEvent(input$bt1_todos, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt1",
      selected = unique(municipios_pe$Situação)
    )
  })
  
  observeEvent(input$bt1_limpar, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt1",
      selected = character(0)
    )
  })
  
  observeEvent(input$bt2_todos, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt2",
      selected = unique(municipios_pe$`Seca Outubro`)
    )
  })
  
  observeEvent(input$bt2_limpar, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt2",
      selected = character(0)
    )
  })
  
  observeEvent(input$bt3_todos, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt3",
      selected = unique(iniciativas_estiagem$SECRETARIA)
    )
  })
  
  observeEvent(input$bt3_limpar, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt3",
      selected = character(0)
    )
  })
  
  observeEvent(input$bt4_todos, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt4",
      selected = unique(iniciativas_estiagem$`FONTE RECURSO`)
    )
  })
  
  observeEvent(input$bt4_limpar, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt4",
      selected = character(0)
    )
  })
  
  observeEvent(input$bt5_todos, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt5",
      selected = unique(iniciativas_estiagem$`TIPO DE AÇÃO`)
    )
  })
  
  observeEvent(input$bt5_limpar, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt5",
      selected = character(0)
    )
  })
  
  observeEvent(input$bt6_todos, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt6",
      selected = unique(iniciativas_estiagem$STATUS)
    )
  })
  
  observeEvent(input$bt6_limpar, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt6",
      selected = character(0)
    )
  })
  
  observeEvent(input$bt8_todos, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt8",
      selected = unique(iniciativas_estiagem$iniciativa_nome)
    )
  })
  
  observeEvent(input$bt8_limpar, {
    shinyWidgets::updateCheckboxGroupButtons(
      session,
      inputId = "bt8",
      selected = character(0)
    )
  })
  
  #### Bases ####
  
  municipio_pe_original <- reactiveVal(municipios_pe)
  
  municipio_pe_filtro <- debounce(reactive({
    municipios_pe %>% dplyr::filter(Situação %in% input$bt1, `Seca Outubro` %in% input$bt2)
  }), 600)
  
  iniciativas_estiagem_filtro <- debounce(
    reactive({
    iniciativas_estiagem  %>% dplyr::filter(
      SECRETARIA %in% input$bt3, 
      `FONTE RECURSO` %in% input$bt4, 
      `TIPO DE AÇÃO` %in% input$bt5,
      STATUS %in% input$bt6,
      `VALOR ESTIMADO` >= input$valor_estimado_f[1]*1000 & `VALOR ESTIMADO` <= input$valor_estimado_f[2]*1000
      )
  }), 600)
  
  mapa_iniciativas_mun_filtro <- debounce(reactive({
    mapa_iniciativas_mun  %>% dplyr::filter(
      SECRETARIA %in% input$bt3, 
      `FONTE RECURSO` %in% input$bt4, 
      `TIPO DE AÇÃO` %in% input$bt5,
      STATUS %in% input$bt6,
      iniciativa_nome %in% input$bt8,
      `VALOR ESTIMADO` >= input$valor_estimado_f[1]*1000 & `VALOR ESTIMADO` <= input$valor_estimado_f[2]*1000
    )
  }), 600)
  
  #### Tab Geral ####
  
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
      addProviderTiles("OpenStreetMap.Mapnik") %>% 
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
            "font-size" = "16px",
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
            "font-size" = "16px",
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
            'Pessoas Afetadas (Taxa)',
            sep =': ')),
        labelOptions = labelOptions(
          noHide = F, 
          direction = "bottom",
          style = list(
            "font-family" = "serif",
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "16px",
            "border-color" = "rgba(0,0,0,0.5)")
        ),
        group = "Municípios OCP"
      ) %>% 
      addLegend(
        pal = fpal_ocp,
        values = ~OCP,
        opacity = 0.7,
        title = 'Municípios OCP',
        position = "bottomleft",
        layerId  = "Municípios OCP"
      ) %>% 
      addPolygons( # PESSOAS OCP TAXA
        fillColor = ~bpal_pessoas_ocp_taxa(pessoas_ocp_tx),
        fillOpacity = 0.8,
        weight = 0.5,
        opacity = 1,
        color = "white",
        label = ~htmlEscape(
          paste(
            NM_MUN, " | ",
            Situação, " | ",
            scales::percent(round(pessoas_ocp_tx, 1), decimal.mark = ","), 
            ' Pessoas OCP (Taxa)',
            sep ='')),
        labelOptions = labelOptions(
          noHide = F, 
          direction = "bottom",
          style = list(
            "font-family" = "serif",
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "16px",
            "border-color" = "rgba(0,0,0,0.5)")
        ),
        group = "Pessoas OCP (Taxa)"
      ) %>% 
      addLegend(
        pal = bpal_pessoas_ocp_taxa,
        values = ~pessoas_ocp_tx,
        opacity = 0.7,
        title = 'Pessoas OCP (Taxa)',
        position = "bottomleft",
        layerId  = "Pessoas OCP (Taxa)"
      ) %>%
      addPolygons( # BASE DO MAPA
        data = municipio_pe_original(),
        fillColor = ~fpal_camada(camada),
        fillOpacity = 0.0,
        weight = 2,
        opacity = 1,
        color = "white",
        group = "BASE"
      ) %>% # BASE DO MAPA
      addLegend(
        pal = fpal_camada,
        values = ~camada,
        opacity = 0.7,
        title = 'COMPESA',
        position = "bottomleft",
        layerId  = "BASE"
      ) %>% 
      addLayersControl(
        overlayGroups = c(
          'BASE'),
        baseGroups = c(
          "Situação por Região",
          "Pessoas Afetadas (Taxa)",
          'Municípios OCP',
          'Pessoas OCP (Taxa)'
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
  
  #### Tab Iniciativas ####
  
  output$total_iniciativa <- renderValueBox({
    infoBox(
      tags$p(div("Total", br(), "Iniciativas",  style = "font-size: 90%;")),
      iniciativas_estiagem_filtro() %>% distinct(iniciativa_nome) %>% tally(), 
      icon = icon("person-digging"),
      color = "olive",
      fill= T
    )
  })
  
  output$pessoas_iniciativa <- renderValueBox({
    infoBox(
      tags$p(div("Pessoas", br(), "Beneficiadas",  style = "font-size: 90%;")),
      iniciativas_estiagem_filtro() %>% distinct(iniciativa_nome, `PESSOAS BENEFICIADAS`) %>% summarise(Total = sum(`PESSOAS BENEFICIADAS`,na.rm = T)) %>% pull(Total) %>% formatC(
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
      iniciativas_estiagem_filtro() %>% distinct(iniciativa_nome, `VALOR ESTIMADO`) %>% summarise(Total = sum(`VALOR ESTIMADO`,na.rm = T)) %>% pull(Total) %>% MillionForm(), 
      icon = icon("money-bill-trend-up"),
      color = "blue",
      fill= T
    )
  })
  
  output$timeline1 <- renderPlot({
    
    iniciativas_estiagem_filtro() %>% vistime::gg_vistime(
      col.event = "tipo_etapa",
      col.group = "iniciativa_nome",
      col.color = 'cor_etapa',
      #title = "Linha do Tempo",
      optimize_y = T
      , linewidth = 7
      ) + theme(
        text = element_text(size = 16),
        axis.text = element_text(size = 13),
        panel.background = element_rect(fill = 'NA'),
        panel.ontop = FALSE
      ) + scale_x_datetime(
        date_labels = "%y %m",
        position = 'top',
        breaks = 'month') + ggtitle(paste("Linha do Tempo de: ", paste(input$bt3, collapse = ", ")))
  })
  
  output$mapa2 <- renderLeaflet({
    mapa_iniciativas_mun_filtro() %>%
      leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>% 
      addPolygons(
        fillColor = "#5bc0de",
        fillOpacity = 0.4,
        color = "white",
        weight = 1
      )
  })
  
  ### dispersão adequação ocp
  
  output$dispersao1 <- renderPlotly({
    limites <- c(0, max(municipio_pe_filtro()$`População Afetada`))
    breaks <- seq(0, limites[2], by = 10000)
    
    graf_disp <- municipio_pe_filtro() %>% 
      ggplot(aes(x=`População Afetada`, y=`População At. OCP`, label = NM_MUN)) + 
      geom_point(aes(color = adequacao_ocp)) + 
      geom_smooth(aes(color = "Smooth Line")) + 
      geom_abline(
        intercept = 0, 
        slope = 1, 
        color = "#6f42c1", 
        linetype = "dashed") + 
      scale_x_continuous(limits = limites, breaks = breaks) +
      scale_y_continuous(limits = limites, breaks = breaks) +
      scale_color_manual(values = c("Sim" = "#43ac6a", "Não" = "#f04124", "Smooth Line" = "black")) + 
      theme(legend.position = "none")
    
    graf_disp
    
    plotly::ggplotly(graf_disp, tooltip = "label")
  })
  
} # fecha o servidor

shinyApp(ui = ui, server = server, options = list(port = 4073))