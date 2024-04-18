pacman::p_load(
  # BASE
  tidyverse, 
  # ETL
  googlesheets4,
  # GRÁFICOS
  plotly, 
  #vistime,
  # MAPAS
  htmlwidgets, htmltools, leaflet, leaflet.extras, sf,
  # TEXTO
  stringr)

##################### ETL #####################

caminhoDados <- 'estiagem/dados/'
caminhoMapas <- 'estiagem/mapas/'

######### MUNICÍPIOS DAS INICIATIVAS
iniciativas_estiagem_raw <- readxl::read_excel(paste(caminhoDados, 'BANCO DE INICIATIVAS_GT DA ESTIAGEM_V3.xlsx', sep = ''))

iniciativas_estiagem_mun <- iniciativas_estiagem_raw %>%
  separate_longer_delim(
    c(MUNICÍPIOS), 
    delim = ",")

iniciativas_estiagem_mun <- iniciativas_estiagem_mun %>% mutate(
  MUNICÍPIOS = toupper(str_trim(MUNICÍPIOS))
) # remover white space e colocar em maiúsculas

iniciativas_estiagem_mun %>% distinct(MUNICÍPIOS) %>% writexl::write_xlsx("estiagem/dados/iniciativas_estiagem_mun.xlsx")

iniciativas_estiagem <- iniciativas_estiagem_raw %>% 
  mutate(
    AGRUPAMENTO = as.factor(AGRUPAMENTO),
    `DATA DE INÍCIO` = as.Date(`DATA DE INÍCIO`),
    `PREVISÃO DE CONCLUSÃO` = as.Date(`PREVISÃO DE CONCLUSÃO`),
    cor = case_when(
      AGRUPAMENTO == 'ABASTECIMENTO' ~ "#92C5DE",
      AGRUPAMENTO == 'AGRICULTURA' ~ "#4393C3",
      AGRUPAMENTO == 'ASSISTENCIAL' ~ "#F4A582",
      AGRUPAMENTO == 'DESSALINIZADORES' ~ "#FDDBC7",
      AGRUPAMENTO == 'POÇOS' ~ "#D1E5F0",
      AGRUPAMENTO == 'OUTROS' ~ "#D6604D",
      .default = "#D6604D"
    ),
    id = ITEM, 
    content = toupper(iniciativa_nome),
    start = `DATA DE INÍCIO` ,
    end = `PREVISÃO DE CONCLUSÃO`
  ) %>% arrange(AGRUPAMENTO)

#### MAPA ####
## PASSO 1 - CARREGAR O MAPA
municipios_pe_shp <- sf::st_read(paste(caminhoMapas, "municipios_pe_shp.shp", sep = '')) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

## PASSO 2 - CARREGAR BASE IBGE
municipios_pe_tab <- readxl::read_excel(paste(caminhoDados, 'municipos_pe_indicadores_basicos.xlsx', sep = '')) %>% filter(ano==2021) %>% mutate(mun_cod = as.character(mun_cod))

## PASSO 3 - JUNTAR MAPA E BASE IBGE
municipios_ibge  <- left_join(municipios_pe_shp, municipios_pe_tab, by = c("CD_MUN"="mun_cod"))

## PASSO 4 - CARREGAR BASE DE INTERESSE
municipios_estiagem <- readxl::read_excel(paste(caminhoDados,'Panorama Situação Emergencial Municípios_ESTIAGEM NOV23.xlsx', sep = ''), sheet = 'base') %>% mutate(mun_cod = as.character(mun_cod))

## PASSO 5 - JUNTAR MAPA E BASE ESTIAGEM
municipios_pe <- left_join(municipios_ibge, municipios_estiagem, by = c("CD_MUN"="mun_cod"))

municipios_pe <- municipios_pe %>%
  mutate(
    OCP = as.factor(OCP),
    situacao_regiao = ifelse(
      Situação %in% c('Aguardando Análise', 'Reconhecido'), meso_regiao_nm, 'Sem emegência'
    ),
    pessoas_afetadas_tx = `Pessoas Afetadas` / populacao
  )

#### OCP ####
library(googledrive)

ocp_temp <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1qd5sL3Za9Z1osS0lRU0qkThTp8ObiEXd/edit#gid=1950702611"),
  path = 'estiagem/dados/OCP.xlsx', 
  overwrite = TRUE, 
  type = "xlsx")

ocp_raw <- readxl::read_excel(paste(caminhoDados, 'OCP.xlsx', sep = '')) %>% 
  mutate(
   `Código IBGE` = as.character(`Código IBGE`) 
  )

## PASSO 6 - JUNTAR MAPA E BASE OCP
municipios_pe <- left_join(
  municipios_pe, 
  ocp_raw, 
  by = c("CD_MUN"="Código IBGE")
  ) %>% mutate(
    camada = 'Sim',
    `Situação OCP` = if_else(is.na(`Situação OCP`), "NI", `Situação OCP`),
    Pipeiros = if_else(is.na(Pipeiros), 0, Pipeiros),
    `População At. OCP` = if_else(is.na(`População At. OCP`), 0, `População At. OCP`),
    `População Afetada` = if_else(is.na(`População Afetada`), 0, `População Afetada`),
    `Percentual de atendidos entre os afetados` = if_else(is.na(`Percentual de atendidos entre os afetados`), 0, `Percentual de atendidos entre os afetados`),
    pessoas_ocp_tx = ifelse(`Percentual de atendidos entre os afetados` > 1, 1, `Percentual de atendidos entre os afetados`),
    `Volume de água (L) por mês OCP` = if_else(is.na(`Volume de água (L) por mês OCP`), 0, `Volume de água (L) por mês OCP`)
  )

##################### OBJETOS #####################
######### MAPAS
municipios_pe %>%
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
    fillColor = ~fpal_camada(camada),
    fillOpacity = 0.1,
    weight = 0.1,
    opacity = 0.1,
    color = "#dee2e6",
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

limites <- c(0, max(municipios_pe$`População Afetada`))
breaks <- seq(0, limites[2], by = 10000)

graf_disp <- municipios_pe %>% 
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
