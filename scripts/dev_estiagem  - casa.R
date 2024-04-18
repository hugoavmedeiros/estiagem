pacman::p_load(plotly, timevis, tidyverse, vistime)

iniciativas_estiagem_raw <- readxl::read_excel('estiagem/dados/BANCO DE INICIATIVAS_GT DA ESTIAGEM.xls')

iniciativas_estiagem_raw %>% glimpse()

iniciativas_estiagem <- iniciativas_estiagem_raw %>% 
  mutate(
    AGRUPAMENTO = as.factor(AGRUPAMENTO),
    id = ITEM, 
    content = iniciativa_nome,
    start = today(),
    end = `PREVISÃO DE CONCLUSÃO`
  ) %>% arrange(AGRUPAMENTO)

timevis(
  iniciativas_estiagem) %>%
  setOptions(list(height = "1000px"))

#####
p <- vistime(
  iniciativas_estiagem, 
  col.event = "content", 
  col.group = "AGRUPAMENTO", 
  title = "Estiagem em Pernambuco",
  optimize_y = TRUE, 
  linewidth = 7)

pp <- plotly::plotly_build(p)

# step 2: change the font size
pp$x$layout$xaxis$tickfont <- list(size = 14)

pp

gg_vistime(
  iniciativas_estiagem, 
  col.event = "content", 
  col.group = "AGRUPAMENTO", 
  title = "Estiagem em Pernambuco",
  optimize_y = TRUE, linewidth = 7) + theme(
    axis.text = element_text(size = 12),
    panel.background = element_rect(fill = 'NA'),
    panel.grid.minor = element_line(colour = "blue"),
    panel.ontop = FALSE
    )

######################## MAPA ########################
yeti_warning <- c('#f04124','#e99002','#43ac6a')
yeti_warning_neg <- c('white', '#43ac6a','#e99002','#f04124')

fpal_situacao_regiao <- colorFactor(c("#cfe902", "yellow", '#e99002', "white", '#e99002'), municipios_pe$situacao_regiao)

bins100 <- c(0, 0.01, 0.25, 0.50, 0.75, 1)

bpal_pessoas_afetadas_taxa <- colorBin(yeti_warning_neg, domain = (municipios_pe$pessoas_afetadas_tx), bins = bins100)

municipios_pe %>%
  leaflet() %>%
  addTiles() %>% 
  addPolygons( # SITUAÇÃO POR REGIÃO
    fillColor = ~fpal_situacao_regiao(situacao_regiao),
    fillOpacity = 0.8,
    weight = 0.5,
    opacity = 1,
    color = "white",
    label = ~htmlEscape(paste(NM_MUN, Situação, `Pessoas Afetadas`, sep =': ')),
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
    label = ~htmlEscape(paste(NM_MUN, Situação, scales::percent(round(pessoas_afetadas_tx, 1), decimal.mark = ","), sep =': ')),
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
  addLayersControl(
    baseGroups = c(
      "Situação por Região",
      "Pessoas Afetadas (Taxa)"
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

