pacman::p_load(
  # BASE
  tidyverse, 
  # GRÁFICOS
  plotly, 
  #vistime,
  # MAPAS
  htmlwidgets, htmltools, leaflet, leaflet.extras, sf,
  # TEXTO
  stringr)

caminhoDados <- 'dados/'
caminhoMapas <- 'mapas/'

######### MUNICÍPIOS DAS INICIATIVAS
iniciativas_estiagem_raw <- readxl::read_excel(paste(caminhoDados, 'BANCO DE INICIATIVAS_GT DA ESTIAGEM_V3.xlsx', sep = ''))

iniciativas_estiagem_mun <- iniciativas_estiagem_raw %>%
  separate_longer_delim(
    c(MUNICÍPIOS), 
    delim = ",")

iniciativas_estiagem_mun <- iniciativas_estiagem_mun %>% mutate(
  MUNICÍPIOS = toupper(str_trim(MUNICÍPIOS))
) # remover white space e colocar em maiúsculas

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

########## MAPA ##########
## PASSO 1 - CARREGAR O MAPA
municipios_pe_shp <- sf::st_read(paste(caminhoMapas, "municipios_pe_shp.shp", sep = '')) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

## PASSO 2 - CARREGAR BASE IBGE
municipios_pe_tab <- readxl::read_excel(paste(caminhoDados, 'municipos_pe_indicadores_basicos.xlsx', sep = '')) %>% filter(ano==2021) %>% mutate(mun_cod = as.character(mun_cod))

## PASSO 3 - JUNTAR MAPA E BASE IBGE
municipios_ibge  <- left_join(municipios_pe_shp, municipios_pe_tab, by = c("CD_MUN"="mun_cod"))

## PASSO 4 - CARREGAR BASE DE INTERESSE
municipios_estiagem <- readxl::read_excel(paste(caminhoDados,'Panorama Situação Emergencial Municípios_ESTIAGEM NOV23.xlsx', sep = ''), sheet = 'base') %>% mutate(mun_cod = as.character(mun_cod))

## PASSO 5 - JUNTAR MAPA E BASE IBGE
municipios_pe <- left_join(municipios_ibge, municipios_estiagem, by = c("CD_MUN"="mun_cod"))

municipios_pe <- municipios_pe %>%
  mutate(
    OCP = as.factor(OCP),
    situacao_regiao = ifelse(
      Situação %in% c('Aguardando Análise', 'Reconhecido'), meso_regiao_nm, 'Sem emegência'
      ),
    pessoas_afetadas_tx = `Pessoas Afetadas` / populacao
  )


