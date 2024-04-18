#### preparação ####
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

# caminhoDados <- 'estiagem/dados/'
# caminhoMapas <- 'estiagem/mapas/'

caminhoDados <- 'dados/'
caminhoMapas <- 'mapas/'

#### credenciais ####
credentials <- readxl::read_excel(paste(caminhoDados, 'credenciais.xlsx', sep = ''))

#### mapa geral ####
### PASSO 1 ###
## carregar o mapa geral ##
municipios_pe_shp <- sf::st_read(paste(caminhoMapas, "municipios_pe_shp.shp", sep = '')) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

#### iniciativas ####
### PASSO 2 ###
##  carregar e tratar base de iniciativas ##
iniciativas_estiagem_raw <- readxl::read_excel(paste(caminhoDados, 'iniciativas.xlsx', sep = '')) %>%
  rename(
    contratação = `PREVISÃO O.S`, 
    serviços = `CONCLUSÃO DOS SERVIÇOS`) %>% mutate(
  MUNICÍPIOS = gsub(";", ",", MUNICÍPIOS),
  MUNICÍPIOS = gsub(" e ", ",", MUNICÍPIOS),
  `VALOR ESTIMADO` = `VALOR PREVISTO 2023`
)  

iniciativas_mun_raw <- iniciativas_estiagem_raw %>%
  separate_longer_delim(
    c(MUNICÍPIOS), 
    delim = ",")

iniciativas_mun_raw <- iniciativas_mun_raw %>% 
  mutate(
    MUNICÍPIOS = toupper(str_trim(MUNICÍPIOS))
    ) # remover white space e colocar em maiúsculas

### PASSO 3 ###
##  carregar base de conversão ##
conversor_municipios <- readxl::read_excel(paste(caminhoDados, 'conversor_municipios.xlsx', sep = ''))

### PASSO 4 ###
##  juntar iniciativas mun e conversão ##
iniciativas_mun  <- iniciativas_mun_raw %>% left_join(
  conversor_municipios, 
  by = c("MUNICÍPIOS"="mun_nm_raw")
  ) %>% mutate(
    mun_cod = as.character(mun_cod)
  )

### PASSO 5 ### 
## juntar mapa e base de iniciativas mun
mapa_iniciativas_mun  <- municipios_pe_shp %>% left_join(
  iniciativas_mun, 
  by = c("CD_MUN"="mun_cod")
  )

### PASSO 6 ###
## base geral de iniciativas
iniciativas_estiagem_raw <- iniciativas_estiagem_raw %>%
  pivot_longer(
    cols = c('contratação', 'serviços'),
    names_to = "tipo_etapa",
    values_to = "PREVISÃO DE CONCLUSÃO",
    values_drop_na = TRUE
  ) %>% mutate(
    `DATA DE INÍCIO OS` = as.character(`DATA DE INÍCIO OS`),
    `DATA DE INÍCIO SV` = as.character(`DATA DE INÍCIO SV`),
    `DATA DE INÍCIO` = ifelse(
      tipo_etapa == 'contratação', `DATA DE INÍCIO OS`, `DATA DE INÍCIO SV`
    ) %>% as.Date()
  )

iniciativas_estiagem <- iniciativas_estiagem_raw %>% 
  mutate(
    AGRUPAMENTO = as.factor(AGRUPAMENTO),
    `DATA DE INÍCIO` = as.Date(`DATA DE INÍCIO`),
    `PREVISÃO DE CONCLUSÃO` = as.Date(`PREVISÃO DE CONCLUSÃO`),
    cor_etapa = case_when(
      tipo_etapa == 'contratação' ~ "#D1E5F0",
      tipo_etapa == 'serviços' ~ "#4393C3",
      .default = "#92C5DE"
    ),
    cor_grupo = case_when(
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

#### indicadores ####
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

######### OCP
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
  `Volume de água (L) por mês OCP` = if_else(is.na(`Volume de água (L) por mês OCP`), 0, `Volume de água (L) por mês OCP`),
  adequacao_ocp = ifelse(`População At. OCP` >= `População Afetada`, 'Sim', 'Não')
)
