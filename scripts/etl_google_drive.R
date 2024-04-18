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

#### INICIATIVAS ####
library(googledrive)

iniciativas_temp <- drive_download(
  as_id("https://docs.google.com/spreadsheets/d/14xnZ8wgLWdr2uepLpJI23I2MqgkXVdXq/edit#gid=241680426"),
  path = 'estiagem/dados/iniciativas.xlsx', 
  overwrite = TRUE, 
  type = "xlsx")

iniciativas_raw <- readxl::read_excel('estiagem/dados/iniciativas.xlsx')
