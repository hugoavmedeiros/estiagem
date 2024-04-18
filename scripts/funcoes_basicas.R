#### JavaScript ####
js_filter <- JS("function(rows, columnId, filterValue) {
        const pattern = new RegExp(filterValue, 'i')

        return rows.filter(function(row) {
          return pattern.test(row.values[columnId])
        })
      }")

js_match_style <- JS(
  "function(cellInfo) {
    try {
      let filterValue = cellInfo.filterValue
      let pattern = filterValue.split(' ').filter(n => n).sort().join('|')
      let regexPattern = new RegExp('(' + pattern + ')', 'gi')
      let replacement = '<span style=\"background: #FFC107; color:black; text-decoration: underline; font-weight:bold;\">$1</span>'
      return cellInfo.value.replace(regexPattern, replacement)
    } catch(e) {
        return cellInfo.value
    }
  }"
)

#### Shiny Widgets ####

criarCheckboxGroup <- function(id, label, choiceValues, choiceNames, selected) {
  checkboxGroupButtons(
    inputId = id,
    label = "Selecione as opções:",
    choiceValues = choiceValues,
    choiceNames = choiceNames, 
    selected = selected,
    individual = T,
    direction = "vertical",
    status = "info",
    checkIcon = list(
      yes = icon("ok", lib = "glyphicon"),
      no = icon("remove", lib = "glyphicon")))
}

#### Mapas ####

yeti_warning <- c('white', '#f04124','#e99002','#43ac6a')
yeti_warning_neg <- c('white', '#43ac6a','#e99002','#f04124')

fpal_situacao_regiao <- colorFactor(c("#cfe902", "yellow", '#e99002', "white", '#e99002'), municipios_pe$situacao_regiao)

fpal_ocp <- colorFactor(c("#f04124", "white", '#43ac6a'), municipios_pe$OCP)

bins100 <- c(0, 0.01, 0.25, 0.50, 0.75, 1)

bpal_pessoas_afetadas_taxa <- colorBin(yeti_warning_neg, domain = (municipios_pe$pessoas_afetadas_tx), bins = bins100)

bpal_pessoas_ocp_taxa <- colorBin(yeti_warning, domain = (municipios_pe$pessoas_ocp_tx), bins = bins100)

fpal_camada <- colorFactor(c("grey", "white"), municipios_pe$camada)

#### Formatação de números / textos ####

MillionForm <- scales::dollar_format(prefix = "R$ ", big.mark = '.', decimal.mark = ',')