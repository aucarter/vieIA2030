#' Demography data derived from UN World Population Prospects
#' @format A list with three dataframes:
#' \describe{
#'      \item{population}{population data for the past and future}
#'      \item{fertility}{fertility data for the past and future}
#'      \item{life_tables}{mx, px, and ax for the past and future}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Population/}
"demog"

#' Vaccine coverage data from the WHO
#' @format A data.table with vaccine data...
#' \describe{
#'      \item{vaccine_short}{the code for for the vaccine}
#'      \item{value}{percentage coverage from zero to 100}
#'      ...
#' }
#' @source \url{https://www.who.int/immunization/monitoring_surveillance/data/en/}
"coverage"

#' VIMC vaccine impact estimates
#' @format A data.table with vaccine impacts estimates from VIMC...
#' \describe{
#'      \item{vaccine_short}{the code for for the vaccine}
#'      \item{value}{deaths averted from the vaccine}
#'      ...
#' }
#' @source \url{https://montagu.vaccineimpact.org/}
"vimc_impact"