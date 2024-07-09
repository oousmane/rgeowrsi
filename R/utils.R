# build zip file name
.path_to_zip <- function(
  year = year,
  dekad = dekad,
  rg = rg,
  continent = continent,
  product = product) {
  # can't handle historical folder
if ((continent == "africa" & product == "chirps")){
  paste0(
    "w",
    year,
    stringr::str_pad(dekad, width = 2,pad = "0"),
    rg,
    ".zip") 
  } else {
  paste0(
  "w",
  substr(year,3,4),
  stringr::str_pad(dekad, width = 2,pad = "0"),
  rg,
  ".zip"
  )
    }
}

# get wrsi-region rg code 

.get_rg <- function(region = region, crop = crop){
  if (!grepl("east",region)) {

    rg <- dplyr::case_when(
      grepl("west",region) & crop == "rangeland" ~ 'w1',
      grepl("west",region) & crop == "millet" ~ 'wa',
      grepl("south",region) & crop == "maize" ~ 'sa',
      .default = NA_character_
    )

  } else {

    if (is.null(growing_period)) cli::cli_abort(message = "if region = {region}, the argument `growing_period` should be defined.")
    rg <- dplyr::case_when(
      growing_period == "Mar-Nov" & crop == "maize" ~ 'ee',
      growing_period == "Oct-Feb" & crop == "maize" ~ 'et',
      growing_period == "Mar-Sep" & crop == "sorghum" ~ 'ek',
      growing_period == "Mar-Nov" & crop == "sorghum" ~ 'el',
      growing_period == "Sep-Jan" & crop == "rangeland" ~ 'e1',
      growing_period == "Feb-Jul" & crop == "rangeland" ~ 'e2',
      .default = NA_character_
    )
  }
  
  return(rg)

}

# build the final link to the zip file

.build_url <- function(product = product, continent = continent, region = region, year = year, dekad = dekad,crop = crop, rg = rg){
    
  product <- tolower(product)
  continent <- tolower(continent)
  region <- tolower(region)
  crop <- tolower(crop)

  region <- dplyr::case_when(
    product == "rfe" & continent == "africa" ~ paste0("africa_",region),
    product == "chirps" & continent == "africa" ~ ifelse(region == "south", "southern", region)
  )
  base_url <- dplyr::case_when(
    continent == "africa" & product == "rfe" ~ glue::glue("https://edcftp.cr.usgs.gov/project/fews/dekadal/{region}/"),
    continent == "africa" & product == "chirps" ~ glue::glue("https://edcftp.cr.usgs.gov/project/fews/africa/{region}/dekadal/wrsi-chirps-etos/{region}/")
  )

  paste0(base_url, .path_to_zip(year = year,dekad = dekad,rg = .get_rg(region = region,crop = crop),continent = continent, product = product))
  
}
