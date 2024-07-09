#' @example
#' # example code
#' get_wrsi(
#' continent = "africa",
#' product = "chirps",
#' crop = "maize",
#' region = "south",
#' year = 2001,
#' dekad = 28
#' )
 
get_wrsi <- function(
  continent = "africa", 
  product = "rfe", 
  region = "west",
  crop = "rangeland",
  growing_period = NULL,
  year = 2024,
  dekad = 13,
  path = NULL
) {

  rg <- .get_rg(region = region, crop = crop)

  if (is.na(rg)) cli::cli_abort(message = "can't find geo-wrsi region code using region = {region} and crop = {crop}. See details.", .envir = rlang::caller_env())
  
  url <- .build_url(product = product, continent = continent, region = region, year = year, dekad = dekad, crop = crop, rg = rg)
  url_res <- httr::GET(url)
  if (identical(httr::status_code(url_res), 200L)) {

  curl::curl_download(
    url = url,
    destfile = ifelse(
      is.null(path),
      here::here(basename(url)),
      normalizePath(paste0(path,basename(url),collapse = "/"),mustWork = TRUE)

    )
  )

  } else {
    cli::cli_abort(message = "the required dataset can not be reached, or is not yet supported. Read the documentation.")
  }

}



