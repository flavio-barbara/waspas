#' Complete data set
#'
#' Small, artificially generated Choppers dataset
#' it is Excel spreadsheet (.xlsx) format
#' Chopper's names are real, but the other data are artificially created
#'
#' @docType .csv
#'
#' @usage read.csv("./data/WASPAS_Data_choppers.csv", header = FALSE, sep = ",", quote = "\"",dec = ",")
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{ccode}{ISO3 country code (as character) for the countries in the
#'      sample (Angola, Benin, France, Rwanda, and UK)}
#'  \item{year}{A value between 1990 and 1999}
#'  \item{month}{An abbreviation (MMM) for month (character)}
#'  \item{gpd}{A fake value for GDP (randomly generated)}
#'  \item{population}{A fake value for population (randomly generated)}
#' }
#' @references This data set was artificially created for the waspas package.
#' @keywords datasets
#' @examples
#'
#' to be done!
#'
"choppers"
