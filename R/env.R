
sits.validate.env <- new.env()
sits.validate.env$base_dir <- "/Users/pedro/TWDTWAmazoniaCerrado/"
sits.validate.env$crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

sits.validate.env$classes_sits <- c(
    "1.  Araguaia",
    "2.  Campo_Cerrado",
    "3.  Cerradao",
    "4.  Cerrado",
    "5.  Cerrado_Rupestre",
    "6.  Dunas",
    "7.  Fallow_Cotton",
    "8.  Millet_Cotton",
    "9.  Pasture",
    "10. Soy_Corn",
    "11. Soy_Cotton",
    "12. Soy_Fallow",
    "13. Soy_Millet",
    "14. Sugarcane",
    "15. Urban Area",
    "16. Water"
)

sits.validate.env$classificacao_tc <- c(
    "1.  Agricultura anual",
    "2.  Agricultura perene",
    "3.  Corpo d'agua",
    "4.  Area urbanizada",
    "5.  Natural",
    "6.  Mineracao",
    "7.  Mosaico de ocupacoes",
    "8.  Nao observado",
    "9.  Natural nao vegetado",
    "10. Outros",
    "11. Pastagem",
    "12. Silvicultura",
    "13. Solo exposto"
)

sits.validate.env$classificacao_tc_simplificada <- c(
    "1Agrican",
    "2Agricpe",
    "3Agua",
    "4Urbana",
    "5Natural",
    "6Miner",
    "7Mosaico",
    "8Noobser",
    "9Natnoveg",
    "10Outros",
    "11Pasto",
    "12Silvic",
    "13Solo"
)

sits.validate.env$classes_mask <- c(
    "0. Fora",
    "1. Dentro"
)

#' @title Base path
#' @description Return a path according to the base directory of the package.
#' @param path A path that within the base directory.
#' @seealso baseDir
#' @export
basePath <- function(path) paste0(sits.validate.env$base_dir, path)

#' @title Get or set base directory
#' @description Set the base directory (if the argument is used).
#' It returns the current base directory.
#' All the data used internally by the package are stored in this path. It is a
#' Dropbox folder that cannot be stored in GitHub due to its size. If you do not
#' have access to this folder please contact the package's maintainer.
#' @param dir A directory.
#' @export
baseDir <- function(dir = NULL){
    if (!is.null(dir))
        sits.validate.env$base_dir <-  dir

    return(sits.validate.env$base_dir)
}
