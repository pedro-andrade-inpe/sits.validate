
sits.validate.env <- new.env()
#sits.validate.env$base_dir will be created by zzz.R

sits.validate.env$crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

sits.validate.env$classes_sits <- c(
    "01.Araguaia",
    "02.Campo_Cerrado",
    "03.Cerradao",
    "04.Cerrado",
    "05.Cerrado_Rupestre",
    "06.Dunas",
    "07.Fallow_Cotton",
    "08.Millet_Cotton",
    "09.Pasture",
    "10.Soy_Corn",
    "11.Soy_Cotton",
    "12.Soy_Fallow",
    "13.Soy_Millet",
    "14.Sugarcane",
    "15.Urban Area",
    "16.Water"
)

sits.validate.env$classificacao_tc <- c(
    "01.Agricultura anual",
    "02.Agricultura perene",
    "03.Corpo d'agua",
    "04.Area urbanizada",
    "05.Natural",
    "06.Mineracao",
    "07.Mosaico de ocupacoes",
    "08.Nao observado",
    "09.Natural nao vegetado",
    "10.Outros",
    "11.Pastagem",
    "12.Silvicultura",
    "13.Solo exposto"
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
#' @seealso setBaseDir
#' @export
baseDir <- function(path){
  paste0(sits.validate.env$base_dir, "/", path) %>% normalizePath(mustWork = FALSE)
}

#' @title Set base directory
#' @description Set the base directory (if the argument is used).
#' It returns the current base directory.
#' All the data used internally by the package are stored in this path. It is a
#' Dropbox folder that cannot be stored in GitHub due to its size. If you do not
#' have access to this folder please contact the package's maintainer.
#' @param dir A directory.
#' @seealso baseDir
#' @export
setBaseDir <- function(dir = NULL){
    if (!is.null(dir))
        sits.validate.env$base_dir <- dir

    return(sits.validate.env$base_dir)
}

#' @title Sits validate environment
#' @description Return the environment of sits.validate, with variables such as
#' base directory, and projection used internally.
#' @export
getSitsValidateEnv <- function() sits.validate.env

#' @title Tif files within a base directory
#' @description Returns a vector with all tif files within a directory inside base directory.
#' @param dir A directory inside base directory.
#' @export
getTifFiles <- function(dir){
  list.files(baseDir(dir), "*.tif$", full.names = TRUE) %>% normalizePath
}

