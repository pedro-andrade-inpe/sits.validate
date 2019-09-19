
sits.validate.env <- new.env()
#sits.validate.env$base_dir will be created by zzz.R

sits.validate.env$crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"



sits.validate.env$classes_tc_amaz <- list(
  FNNF = 1,
  SECV = 2,
  PFOR = 3,
  RANG = 4,
  GRAS = 5,
  "06.AgricPerene",
  "07.AgricSemiPerene",
  "08.AgricAnual",
  MNNG = 9,
  URBA = 10,
  OTHR = 11,
  NOBS = 12,
  "13.DeforNoAno",
  "14.NaoFloresta",
  WATR = 7
)


# mb == mapbiomas
sits.validate.env$classes_mb <- c(
  "00.ND",
  "01.Forest",
  "02.NatFor",
  "03.ForestFor",
  "04.Savanna",
  "05.Mangrove",
#  "06.ND",
#  "07.ND",
#  "08.ND",
  "09.PlantedFor",
#  "10.NonForNat",
  "11.Wetland",
  "12.Grassland",
  "13.SaltFlat",
#  "14.Farming",
  "15.Pasture",
#  "16.ND",
#  "17.ND",
#  "18.Agricult",
  "19.AnnPerCrop",
  "20.SemiPerCrop",
  "21.Agric/Past",
#  "22.NonVeg",
  "23.Beach",
  "24.Urban",
  "25.OtherNonVeg",
#  "26.Water",
  "27.NonObs",
#  "28.ND",
  "29.Rock",
  "30.Mining",
#  "31.Aquacult",
  "32.ND",
  "33.RiverLake"
)

sits.validate.env$classes_mask <- c(
    "0. Fora",
    "1. Dentro"
)

sits.validate.env$classes_hansen <- c(
  "0. NoForest",
  "1. Forest"
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
