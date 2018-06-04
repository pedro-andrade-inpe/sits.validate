
sits_validate.env <- new.env()
sits_validate.env$base_dir = "/Users/pedro/TWDTWAmazoniaCerrado/"
sits_validate.env$crs_sits <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

sits_validate.env$classes_sits = c(
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

sits_validate.env$classificacao_tc = c(
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

sits_validate.env$classificacao_tc_simplificada = c(
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

sits_validate.env$classes_mask = c(
    "0. Fora",
    "1. Dentro"
)

.onAttach = function(lib, pkg){
    packageStartupMessage("sits.validate - Validating sits classifications.")
    packageStartupMessage(sprintf("Loaded version %s.", utils::packageDescription("sits.validate")$Version))
    packageStartupMessage(sprintf("Using base directory '%s'.", sits_validate.env$base_dir))
}

utils::globalVariables("%>%")
