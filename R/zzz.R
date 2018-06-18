
.onAttach <- function(lib, pkg){
    packageStartupMessage("sits.validate - Validating sits classifications")
    packageStartupMessage(sprintf("Version %s is now loaded", utils::packageDescription("sits.validate")$Version))

    if(Sys.info()["sysname"] == "Windows")
      sits.validate.env$base_dir <- "~/../Dropbox/sits.validate"
    else
      sits.validate.env$base_dir <- "~/Dropbox/sits.validate"

    sits.validate.env$base_dir <- normalizePath(sits.validate.env$base_dir)

    if(is.na(file.info(sits.validate.env$base_dir)$isdir)){
      packageStartupMessage("Could not find a valid base directory")
      packageStartupMessage("Please set the base directory using setBaseDir()")
      sits.validate.env$base_dir <- ""
    }
    else{
      packageStartupMessage(sprintf("Using base directory '%s'", sits.validate.env$base_dir))
      packageStartupMessage("You can set this directory by using setBaseDir()")
    }
}

utils::globalVariables("%>%")
