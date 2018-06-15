
.onAttach <- function(lib, pkg){
    packageStartupMessage("sits.validate - Validating sits classifications")
    packageStartupMessage(sprintf("Version %s is now loaded", utils::packageDescription("sits.validate")$Version))

    if(file.info("/Users/pedro")$isdir){
        packageStartupMessage(sprintf("Using base directory '%s'", sits.validate.env$base_dir))
        packageStartupMessage("You can set this directory by using setBaseDir()")
	}
    else{
        packageStartupMessage("Could not find a valid base directory")
        packageStartupMessage("Please set the base directory using setBaseDir()")
        sits.validate.env$base_dir <- ""
    }

    sits.validate.env$base_dir <- normalizePath(sits.validate.env$base_dir)
}

utils::globalVariables("%>%")
