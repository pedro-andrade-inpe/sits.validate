
.onAttach <- function(lib, pkg){
    packageStartupMessage("sits.validate - Validating sits classifications")
    packageStartupMessage(sprintf("Version %s is now loaded", utils::packageDescription("sits.validate")$Version))

    if(file.info("/Users/pedro")$isdir)
        packageStartupMessage(sprintf("Using base directory '%s'", sits.validate.env$base_dir))
    else
    {
        sits.validate.env$base_dir <- ""
        packageStartupMessage("Please set the base directory using baseDir()")
    }
}

utils::globalVariables("%>%")
