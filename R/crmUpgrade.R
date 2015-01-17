#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: crmPack
##
## Time-stamp: <[crmUpgrade.R] by DSB Sam 17/01/2015 17:53>
##
## Description:
## Upgrade function for crmPack
##
## History:
## 11/01/2015   file creation
#####################################################################################


##' Upgrade your crmPack installation with the latest version
##'
##' Executing this function upgrades your crmPack installation with the newest
##' version available on the server. You will need connection to the Roche
##' network (i.e. RANGE connection when you are working off-site) in order to
##' successfully run it.
##'
##' After installation, the new features in this version will be shown by
##' printing the relevant parts of the NEWS file.
##'
##' @param lib library where to install the new version (and other required
##' packages) into. Default: same location as the last crmPack version.
##' @param devel Should the development version be installed? (default) -
##' currently there is no other possibility.
##' @param force Should the installation be forced, i.e., even if you have
##' already the latest version, should the package be re-installed?
##' @param repos which repository to use for installing required packages
##' (default: ETHZ in Switzerland)
##' @return nothing
##'
##' @export
##' @importFrom httr GET content
##' @keywords programming
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
crmPackUpgrade <- function(lib = NULL, devel = TRUE, force = FALSE,
                           repos=structure(c(CRAN="http://stat.ethz.ch/CRAN/")))
{
    ## check devel parameter.
    if(! devel)
    {
        stop("currently there is only the development version for download")
    }
    ## todo: later we will probably have a release version (on the package
    ## server) and still the development version (on Stash)

    ## set the repository (set it back to old setting when exiting this
    ## function)
    oldRepos <- options("repos")
    options(repos=repos)
    on.exit(options(repos=oldRepos))

    ## should we find out the correct library?
    if(is.null(lib))
    {
        ## find out where the currently loaded crmPack is installed
        sps <- searchpaths()
        lib <- sps[grep("[\\/]crmPack$", sps)]
        if (length(lib) == 0) {
            warning("crmPack does not seem to be installed yet in your system")
            lib <- NULL
        } else {
            while(length(grep("/?crmPack$", lib)))
                lib <- sub("/?crmPack$", "", lib)
            ## here lib is the library where the current crmPack is installed
        }
    }

    ## specify source and target (temporary file in tmp)
    url <-
        paste("https://stash.intranet.roche.com/stash/plugins",
              "/servlet/archive/projects/RSTAT",
              "/repos/crmpack?at=refs%2Fheads%2Fmaster",
              sep="")
    tmp <- tempdir()
    target <- file.path(tmp, "crmPack.zip")

    ## download from source to target
    x <- httr::GET(url, config = list(ssl.verifypeer = FALSE))
    bin <- httr::content(x, "raw")
    writeBin(bin, target)

    ## unzip to a temporary directory below tmp
    dir <- file.path(tmp, "crmPack")
    unzip(zipfile=target, exdir=dir)

    ## now compare the installed and the just downloaded crmPack versions
    oldVersion <- try(packageVersion("crmPack", lib.loc=lib))
    ## if it is not found, then take 0.0.0 version as old one:
    noPrevious <- inherits(oldVersion, "try-error")
    if(noPrevious)
    {
        oldVersion <- package_version('0.0.0')
        cat("No crmPack installation found\n")
    } else {
        cat("Installed version:", as.character(oldVersion), "\n")
    }
    newVersion <- packageVersion("crmPack", lib.loc=tmp)
    cat("New version:      ", as.character(newVersion), "\n")

    ## in case the new version is not newer
    if(newVersion <= oldVersion)
    {
        cat("New version is not more recent than the installed version\n")
        if(! force)
        {
            cat("Thank you for checking to upgrade crmPack :-)\n")
            return(invisible())
        } else {
            cat("Still continuing with the installation because forced\n")
        }
    }

    ## remove old library before installing the new one
    try(detach(package:crmPack), silent = TRUE)
    try(unloadNamespace("crmPack"), silent = TRUE)

    ## install new version
    install.packages(dir, lib = lib, repos=NULL, type="source")
    library("crmPack", lib.loc = lib)

    ## find the NEWS file
    newsPath <- system.file("NEWS", package="crmPack")
    newsLines <- readLines(newsPath)

    ## and skip showing it or show relevant parts of it
    if(noPrevious)
    {
        cat("Not showing NEWS since no previous version installed\n")
    } else if (newVersion <= oldVersion) {
        cat("No NEWS since new version is not more recent than previous\n")
    } else {
        cat("Showing crmPack NEWS since previous version:\n")

        ## find out which lines of NEWS file are old
        oldLineIndex <- grep(as.character(oldVersion),
                         x=newsLines)
        if(length(oldLineIndex) > 0)
        {
            cat(newsLines[1:(oldLineIndex-1)], sep="\n")
        } else {
            ## old version number was not found - therefore everything is new
            cat(newsLines, sep="\n")
        }
    }

    ## show important help options
    cat("Thank you very much for upgrading crmPack :-)\n")
    cat("Type crmPackHelp() to open help browser\n")
    cat("Type crmPackExample() to open example pdf\n")
    cat("Please visit https://roche.jiveon.com/projects/crmpack for more\n\n")

    return(invisible())
}
