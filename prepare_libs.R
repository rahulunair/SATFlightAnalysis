
EnsurePackage<-function(x)
{ # EnsurePackage(x) - Installs and loads a package
  # if necessary
  x <- as.character(x)
  if (!require(x, character.only=TRUE))
  {
    install.packages(pkgs=x,
                     repos="http://cran.r-project.org")
  }
  library(x, character.only=TRUE)
  
}

#Installs and loads all packages necessary

Prepare.models<-function(){
  
  EnsurePackage ("lubridate")
  EnsurePackage ("dplyr")
  EnsurePackage ("tidyr")
  EnsurePackage ("data.table")
  EnsurePackage("mosaic")
  EnsurePackage("mosaicData")
  EnsurePackage ("RCurl")
}

Prepare.models()


# Essential functions

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

