
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
#
Prepare.models<-function(){
  
  EnsurePackage ("dplyr")
  EnsurePackage ("tidyr")
  EnsurePackage ("data.table")
  EnsurePackage("mosaic")
  EnsurePackage("mosaicData")

}

Prepare.models()


