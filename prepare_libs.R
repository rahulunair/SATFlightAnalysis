devtools::install_git("https://github.com/hadley/devtools.git", branch = "master")
devtools::install_git("https://github.com/jbryer/makeR.git", branch = "master")

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
  
  EnsurePackage ("mosaic")
  EnsurePackage ("lubridate")
  EnsurePackage ("dplyr")
  EnsurePackage ("tidyr")
  EnsurePackage ("data.table")
  EnsurePackage ("RCurl")
  # A library, that faciltates sql qureires on dataframe, I found it more flexible that dplyr.
  EnsurePackage ("sqldf")
  # A library, that has a set of helper method to calculate MSE, SE among others
  EnsurePackage ("metrics")
  EnsurePackage ("devtools")
  # A library using which heat maps of for weather and delay are drawn
  EnsurePackage ("makeR")
  # A library which helped in plotting the map of routes from SAT across the US
  EnsurePackage ("ggmap")

  
  
}

Prepare.models()


# Essential functions

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

