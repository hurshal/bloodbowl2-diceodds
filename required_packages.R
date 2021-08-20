#### Packages ##########

list.of.packages <- c("RSelenium", "dplyr", "XML", "rvest", "reshape2", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(RSelenium)
require(dplyr)
require(XML)
require(rvest)
require(reshape2)
require(readr)

