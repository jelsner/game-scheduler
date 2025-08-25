install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(name = "jameselsner",
                          token = "A4855D2093FC612243A597AD9074A195",
                          secret = "qs/+/rRjqFmRBMv++4TM/UHQObxzdObjRyVxHdfU")
rsconnect::deployApp()   # uploads the current folder
