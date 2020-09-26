# Section One ---------------------------------
rm(list = ls())
setwd("../../Data/")
pw <- readline(prompt = "Enter the password: ")

# Section Two ---------------------------------
system(command = paste0("unzip -o -P ", pw, " ", "ASPIRE.zip"), wait = TRUE )

setwd("../Data/data")

load("ASPIREmice1.RData")
rm(d1.complete, d3, d3.mice,pw)

setwd("..")
unlink("data", recursive = TRUE)

summary(d1)


