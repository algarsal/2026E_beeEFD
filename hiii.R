#Mansi
install.packages(c("readxl", "dplyr", "janitor", "ggplot2"))
library(readxl)
path <- "data/raw/peerj-14-21338-s001.xlsx"
excel_sheets(path)   #name of all the sheets
efb <- read_excel(path, sheet = 1)
str(efb)   #column names and type
head(efb)

