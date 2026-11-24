#Mansi 
install.packages(c("readxl", "dplyr", "janitor", "ggplot2"))
library(readxl)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
file.copy(file.choose(), "data/raw/Supplementary_Data_1.xlsx")
path <- "data/raw/Supplementary_Data_1.xlsx"
excel_sheets(path)   #name of all the sheets
efb <- read_excel(path, sheet = 1)
str(efb)   #column names and type
head(efb)
#to clean
library(dplyr)
path <- "data/raw/Supplementary_Data_1.xlsx"
efb <- read_excel(path, sheet = "EFB data TP1-TP3",
na = c("", "NA", "Not measured"))   # NA text will become a real blank
efb_clean <- efb %>%
filter(!is.na(Colony)) %>%               
mutate(Site.ID    = factor(Site.ID),                                 
Time.point = factor(Time.point, levels = c("TP1", "TP2", "TP3")),  # visits in order
PP = factor(PP)                                        # patty: Y / N
)
str(efb_clean) 
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
saveRDS(efb_clean, "data/clean/efb.rds")


