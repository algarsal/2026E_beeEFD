#Mansi 
#install.packages(c("readxl", "dplyr", "ggplot2"))
library(readxl)
#dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
#file.copy(file.choose(), "data/raw/Supplementary_Data_1.xlsx")
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
PP = factor(PP))                                       # patty: Y / N
str(efb_clean) 
varroa <- read_excel(path, sheet = "Varroa data")
varroa <- varroa[!is.na(varroa$Colony), ]   # to remove empty rows

# save for the group
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
saveRDS(efb_clean, "data/clean/efb.rds")
saveRDS(varroa,    "data/clean/varroa.rds")
library(ggplot2)
ggplot(varroa, aes(x= Mite.percent)) + 
geom_histogram(bins = 30)+
facet_wrap(~Time.point) +
labs( x = "mites per 100 bees", y = "number of colonies")

# average mites per group
aggregate(Mite.percent ~ Site.ID + Time.point, data = varroa, FUN = mean)

#Mann-Whiteny test and Bonferroni
tp1 <- subset(varroa, Time.point == "TP1")
pairwise.wilcox.test(tp1$Mite.percent, tp1$Site.ID, p.adjust.method = "bonferroni")
tp2 <- subset(varroa, Time.point == "TP2")
pairwise.wilcox.test(tp2$Mite.percent, tp1$Site.ID, p.adjust.method = "bonferroni")

#boxplot
ggplot(varroa, aes(x = Site.ID, y = Mite.percent)) +
geom_boxplot() +
geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
facet_wrap(~Time.point) +
labs(x = "Colony group", y = "mites per 100 bees")
