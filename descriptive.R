# =====================================================================
# Author : Naima Kaloun
# Goal : Does EFB get worse over time?  
# Paper: Gregoris et al. (2026)
# Input: data/clean/efb.rds   (made by Manshi)
# =====================================================================

# ---- 0. Packages--------------------------------------------------------
# Call packages  
library(tidyr)     # to reshape the table (long -> wide)
library(ggplot2)   # graphs

# ---- 1. Read Manshi's clean data ---------------------------------------------
efb <- readRDS("data/clean/efb.rds")
readRDS("data/clean/efb.rds")
readRDS("data/clean/varroa.rds")
# Quick checks: 
dim(efb)                 # Nb of observations and variables
names(efb)               #Names of variables
table(efb$Time.point)       # TP1, TP2, TP3
table(efb$Site.ID)          # colonies 

# ---- 2. Reshape: ONE ROW PER COLONY -------------------------------------
# Manshi's table is in "long" format: each colony appears 3 times (TP1, TP2, TP3).
# McNemar needs each colony's three results SIDE BY SIDE

efb$Time.point <- as.character(efb$Time.point)     # make sure it is text

wide <- pivot_wider(
  efb[, c("Site.ID", "Colony", "Time.point", "EFB.incidence")],
  id_cols     = c("Site.ID", "Colony"),   # one row per colony
  names_from  = "Time.point",             # new column names come from here
  values_from = "EFB.incidence"           # the values that fill the columns
)
dim(wide)    # expected: 219 rows, 5 columns (Site.ID, Colony, TP1, TP2, TP3)
head(wide)

# The 6 yards (groups) in the paper
yards <- c("nonBB AB-1", "BB AB-1", "BC-1", "BC-2", "BC-3", "AB-2")

# =====================================================================
# PART 1 - Descriptive: % of sick colonies per yard and time point
# =====================================================================
# EFB is coded 0/1, so the MEAN of a column = proportion of sick colonies.
prev <- data.frame()
for (y in yards) {
  d <- wide[wide$Site.ID == y, ]               # this yard only
  for (tp in c("TP1", "TP2", "TP3")) {
    x <- d[[tp]]
    x <- x[!is.na(x)]                          # drop colonies not assessed
    prev <- rbind(prev, data.frame(
      Yard = y, TimePoint = tp,
      N = length(x),                           # colonies assessed
      Sick = sum(x),                           # colonies with symptoms
      Prevalence = 100 * mean(x)               # percent sick
    ))
  }
}
prev$Yard <- factor(prev$Yard, levels = yards)
print(prev)


ggplot(prev, aes(x = TimePoint, y = Prevalence)) +
  geom_col(fill = "navy") +
  facet_wrap(~ Yard, nrow = 1) +
  labs(x = "Time point", y = "EFB prevalence (% symptomatic colonies)",
       title = "EFB prevalence over time, per yard") +
  theme_bw()
ggsave("Fig_B1_prevalence.png", width = 10, height = 4, dpi = 300)
