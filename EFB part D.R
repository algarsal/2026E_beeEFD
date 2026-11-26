#PARTD
#HARI VALENTINO

install.packages(c("lme4", "lmerTest", "ggplot2", "DHARMa", "readxl"))
library(lme4)        # glmer() and lmer(): models with random effects
library(lmerTest)    # adds p-values to lmer() (load AFTER lme4)
library(ggplot2)     # plots
library(DHARMa)      # residual checks
library(readxl)      # to read the raw Excel 

dir.create("results", showWarnings = FALSE)   # folder for outputs
set.seed(123)                                 # DHARMa uses simulations: same seed = same result


#LOAD THE CLEAN DATA

efb <- readRDS("data/clean/efb.rds")   # the file cleaned by Mansi
dim(efb)                               # 657 rows = 219 colonies x 3 time points
str(efb)

# Column used in part D:
#   EFB.incidence    field symptoms (0 = no, 1 = yes)
#   EFB.score        field severity (average frame score), measured at TP3 only
#   EFB.ratio.ddPCR  M. plutonius / bee actin (lab result, relative amount)
#   Site.ID          yard (6 yards); Colony = colony; Time.point = TP1, TP2, TP3

# NEW VARIABLES
# 1) detection: 1 if the lab found bacteria (ratio > 0), 0 if not. NA stays NA.
efb$ddPCR.detect <- ifelse(efb$EFB.ratio.ddPCR > 0, 1, 0)

# 2) log10 of the ratio. log10(0) is -Inf, so only colonies with bacteria get a value.
efb$log10.ratio <- ifelse(efb$EFB.ratio.ddPCR > 0, log10(efb$EFB.ratio.ddPCR), NA)

# 3) readable labels for the plots
efb$Symptoms <- factor(efb$EFB.incidence, levels = c(0, 1),
                       labels = c("Asymptomatic", "Symptomatic"))

summary(efb$EFB.ratio.ddPCR)            
sum(efb$EFB.ratio.ddPCR == 0, na.rm = TRUE)   


#SYMPTOMS, DETECTION OF THE BACTERIA

d1 <- efb[!is.na(efb$EFB.incidence) & !is.na(efb$ddPCR.detect), ]
nrow(d1)

# Table of counts and row percentages
tab1 <- table(Symptoms = d1$Symptoms, ddPCR.found = d1$ddPCR.detect)
tab1
round(100 * prop.table(tab1, margin = 1), 1)   # % of each row (each symptom group)

# Percentage of colonies with bacteria, by symptoms and time point
agg1 <- aggregate(ddPCR.detect ~ Symptoms + Time.point, data = d1,
                  FUN = function(x) 100 * mean(x))
agg1

fig1 <- ggplot(agg1, aes(x = Time.point, y = ddPCR.detect, fill = Symptoms)) +
  geom_col(position = "dodge", colour = "black") +
  scale_fill_manual(values = c("#fff7bc", "#fe9929")) +
  labs(x = "Time point", y = "Colonies with M. plutonius detected (%)") +
  theme_bw()
fig1
ggsave("results/D1_detection_by_symptoms.png", fig1, width = 5, height = 3.5, dpi = 100)

# MODEL: logistic regression with random effects (binomial response 0/1)
# (1 | Site.ID/Colony) = each yard has its own baseline, and each colony
# inside the yard too, because the same colony is measured 3 times.
fit1 <- glmer(ddPCR.detect ~ EFB.incidence + (1 | Site.ID/Colony),
              family = binomial, data = d1)
summary(fit1)

# Odds ratio: how many times higher are the odds of finding bacteria
# in a symptomatic colony than in an asymptomatic one?
exp(fixef(fit1))
exp(confint(fit1, parm = "EFB.incidence", method = "Wald"))

# Check: same model, also correcting for time point
fit1b <- glmer(ddPCR.detect ~ EFB.incidence + Time.point + (1 | Site.ID/Colony),
               family = binomial, data = d1)
summary(fit1b)
