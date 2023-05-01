##############################################################################################################################
# -- this file contains:
# - mixed effect model: interaction effect of age group and domain on changes in intention
# - main comparison analysis: moderating effect of age group on direct and indirect effects of group norms on intention change

# load required packages
library(here) # set dir and load data
library(tidyverse) # includes packages for procesing and plotting
library(lme4) # test main effects in lmer
library(lmerTest) # to get pvalues in lmer
library(lavaan) # conduct main analysis with function sem

# read data
w2_coh<-read.csv("both_groups.csv")

# convert variables to factor
w2_coh <- w2_coh %>%
  mutate(cohort_rec = as.factor(cohort_rec),
         domain = as.factor(domain))

# Mixed effect model: interaction between age group and domain (results in Table S3)
IB_change_age_int <- lmer(
  IB_moves ~ cohort_rec * domain + (1 | ID),
  data = w2_coh
)
summary(IB_change_age_int)

####### Paper section: Comparison between adults in distinct life stages  #######
## Moderation mediation model: moderator age group on path a and c'
mod0 <-  '
# direct effect
IB_moves ~ c*Treatment_GN + w*cohort_rec

# mediator
PN_moves ~ a*Treatment_GN + w1*cohort_rec
IB_moves ~ b*PN_moves

#define moderator (m - cohort with c path; m1 cohort with a path)
m := w*c
m1 := w1*a

# indirect effect (a*b)
ab := a*b

# total effect
total := c + (a*b)
'
fit_coh <- sem(model = mod0,
               data = w2_coh,)
summary(fit_coh, fit.measures = T, ci = TRUE)
parameterEstimates(fit_coh,
                   ci = TRUE,
                   level = 0.95,
                   boot.ci.type = "perc")



