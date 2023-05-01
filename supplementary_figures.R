#####################################
# -- this file contains:   
# - supplementary figures and results

# load required packages
library(here) # set dir and load data
library(tidyverse) # includes packages for processing and plotting
library(WebPower) # power analysis
library(patchwork) # combine plots 
library(lme4) # test main effects in lmer
library(lmerTest) # to get pvalues in lmer
library(jtools) # creates plots from regression models

# read data
w2_y <- read.csv("younger_adults.csv")
w2_y <- w2_y[, -c(1:2)] # remove first column created by loading the data

w2_o <- read.csv("older_adults.csv")
w2_o <- w2_o[,-c(1)] # remove first column created by loading the data

both <- read.csv("both_groups.csv")
both <- both[, -c(1)] # remove first column created by loading the data


# theme set-up for plots
cleanup = theme(
  legend.key = element_rect(fill = "white"),
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12, face = "bold"),
  axis.title.y = element_text(size = 12, face = "bold"),
  panel.background = element_rect(fill = "white", colour = "black"),
  axis.text.x = element_text(size = 12, colour = "black"),
  axis.text.y = element_text(size = 12, colour = "black"),
  strip.text = element_text(size = 10)
)

###########################
## Supplementary Figures
###########################
# Figure S2: power analysis of hypothesized indirect effect
FigS2 <-
  wp.mediation(
    n = seq(50, 200, 10),
    power = NULL,
    a = 0.4,
    b = 0.5,
    varx = 10,
    vary = 2 ,
    varm = 2,
    alpha = 0.05
  )

plot(FigS2)

# Figure S3: frequency distribution of subjective closeness (a) younger age group; (b) older age group
FigS3a <- w2_y %>%
  dplyr::select(subCloseness, participant_id) %>%
  group_by(participant_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(subCloseness = as.factor(subCloseness)) %>%
  ggplot(aes(x = subCloseness, y = after_stat(count))) +
  geom_bar(
    position = "identity",
    colour = "black",
    fill = "#c7eae5",
    width = 1
  ) +
  ylab("Frequency") +
  scale_x_discrete(breaks = seq(1, 7, by = 1)) +
  cleanup +
  theme(axis.title.x = element_blank())

FigS3b <- w2_o %>%
  dplyr::select(subCloseness, participant_id) %>%
  group_by(participant_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(subCloseness = as.factor(subCloseness)) %>%
  ggplot(aes(x = subCloseness, y = after_stat(count))) +
  geom_bar(
    position = "identity",
    colour = "black",
    fill = "#c7eae5",
    width = 1
  ) +
  scale_x_discrete(breaks = seq(1, 7, by = 1)) +
  cleanup +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

FigS3 <- FigS3a + FigS3b + plot_annotation(tag_levels = c('a'))

FigS3 <- wrap_elements(panel = FigS3) +
  labs(tag = "Subjective closeness\n[1=Very distance;7=Very close]") +
  theme(plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = "bottom")

# ggsave(filename = 'FigS3.png', FigS3, width = 7, height = 4)

# closeness means
# younger group
w2_y %>%
  dplyr::select(subCloseness, participant_id) %>%
  group_by(participant_id) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(meanC = mean(subCloseness))

# older group
w2_o %>%
  dplyr::select(subCloseness, participant_id) %>%
  group_by(participant_id) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(meanC = mean(subCloseness))

# Figure S4: frequency distribution of PN and IB in wave one across both age groups
#  PN1
FigS4a <- w2_coh %>%
  select(cohort, PN_1, Treatment) %>%
  mutate(Treatment = forcats::fct_relevel(Treatment,
                                          "Antisocial",
                                          "Prosocial")) %>%
  mutate(cohort = forcats::fct_relevel(cohort,
                                       "younger",
                                       "older")) %>%
  group_by(Treatment) %>%
  ggplot(aes(x = PN_1,
             color = Treatment,
             fill = Treatment)) +
  geom_bar(alpha = 0.7) +
  scale_x_continuous(breaks = seq(1, 11, 1)) +
  xlab("Personal norm \n[1=Strongly disapprove;11=Strongly approve]") +
  ylab("Frequency") +
  scale_color_manual(values = c("#df73ff", "#00A6A7"),
                     name = "Domain") +
  scale_fill_manual(values = c("#df73ff", "#00A6A7"),
                    name = "Domain") +
  theme(legend.position = "top") +
  facet_wrap(~ cohort) +
  cleanup

# IB1
FigS4b <- w2_coh %>%
  select(cohort, IB_1, Treatment) %>%
  mutate(Treatment = forcats::fct_relevel(Treatment,
                                          "Antisocial",
                                          "Prosocial")) %>%
  mutate(cohort = forcats::fct_relevel(cohort,
                                       "younger",
                                       "older")) %>%
  group_by(Treatment) %>%
  ggplot(aes(x = IB_1,
             color = Treatment,
             fill = Treatment)) +
  geom_bar(alpha = 0.7) +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  xlab("Intention of behavior \n[1=Very unlikely;7=Very likely]") +
  ylab("Frequency") +
  scale_color_manual(values = c("#df73ff", "#00A6A7"),
                     name = "Domain") +
  scale_fill_manual(values = c("#df73ff", "#00A6A7"),
                    name = "Domain") +
  theme(legend.position = "none") +
  facet_wrap(~ cohort) +
  cleanup


FigS4 <- FigS4a / FigS4b
FigS4 <- FigS4 + plot_annotation(tag_levels = c('a'))
FigS4

# ggsave(filename = 'FigS4.png', FigS4, width = 8, height = 7)

# test mean differences in PN1 between the two age groups
t.test(PN_1 ~ cohort, data = w2_coh)

# test mean differences in IB1 between the two age groups
t.test(IB_1 ~ cohort, data = w2_coh)

# Mixed effect model: interaction between age group and domain (results in Table S3)
IB_change_age_int <- lmer(
  IB_moves ~ cohort_rec * domain + (1 | ID),
  data = w2_coh
)
summary(IB_change_age_int)

# Figure S5: interaction effect of age group and domain on intention change
FigS5 <-
  cat_plot(
    IB_change_age_int,
    pred = cohort_rec,
    pred.labels = c("older", "younger"),
    modx = domain,
    modx.labels = c("Antisocial", "Prosocial"),
    geom = "bar",
    x.label = "Age group",
    y.label = "Intention change (mean ±s.e.m)",
    legend.main = "Domain",
    colors = c("#df73ff", "#00A6A7")
  )

FigS5 <- FigS5 + theme(legend.position = "top")
FigS5

# ggsave(filename = 'FigS5.png', FigS5, width = 4, height = 4)

###########################
## Supplementary Results
###########################
# Study 2: Group norms impact personal norms and intentions for the better.
# compute mean across subset of antisocial and prosocial items - PN
# wave 1
w2_o %>% 
  group_by(Treatment) %>% 
  summarise(meanPN1=mean(PN_1),
            sdPN1=sd(PN_1))

# wave 2
w2_o %>% 
  group_by(Treatment) %>% 
  summarise(meanPN1=mean(PN_2),
            sdPN1=sd(PN_2))

# compute mean across subset of antisocial and prosocial items - IB
# wave 1
w2_o %>% 
  group_by(Treatment) %>% 
  summarise(meanIB1=mean(IB_1),
            sdIB1=sd(IB_1))

# wave 2
w2_o %>% 
  group_by(Treatment) %>% 
  summarise(meanIB2=mean(IB_2),
            sdIB2=sd(IB_2))




