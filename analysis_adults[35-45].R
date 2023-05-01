#####################################################################
# -- this file contains:   
# - study 2:  main in-text analysis regarding older adults 35-45 data 

# load required packages
library(here) # set dir and load data
library(tidyverse) # includes packages for procesing and plotting
library(PupillometryR) # function: geom_flat_violin
library(patchwork) # combine plots 
library(lme4) # test main effects in lmer
library(lmerTest) # to get pvalues in lmer
library(lavaan) # conduct main analysis with function sem

# read data
w2 <- read.csv("older_adults.csv")
w2 <- w2[, -c(1)] # remove first column created by loading the data


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

####### Study 2: Group norms impact personal norms and intentions for the better.  ####### 
# change class of Treatment into factor
w2$Treatment = as.factor(w2$Treatment)

# PN mean comparison between w1 and w2 
# antisocial
t.test(w2$PN_2[w2$Treatment=="Antisocial"], w2$PN_1[w2$Treatment=="Antisocial"])
# prosocial
t.test(w2$PN_2[w2$Treatment=="Prosocial"], w2$PN_1[w2$Treatment=="Prosocial"])

# IB mean comparison between w1 and w2 
# antisocial
t.test(w2$IB_2[w2$Treatment=="Antisocial"], w2$IB_1[w2$Treatment=="Antisocial"])
# prosocial
t.test(w2$IB_2[w2$Treatment=="Prosocial"], w2$IB_1[w2$Treatment=="Prosocial"])

################
## Figure 5: a-b
################
# raincloud plot personal norms wave 1 and 2
# create long format dataframe for rainplot of PN in both waves
PN_both_waves <- w2 %>%
  dplyr::select(PN_1, PN_2,
                Treatment) %>%
  pivot_longer(!Treatment,
               names_to = "Wave",
               values_to = "PN") %>%
  filter(Treatment == "Prosocial" | Treatment == "Antisocial") %>%
  mutate(Wave = case_when(Wave == "PN_1" ~ "1",
                          Wave == "PN_2" ~ "2")) %>%
  rename(Domain = Treatment) %>%
  mutate(Domain = as.factor(Domain),
         Wave = as.factor(Wave))

sumdat <- Rmisc::summarySE(PN_both_waves,
                           measurevar = "PN",
                           groupvars = c("Domain", "Wave"))

sumdat <- sumdat %>%
  mutate(Wave = case_when(Wave == "1" ~ 2,
                          Wave == "2" ~ 1))

# create plot
Fig5a <- PN_both_waves %>%
  ggplot(aes(x = Wave,
             y = PN,
             fill = Domain)) +
  geom_flat_violin(
    aes(fill = Domain),
    position = position_nudge(x = .1),
    trim = F,
    adjust = 1.5,
    alpha = .6,
    colour = NA
  ) +
  geom_point(
    data = sumdat,
    aes(
      x = as.numeric(Wave) + .1,
      y = PN,
      group = Domain,
      colour = Domain
    ),
    shape = 18,
    alpha = 1
  ) +
  geom_errorbar(
    data = sumdat,
    aes(
      x = as.numeric(Wave) + .1,
      y = PN,
      group = Domain,
      colour = Domain,
      ymin = PN - se,
      ymax = PN + se
    ),
    width = .05
  ) +
  geom_line(
    data = sumdat,
    aes(
      x = as.numeric(Wave) + .1,
      y = PN,
      group = Domain,
      colour = Domain
    ),
    linetype = 1,
    alpha = 1
  ) +
  ylab("Personal norm \n[1=Strongly disapprove;11=Strongly approve]") +
  xlab("Wave") +
  scale_fill_manual(values = c("Antisocial" = "#df73ff" ,
                               "Prosocial" = "#00A6A7"))  +
  scale_colour_manual(values = c("Antisocial" = "#df73ff" ,
                                 "Prosocial" = "#00A6A7")) +
  scale_y_continuous(breaks = seq(1, 11)) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  cleanup

# recode to match with plot bc of factor variable
recoded <- PN_both_waves %>%
  dplyr::select(PN,
                Domain,
                Wave) %>%
  mutate(Wave = case_when(Wave == "1" ~ 2, # wave 1 is at position 2 in y axis to add the jitter below boxplots
                          Wave == "2" ~ 1)) # wave 2 is at position 1 in y axis to add the jitter below boxplots)) 

Fig5a <- Fig5a +
  geom_point(
    data = recoded,
    aes(x = Wave - .15,
        y = PN,
        colour = Domain),
    position = position_jitter(width = 0.1),
    size = .25,
    shape = 23,
    alpha = .8
  )
Fig5a <- Fig5a + theme(legend.position = "none")

# raincloud plot intentions wave 1 and 2
# create long format dataframe for rainplot of IB in both waves
IB_both_waves <- w2 %>%
  dplyr::select(IB_1, IB_2,
                Treatment) %>%
  pivot_longer(!Treatment,
               names_to = "Wave",
               values_to = "IB") %>%
  filter(Treatment == "Prosocial" | Treatment == "Antisocial") %>%
  mutate(Wave = case_when(Wave == "IB_1" ~ "1",
                          Wave == "IB_2" ~ "2")) %>%
  rename(Domain = Treatment) %>%
  mutate(Domain = as.factor(Domain),
         Wave = as.factor(Wave))

sumdatIB <- Rmisc::summarySE(IB_both_waves,
                             measurevar = "IB",
                             groupvars = c("Domain", "Wave"))

sumdatIB <- sumdatIB %>%
  mutate(Wave = case_when(Wave == "1" ~ 2,
                          Wave == "2" ~ 1))

# create the plot 
Fig5b <- IB_both_waves %>%
  ggplot(aes(x = Wave,
             y = IB,
             fill = Domain)) +
  geom_flat_violin(
    aes(fill = Domain),
    position = position_nudge(x = .1),
    trim = F,
    adjust = 1.5,
    alpha = .6,
    colour = NA
  ) +
  geom_point(
    data = sumdatIB,
    aes(
      x = as.numeric(Wave) + .1,
      y = IB,
      group = Domain,
      colour = Domain
    ),
    shape = 18,
    alpha = 1
  ) +
  geom_errorbar(
    data = sumdatIB,
    aes(
      x = as.numeric(Wave) + .1,
      y = IB,
      group = Domain,
      colour = Domain,
      ymin = IB - se,
      ymax = IB + se
    ),
    width = .05
  ) +
  geom_line(
    data = sumdatIB,
    aes(
      x = as.numeric(Wave) + .1,
      y = IB,
      group = Domain,
      colour = Domain
    ),
    linetype = 1,
    alpha = 1
  ) +
  ylab("Intention of behavior \n[1=Very unlikely;7=Very likely]") +
  xlab("Wave") +
  scale_fill_manual(values = c("Antisocial" = "#df73ff" ,
                               "Prosocial" = "#00A6A7"))  +
  scale_colour_manual(values = c("Antisocial" = "#df73ff" ,
                                 "Prosocial" = "#00A6A7")) +
  scale_y_continuous(breaks = seq(1, 7)) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  cleanup

# recode to match with plot bc of factor variable
recodedIB <- IB_both_waves %>%
  dplyr::select(IB,
                Domain,
                Wave) %>%
  mutate(Wave = case_when(Wave == "1" ~ 2, # wave 1 is at position 2 in y axis to add the jitter below boxplots
                          Wave == "2" ~ 1)) # wave 2 is at position 1 in y axis to add the jitter below boxplots))

Fig5b <- Fig5b +
  geom_point(
    data = recodedIB,
    aes(x = Wave - .15,
        y = IB,
        colour = Domain),
    position = position_jitter(width = .1),
    size = .25,
    shape = 23,
    alpha = .8
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


Fig5 <- Fig5a | Fig5b
Fig5 <- Fig5 + plot_annotation(tag_levels = c('a'))
Fig5

# ggsave(filename = 'Fig5.png', Fig5, width = 11.5, height = 4)

####### Study 2: The direct effect of group norms is stronger than the indirect effect via norm internalization.####### 
################
## Figure 3: a-c
################
# effect of group norm on PN change - a
modGNPN <- lmer(PN_moves ~ Treatment + (1 | participant_id), data = w2)
summary(modGNPN)

GNPNEffect <- effects::effect('Treatment',
                              modGNPN,
                              se = TRUE)

GNPNEffect.DF <- as.data.frame(GNPNEffect)

# re-label the variable for the plot
GNPNEffect.DF$Treatment <- factor(
  GNPNEffect.DF$Treatment,
  level = c("Antisocial", "Prosocial"),
  labels = c("Extreme\ndisapproval",
             "Extreme\napproval")
)

# plot effect of group norm on intention change
Fig6a <- ggplot(GNPNEffect.DF,
                aes(
                  x = factor(
                    Treatment,
                    level = c("Extreme\ndisapproval",
                              "Extreme\napproval"),
                    labels = c("Extreme\ndisapproval",
                               "Extreme\napproval")
                  ),
                  y = fit,
                  colour = Treatment
                )) +
  geom_bar(
    stat = "identity",
    position = position_dodge(),
    fill = "white",
    width = 0.4
  ) +
  geom_errorbar(aes(ymin = fit - se,
                    ymax = fit + se),
                width = 0.25,
                position = position_dodge(width = 0.9)) +
  scale_colour_manual(values = c(
    "Extreme\ndisapproval" = "#df73ff",
    "Extreme\napproval" = "#00A6A7"
  )) +
  ylab("Personal norm change (mean ±s.e.m)") +
  xlab("Group norm") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(-1.5, 1.5, by = 0.5),
                     limits = c(-1.5, 1.5)) +
  geom_hline(yintercept = 0.0,
             linetype = "dashed",
             colour = "gray") +
  cleanup

Fig6a <-
  Fig6a + annotate(
    "text",
    x = 1,
    y = 0.1,
    label = "Antisocial",
    size = 4,
    colour = "#df73ff"
  )
Fig6a <-
  Fig6a + annotate(
    "text",
    x = 2,
    y = -0.1,
    label = "Prosocial",
    size = 4,
    colour = "#00A6A7"
  )
Fig6a

# effect of PN change on intention change
Fig6b <- w2 %>%
  dplyr::select(IB_moves, PN_moves) %>%
  ggplot(aes(
    y = IB_moves,
    x = PN_moves,
    fill = as.factor(PN_moves)
  )) +
  geom_jitter(position = position_jitter(0.2),
              alpha = 0.2,
              colour = "gray77") +
  geom_boxplot(outlier.shape = NA, colour = "gray77") +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    size = 1,
    stroke = 1,
    color = "Black",
    fill = "Black"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    aes(group = 1),
    color = 'black',
    lty = 3
  ) +
  xlab("Personal norm change") +
  ylab("Intention change") +
  scale_x_continuous(breaks = seq(-10, 8, by = 2)) +
  scale_y_continuous(breaks = seq(-6, 6, by = 2)) +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c(
      "gray93",
      "gray92",
      "gray91",
      "gray90",
      "gray89",
      "gray88",
      "gray87",
      "gray86",
      "gray85",
      "gray84",
      "gray83",
      "gray82",
      "gray81",
      "gray80",
      "gray79",
      "gray78",
      "gray77",
      "gray76",
      "gray75",
      "gray74",
      "gray73",
      "gray72"
    )
  ) +
  geom_hline(yintercept = 0.0,
             linetype = "dashed",
             colour = "gray") +
  cleanup

Fig6b

# effect of group norm on intention change
modGN <- lmer(IB_moves ~ Treatment + (1 | participant_id), data = w2)
summary(modGN)

GNEffect <- effects::effect('Treatment',
                            modGN,
                            se = TRUE)

GNEffect.DF <- as.data.frame(GNEffect)

# re-label the variable for the plot
GNEffect.DF$Treatment <- factor(
  GNEffect.DF$Treatment,
  level = c("Antisocial", "Prosocial"),
  labels = c("Extreme\ndisapproval",
             "Extreme\napproval")
)

# plot effect of group norm on intention change
Fig6c <- ggplot(GNEffect.DF,
                aes(
                  x = factor(
                    Treatment,
                    level = c("Extreme\ndisapproval",
                              "Extreme\napproval"),
                    labels = c("Extreme\ndisapproval",
                               "Extreme\napproval")
                  ),
                  y = fit,
                  colour = Treatment
                )) +
  geom_bar(
    stat = "identity",
    position = position_dodge(),
    fill = "white",
    width = 0.4
  ) +
  geom_errorbar(aes(ymin = fit - se,
                    ymax = fit + se),
                width = 0.25,
                position = position_dodge(width = 0.9)) +
  scale_colour_manual(values = c(
    "Extreme\ndisapproval" = "#df73ff",
    "Extreme\napproval" = "#00A6A7"
  )) +
  ylab("Intention change (mean ±s.e.m)") +
  xlab("Group norm") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(-1.5, 1.5, by = 0.5),
                     limits = c(-1.5, 1.5)) +
  geom_hline(yintercept = 0.0,
             linetype = "dashed",
             colour = "gray") +
  cleanup

Fig6c <-
  Fig6c + annotate(
    "text",
    x = 1,
    y = 0.1,
    label = "Antisocial",
    size = 4,
    colour = "#df73ff"
  )
Fig6c <-
  Fig6c + annotate(
    "text",
    x = 2,
    y = -0.1,
    label = "Prosocial",
    size = 4,
    colour = "#00A6A7"
  )
Fig6c

# plot order
Fig6 <- Fig6a | Fig6b | Fig6c
Fig6 <- Fig6 + plot_annotation(tag_levels = c('a'))
Fig6

# ggsave(filename = 'Fig6.png', Fig6, width = 10, height = 4)

# compute mean change across participants
# PN
w2 %>% 
  group_by(Treatment) %>% 
  summarise(meanPNmoves=mean(PN_moves),
            sdPNmoves=sd(PN_moves))
# IB
w2 %>% 
  group_by(Treatment) %>% 
  summarise(meanIBmoves=mean(IB_moves),
            sdIBmoves=sd(IB_moves))

# correlation between changes in PN and IB 
cor.test(w2$PN_moves, w2$IB_moves) 


###########################################################
## statistical model - Figure 7 and results in Table S2
##########################################################
mod <-  '
# direct effect (a)
IB_moves ~ c*Treatment_GN + w*subCloseness

# mediator (a and b)
PN_moves ~ a*Treatment_GN + w1*subCloseness
IB_moves ~ b*PN_moves

#define moderator (m - closennes with c path; m1 closennes with a path)
m := w*c
m1 := w1*a

# indirect effect (a*b)
ab := a*b

# total effect
total := c + (a*b)
'

fit <- sem(
  model = mod,
  data = w2,
  se = "bootstrap",
  bootstrap = 1000
)

summary(fit, fit.measures = T)
parameterEstimates(fit,
                   ci = TRUE,
                   level = 0.95,
                   boot.ci.type = "perc")





