## ----read in-------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      fig.path = "rel-continuation-figs/",
                      fig.width = 12,
                      fig.height = 8)
# data prep
library(haven)
library(ggplot2)
library(dplyr)
library(reshape2)
library(stringr)
library(survival)
library(tidyr)
#
theme_set(theme_bw())
# read in
thedata <- read_sav("/Users/Ben/Dropbox/PhD/Research/CCHN-relationship-continuation/171015Data-for-Ben.sav")
##
# identify those together at t1
##
# relationship status legend
# t1 legend m112_1000:
# 1 is married
# 2 is not married but together
# 3 is not married and not together
# 10 is father is deceased
# 98 is don't know
# 99 is refused to respond
thedata <- thedata %>%
  mutate(together.t1 = ifelse(m112_1000 %in% 1:2, 1, 0))
##
# identifying those together at t2:5
##
# relationship update legend
##
# 1 is moved in together
# 2 is no longer living together
# 3 is got married
# 4 is now separated
# 5 is now divorced
# 6 is broke up
# 7 is got back together
# 8 is no change
# 9 is other change
# 10 is father deceased
# 11 is engaged
# 98 is don't know
# 99 is refused to respond
##
# the selection is:
##
# for time t(n)
# if not together at t(n-1)
# and changed by moving in, marrying, back together or engaged
# then together at tn
# if together at t(n-1)
# and no change
# then together at tn
# all else then not together at tn
##
# identify those together at t2
##
# m201_1630 was there a change?
# m201_1640 if yes, what change?
thedata <- thedata %>%
  mutate(together.t2 = case_when(together.t1 == 0 &
                                   m201_1630 == 1 &
                                   m201_1640 %in% c(1, 3, 7, 11) ~ 1,
                                 together.t1 == 1 & m201_1630 == 0 ~ 1,
                                 TRUE ~ 0))
##
# identify those together at t3
##
# m301_1240 was there any changes?
# m301_1250 if yes, what change?
thedata <- thedata %>%
  mutate(together.t3 = case_when(together.t2 == 0 &
                                   m301_1240 == 1 &
                                   m301_1250 %in% c(1, 3, 7, 11) ~ 1,
                                 together.t2 == 1 & m301_1240 == 0 ~ 1,
                                 TRUE ~ 0))
##
# identify those together at t4
##
# m401_1240 was there any changes?
# m401_1250 if yes, what change?
thedata <- thedata %>%
  mutate(together.t4 = case_when(together.t3 == 0 &
                                   m401_1240 == 1 &
                                   m401_1250 %in% c(1, 3, 7, 11) ~ 1,
                                 together.t3 == 1 & m401_1240 == 0 ~ 1,
                                 TRUE ~ 0))
##
# identify those together at t5
##
# m501_1240 was there any changes?
# m501_1250 if yes, what change?
thedata <- thedata %>%
  mutate(together.t5 = case_when(together.t4 == 0 &
                                   m501_1240 == 1 &
                                   m501_1250 %in% c(1, 3, 7, 11) ~ 1,
                                 together.t4 == 1 & m501_1240 == 0 ~ 1,
                                 TRUE ~ 0))
# Kharah, per your comments in the kmr.comments variable, I'm excluding any of the participants that you flagged
thedata <- thedata %>% filter(str_length(kmr.comments) == 0)
# filtering to include only participants who were together at t2
thedata <- thedata %>% filter(together.t2 == 1)  # 1151 participants
##
# reshape the data to have event times and censoring
##
thedata <- thedata %>%
  mutate(separation.time = case_when(together.t3 == 0 ~ 3,
                                     together.t4 == 0 ~ 4,
                                     together.t5 == 0 ~ 5,
                                     together.t5 == 1 ~ 5),
         censored = ifelse(together.t5 == 1 & separation.time == 5, 1, 0))
##
# reshaping predictors
##
# centering
thedata <- thedata %>% mutate_at(vars(starts_with("fDAS"),
                                      starts_with("mDAS"),
                                      starts_with("cmean")),
                                 funs(c = scale(.)))
# computing couple level means and sd
thedata <- thedata %>%
  rowwise() %>%
  mutate(cmean.conflict = mean(c(fDAS.conflict, mDAS.conflict)),
         cmean.pos = mean(c(fDAS.pos.sat, mDAS.pos.sat)),
         cmean.conflict_c = mean(c(fDAS.conflict_c, mDAS.conflict_c)),
         cmean.pos_c = mean(c(fDAS.pos.sat_c, mDAS.pos.sat_c)),
         csd.conflict = sd(c(fDAS.conflict, mDAS.conflict)),
         csd.pos = sd(c(fDAS.pos.sat, mDAS.pos.sat))) %>%
  ungroup() %>%
  mutate_at(vars(starts_with("csd")),
            funs(c = scale(., scale = FALSE)))
# spot check
# select(thedata, starts_with("together"), separation.time, censored) %>%
  # filter(together.t4 == 0 & together.t5 == 1)
##
# generating person period format
##
pp.data <- thedata %>%
  select(subjid, separation.time, censored) %>%
  mutate(separation.t2 = 0,
         separation.t3 = case_when(separation.time == 3 ~ 1,
                                   TRUE ~ 0),
         separation.t4 = case_when(separation.time == 4 ~ 1,
                                   separation.time > 4 ~ 0,
                                   TRUE ~ NA_real_),
         separation.t5 = case_when(separation.time == 5 & censored == 0 ~ 1,
                                   separation.time == 5 & censored == 1 ~ 0,
                                   TRUE ~ NA_real_)) %>%
  gather(time, separation, -c(subjid, censored, separation.time)) %>%
  arrange(subjid) %>%
  filter(!is.na(separation)) %>%
  mutate(timen = as.numeric(str_sub(time, start = -1))) %>%
  left_join(select(thedata,
                   subjid,
                   contains("DAS"),
                   starts_with("cmean"),
                   starts_with("csd"),
                   parity,
                   mother_age,
                   father_age,
                   income = per_capita_hh_income_m),
                   by = "subjid")
# function to generat hazard tables and ci
# this is separate so we only have to compute cis once
make.initial.hazard <- function(the.fit) {
  tibble(time = 3:5,
         logit = the.fit$coefficients[1:3],
         ci.low = confint(the.fit)[1:3, 1],
         ci.high = confint(the.fit)[1:3, 2])
}
# function to generate hazard tables for main effects
make.hazard.me <- function(the.fit,
                           the.initial.hazard,
                           the.main.effect,
                           main.effect.level,
                           main.effect.label) {
  main.effect.val <- the.fit$coefficients[the.main.effect] * main.effect.level
  the.hazard <- the.initial.hazard %>%
    mutate_at(vars(logit, ci.low, ci.high),
                 funs(. + main.effect.val)) %>%
    mutate(main.effect = main.effect.label) %>%
    mutate_at(vars(logit, ci.low, ci.high),
              funs(odds = exp(.),
                   hazard = exp(.)/(1 + exp(.)))) %>%
    rename(ci.low_logit = ci.low,
           ci.high_logit = ci.high,
           odds = logit_odds,
           hazard = logit_hazard) %>%
    mutate(survival = cumprod(1 - hazard)) %>%
    select(time, main.effect, survival, matches("hazard"), everything()) %>%
    bind_rows(tibble(time = 2,
                     survival = 1,
                     main.effect = main.effect.label))
  the.hazard
}
hazard.main.effect <- function(the.fit,
                               the.main.effect) {
  the.initial.hazard <- make.initial.hazard(the.fit)
  fit.hazard.mean <- make.hazard.me(the.fit,
                                    the.initial.hazard,
                                    the.main.effect,
                                    main.effect.level = 0,
                                    main.effect.label = "Mean")
  fit.hazard.low <- make.hazard.me(the.fit,
                                   the.initial.hazard,
                                   the.main.effect,
                                   main.effect.level = -1,
                                   main.effect.label = "-1 SD")
  fit.hazard.high <- make.hazard.me(the.fit,
                                    the.initial.hazard,
                                    the.main.effect,
                                    main.effect.level = +1,
                                    main.effect.label = "+1 SD")
  fit.hazard <- bind_rows(fit.hazard.mean,
                          fit.hazard.low,
                          fit.hazard.high) %>%
    select(time, main.effect, survival, matches("hazard"), everything()) %>%
    mutate(main.effect = factor(main.effect,
                                levels = c("+1 SD",
                                           "Mean",
                                           "-1 SD")))
  fit.hazard
}
# function to generate hazard tables for interactions
# testing hazard.int(fit.c5, 4:7, 8:15)
make.hazard.int <- function(the.fit,
                            the.initial.hazard,
                            effect.val,
                            the.var.names,
                            the.var.levels){
  the.hazard <- the.initial.hazard %>%
    mutate_at(vars(logit, ci.low, ci.high),
                 funs(. + effect.val))
  var.labels <- tibble(time = 3:5)
  for(i in 1:length(the.var.names)){
    var.labels <- bind_cols(var.labels,
                            tibble(rep(the.var.levels[i], 3)))
  }
  names(var.labels)[-1] <- the.var.names
  the.hazard <- left_join(the.hazard, var.labels, by = "time") %>%
    mutate_at(vars(logit, ci.low, ci.high),
              funs(odds = exp(.),
                   hazard = exp(.)/(1 + exp(.)))) %>%
    rename(ci.low_logit = ci.low,
           ci.high_logit = ci.high,
           odds = logit_odds,
           hazard = logit_hazard) %>%
    mutate(survival = cumprod(1 - hazard)) %>%
    select(time, the.var.names, survival, matches("hazard"), everything())
surv1 <- tibble(time = 2,
                survival = 1) %>%
  bind_cols(select(var.labels, -time)[1,])
bind_rows(the.hazard, surv1)
}
hazard.int <-function(the.fit, the.main.effects, the.interactions){
  the.initial.hazard <- make.initial.hazard(the.fit)
  # permutations for main effects
  permute.me <- expand.grid(rep(list(-1:1),
                                  length(the.main.effects)))
  # permutations for interactions
  fit.coefs <- the.fit$coefficients
  permute.int <- tibble()
  for (i in 1:length(the.interactions)){
    int.term <- names(fit.coefs)[the.interactions[i]]
    the.terms <- unlist(str_split(int.term, ":"))
    term.positions <- match(the.terms, names(fit.coefs)) %>%
      match(the.main.effects)
    int.indicator <- permute.me[, term.positions] %>%
      apply(1, prod) %>%
      tibble()
    if (length(permute.int) == 0) {
      permute.int <- int.indicator
    } else {
      permute.int <- bind_cols(permute.int, int.indicator)
    }
  }
  names(permute.int) <- paste0("int", the.interactions)
  # create hazard tables
  the.hazard.tables <- tibble()
  for (i in 1:nrow(permute.me)) {
    effect.val <- sum(fit.coefs[the.main.effects] * permute.me[i, ],
                      fit.coefs[the.interactions] * permute.int[i, ])
    the.var.levels <- permute.me[i, ] %>%
      factor(levels = -1:1, labels = c("-1 SD", "Mean", "+1 SD"))
    temp.hazard <- make.hazard.int(the.fit,
                                   the.initial.hazard,
                                   effect.val,
                                   names(fit.coefs[the.main.effects]),
                                   the.var.levels)
    if (length(the.hazard.tables) == 0) {
      the.hazard.tables <- temp.hazard
    } else {
      the.hazard.tables <- bind_rows(the.hazard.tables, temp.hazard)
    }
  }
  the.hazard.tables
}
# Counting Ns
# Mom's, no covariates
md1 <- pp.data %>%
  select(subjid, separation, mDAS.conflict, mDAS.pos.sat, timen) %>%
  filter(timen > 2) %>%
  filter(complete.cases(.))
# N obs: 2518
nrow(md1)
# N subjects: 1103
unique(md1$subjid) %>% length()
# Mom's, with covariates
md2 <- pp.data %>%
  select(subjid, separation, mDAS.conflict, mDAS.pos.sat, timen,
         mother_age, father_age, income, parity) %>%
  filter(timen > 2) %>%
  filter(complete.cases(.))
# N obs: 1653
nrow(md2)
# N subjects: 700
unique(md2$subjid) %>% length()
# Dad's no covariates
fd1 <- pp.data %>%
  select(subjid, separation, fDAS.conflict, fDAS.pos.sat, timen) %>%
  filter(timen > 2) %>%
  filter(complete.cases(.))
# N obs: 1583
nrow(fd1)
# N subjects: 673
unique(fd1$subjid) %>% length()
# Dad's no covariates
fd2 <- pp.data %>%
  select(subjid, separation, fDAS.conflict, fDAS.pos.sat, timen,
         mother_age, father_age, income, parity) %>%
  filter(timen > 2) %>%
  filter(complete.cases(.))
# N obs: 1492
nrow(fd2)
# N subjects: 633
unique(fd2$subjid) %>% length()

## ----survival------------------------------------------------------------
ts <- survfit(Surv(separation.time, 1-censored) ~ 1, conf.type = "none", data = thedata)
emp.hazard <- with(ts, tibble(time, surv, n.event, n.risk))
emp.hazard <- emp.hazard %>%
  mutate(hazard = n.event/n.risk) %>%
  add_row(time = 2, surv = 1, n.event = 0, n.risk = nrow(thedata), hazard = NA)
ggplot(emp.hazard, aes(x = time, y = hazard)) +
  geom_line() +
  geom_point() +
  labs(y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time")
ggplot(emp.hazard, aes(x = time, y = surv)) +
  geom_line() +
  geom_point() +
  labs(y = "Proportion of Couples not Separated") +
  ggtitle("Couples' separation over time")

## ----mom reports---------------------------------------------------------
# fit moms predictors
fit.m1 <- glm(separation ~ - 1 + factor(timen) + mDAS.conflict_c +
                mDAS.pos.sat_c + mDAS.conflict_c:mDAS.pos.sat_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.m1)
# interaction is not significant, so dropping
fit.m2 <- glm(separation ~ - 1 + factor(timen) + mDAS.conflict_c +
                mDAS.pos.sat_c, family="binomial",
              data=filter(pp.data, timen > 2))
summary(fit.m2)
# m conflict and m pos are significant
# still significant with controls for parity, income, and age
fit.m2.1 <- glm(separation ~ - 1 + factor(timen) + mDAS.conflict_c +
                mDAS.pos.sat_c + parity + income + mother_age + father_age,
                family="binomial",
              data=filter(pp.data, timen > 2))
summary(fit.m2.1)

## ----mom conflict plot---------------------------------------------------
# plot for conflict main effect
fit.hazard <- hazard.main.effect(fit.m2, 4)
ggplot(fit.hazard, aes(x = time, y = hazard, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time by mothers' report of conflict")
ggplot(fit.hazard, aes(x = time, y = survival, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict",
       y = "Proportion of Couples Not Separated") +
  ggtitle("Separation over time by mothers' report of conflict")

## ----mom satisfaction plot-----------------------------------------------
# plot for positive main effect
fit.hazard <- hazard.main.effect(fit.m2, 5)
ggplot(fit.hazard, aes(x = time, y = hazard, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Satisfaction",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time by mothers' satisfaction")
ggplot(fit.hazard, aes(x = time, y = survival, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Satisfaction",
       y = "Proportion of Couples Not Separated") +
  ggtitle("Separation over time by mothers' report of satisfaction")

## ----dad reports---------------------------------------------------------
# fit fathers predictors
fit.f1 <- glm(separation ~ - 1 + factor(timen) + fDAS.conflict_c +
                fDAS.pos.sat_c + fDAS.conflict_c:fDAS.pos.sat_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.f1)
# interaction is not significant, so dropping
fit.f2 <- glm(separation ~ - 1 + factor(timen) + fDAS.conflict_c +
                fDAS.pos.sat_c, family="binomial",
              data=filter(pp.data, timen > 2))
summary(fit.f2)
# Father conflict is significant, but satisfaction is not.
# Dropping father satisfaction
fit.f3 <- glm(separation ~ - 1 + factor(timen) + fDAS.conflict_c,
              family="binomial",
              data=filter(pp.data, timen > 2))
summary(fit.f3)
# Still significant with controls for parity, income, and age
fit.f3.1 <- glm(separation ~ - 1 + factor(timen) + fDAS.conflict_c +
                parity + income + mother_age + father_age,
                family="binomial",
              data=filter(pp.data, timen > 2))
summary(fit.f3.1)

## ----dad conflict plots--------------------------------------------------
# plot for conflict main effect
fit.hazard <- hazard.main.effect(fit.f3, 4)
ggplot(fit.hazard, aes(x = time, y = hazard, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time by fathers' report of conflict")
ggplot(fit.hazard, aes(x = time, y = survival, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict",
       y = "Proportion of Couples Not Separated") +
  ggtitle("Separation over time by fathers' report of conflict")

## ----couple reports------------------------------------------------------
# fit couples predictors
fit.c1 <- glm(separation ~ - 1 + factor(timen) + cmean.conflict_c +
                cmean.pos_c + cmean.conflict_c:cmean.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c1)
# dropping interaction term
fit.c2 <- glm(separation ~ - 1 + factor(timen) + cmean.conflict_c +
                cmean.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c2)
# adding controls
# still significant over and above parity, income, and ages
fit.c3 <- glm(separation ~ - 1 + factor(timen) + cmean.conflict_c +
                cmean.pos_c + parity + income + mother_age + father_age,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c3)

## ----couple conflict plots-----------------------------------------------
# plot for conflict main effect
fit.hazard <- hazard.main.effect(fit.c2, 4)
ggplot(fit.hazard, aes(x = time, y = hazard, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time by partners' average report of conflict")
ggplot(fit.hazard, aes(x = time, y = survival, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict",
       y = "Proportion of Couples Not Separated") +
  ggtitle("Separation over time by partners' average report of conflict")

## ----couple satisfaction plots-------------------------------------------
# plot for conflict main effect
fit.hazard <- hazard.main.effect(fit.c2, 5)
ggplot(fit.hazard, aes(x = time, y = hazard, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "satisfaction",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time by partners' average report of satisfaction")
ggplot(fit.hazard, aes(x = time, y = survival, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "satisfaction",
       y = "Proportion of Couples Not Separated") +
  ggtitle("Separation over time by partners' average report of satisfaction")

## ----report aggreement---------------------------------------------------
# fit couples predictors
fit.c1 <- glm(separation ~ -1 + factor(timen) + csd.conflict_c +
                csd.pos_c + csd.conflict_c:csd.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c1)
# dropping interaction term
fit.c2 <- glm(separation ~ -1 + factor(timen) + csd.conflict_c + csd.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c2)
# dropping conflict term
fit.c3 <- glm(separation ~ -1 + factor(timen) + csd.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c3)
# adding controls
# still significant over and above parity, income, and ages
fit.c4 <- glm(separation ~ -1 + factor(timen) + csd.pos_c + parity +
                income + mother_age + father_age,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c4)

## ----satisfaction agreement plot-----------------------------------------
# plot for conflict main effect
fit.hazard <- hazard.main.effect(fit.c3, 4)
ggplot(fit.hazard, aes(x = time, y = hazard, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "satisfaction",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time by the sd of partners' report of satisfaction")
ggplot(fit.hazard, aes(x = time, y = survival, color = main.effect)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "satisfaction",
       y = "Proportion of Couples Not Separated") +
  ggtitle("Separation over time by the sd of partners' report of satisfaction")

## ----satisfaction agreement int------------------------------------------
# mean and sd of satisfaction
fit.c1 <- glm(separation ~ -1 + factor(timen) + csd.pos_c + cmean.pos_c +
                csd.pos_c:cmean.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c1)
# mean and sd of conflict
fit.c2 <- glm(separation ~ -1 + factor(timen) + csd.conflict_c +
                cmean.conflict_c + csd.conflict_c:cmean.conflict_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c2)
# mean of satisfaction by sd of conflict
fit.c3 <- glm(separation ~ -1 + factor(timen) + cmean.pos_c +
                csd.conflict_c + csd.conflict_c:cmean.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c3)
# mean of conflict by sd of satisfaction
fit.c4 <- glm(separation ~ -1 + factor(timen) + cmean.conflict_c +
                csd.pos_c + csd.pos_c:cmean.conflict_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c4)

## ----conflict satisfaction plot------------------------------------------
fit.hazard <- hazard.int(fit.c4, 4:5, 6)
plotdata <- fit.hazard %>% rename(Conflict.Level = cmean.conflict_c,
                                  Satisfaction.Disagreement = csd.pos_c)
ggplot(plotdata, aes(x = time,
                       y = hazard,
                       color = Conflict.Level)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict Level",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time by conflict level and satisfaction disagreement") +
  facet_grid(. ~ Satisfaction.Disagreement, labeller = label_both)
ggplot(plotdata, aes(x = time,
                       y = survival,
                       color = Conflict.Level)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Conflict Level",
       y = "Likelihood of separation") +
  ggtitle("Separation over time by conflict level and satisfaction disagreement") +
  facet_grid(. ~ Satisfaction.Disagreement, labeller = label_both)

## ----three way int-------------------------------------------------------
# three way interactions
# SD of conflict x Mean of conflict x SD of satisfaction
fit.c5 <- glm(separation ~ -1 + factor(timen) +
                # main effects
                csd.conflict_c + cmean.conflict_c +
                csd.pos_c + cmean.pos_c +
                # 2 way terms
                csd.conflict_c:cmean.conflict_c +
                csd.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.pos_c +
                cmean.conflict_c:csd.pos_c +
                cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c +
                # 3 way term
                csd.conflict_c:cmean.conflict_c:csd.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c5)
# SD of conflict x Mean of conflict x Mean of satisfaction
fit.c6 <- glm(separation ~ -1 + factor(timen) +
                # main effects
                csd.conflict_c + cmean.conflict_c +
                csd.pos_c + cmean.pos_c +
                # 2 way terms
                csd.conflict_c:cmean.conflict_c +
                csd.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.pos_c +
                cmean.conflict_c:csd.pos_c +
                cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c +
                # 3 way term
                csd.conflict_c:cmean.conflict_c:cmean.pos_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c6)
# SD of satisfaction x Mean of conflict x Mean of satisfaction
fit.c7 <- glm(separation ~ -1 + factor(timen) +
                # main effects
                csd.conflict_c + cmean.conflict_c +
                csd.pos_c + cmean.pos_c +
                # 2 way terms
                csd.conflict_c:cmean.conflict_c +
                csd.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.pos_c +
                cmean.conflict_c:csd.pos_c +
                cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c +
                # 3 way term
                csd.pos_c:cmean.pos_c:cmean.conflict_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c7)
# SD of satisfaction x Mean of satisfaction x SD of conflict
fit.c8 <- glm(separation ~ -1 + factor(timen) +
                # main effects
                csd.conflict_c + cmean.conflict_c +
                csd.pos_c + cmean.pos_c +
                # 2 way terms
                csd.conflict_c:cmean.conflict_c +
                csd.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.pos_c +
                cmean.conflict_c:csd.pos_c +
                cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c +
                # 3 way term
                csd.pos_c:cmean.pos_c:csd.conflict_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c8)
# all 3-way interactions together
fit.c9 <- glm(separation ~ -1 + factor(timen) +
                # main effects
                csd.conflict_c + cmean.conflict_c +
                csd.pos_c + cmean.pos_c +
                # 2 way terms
                csd.conflict_c:cmean.conflict_c +
                csd.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.pos_c +
                cmean.conflict_c:csd.pos_c +
                cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c +
                # 3 way terms
                csd.conflict_c:cmean.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c:cmean.conflict_c +
                csd.pos_c:cmean.pos_c:csd.conflict_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c9)
# Now 2 of them are significant
# csd.conflict_c:cmean.conflict_c:csd.pos_c
# csd.conflict_c:csd.pos_c:cmean.pos_c 
# dropping the other 3-way terms that are not significant
fit.c10 <- glm(separation ~ -1 + factor(timen) +
                # main effects
                csd.conflict_c + cmean.conflict_c +
                csd.pos_c + cmean.pos_c +
                # 2 way terms
                csd.conflict_c:cmean.conflict_c +
                csd.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.pos_c +
                cmean.conflict_c:csd.pos_c +
                cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c +
                # 3 way terms
                csd.conflict_c:cmean.conflict_c:csd.pos_c +
                csd.pos_c:cmean.pos_c:csd.conflict_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c10)

## ----three way plot1-----------------------------------------------------
# csd.conflict_c:cmean.conflict_c:csd.pos_c
# make hazard table to visualize interactions
fit.hazard <- hazard.int(fit.c10, 4:6, c(8, 9, 11, 14))
plotdata <- fit.hazard %>% rename(Conflict.Disagreement = csd.conflict_c,
                                    Conflict.Level = cmean.conflict_c,
                                    Satisfaction.Disagreement = csd.pos_c)
ggplot(plotdata, aes(x = time,
                       y = hazard,
                       color = Conflict.Disagreement)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Disagreement on level conflict",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time") +
  facet_grid(Satisfaction.Disagreement ~ Conflict.Level, labeller = label_both)
ggplot(plotdata, aes(x = time,
                       y = survival,
                       color = Conflict.Disagreement)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Disagreement on level conflict",
       y = "Proportion of Couples Still Together") +
  ggtitle("Couples' separation over time") +
  facet_grid(Satisfaction.Disagreement ~ Conflict.Level, labeller = label_both)

## ----three way plot2-----------------------------------------------------
# csd.pos_c:cmean.pos_c:csd.conflict_c
# make hazard table to visualize interactions
fit.hazard <- hazard.int(fit.c10, c(4, 6, 7), c(9, 10, 13, 15))
plotdata <- fit.hazard %>% rename(Conflict.Disagreement = csd.conflict_c,
                                    Satisfaction.Level = cmean.pos_c,
                                    Satisfaction.Disagreement = csd.pos_c)
ggplot(plotdata, aes(x = time,
                       y = hazard,
                       color = Conflict.Disagreement)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Disagreement on level conflict",
       y = "Likelihood of separation") +
  ggtitle("Likelihood of separation over time") +
  facet_grid(Satisfaction.Disagreement ~ Satisfaction.Level, labeller = label_both)
ggplot(plotdata, aes(x = time,
                       y = survival,
                       color = Conflict.Disagreement)) +
  geom_line() +
  geom_point() +
  xlim(c(2, 5)) +
  labs(color = "Disagreement on level conflict",
       y = "Proportion of Couples Still Together") +
  ggtitle("Couples' separation over time") +
  facet_grid(Satisfaction.Disagreement ~ Satisfaction.Level, labeller = label_both)

## ----four way------------------------------------------------------------
# four way interaction
fit.c11 <- glm(separation ~ -1 + factor(timen) +
                # main effects
                csd.conflict_c + cmean.conflict_c +
                csd.pos_c + cmean.pos_c +
                # 2 way terms
                csd.conflict_c:cmean.conflict_c +
                csd.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.pos_c +
                cmean.conflict_c:csd.pos_c +
                cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c +
                # 3 way terms
                csd.conflict_c:cmean.conflict_c:csd.pos_c +
                csd.conflict_c:cmean.conflict_c:cmean.pos_c +
                csd.pos_c:cmean.pos_c:cmean.conflict_c +
                csd.pos_c:cmean.pos_c:csd.conflict_c +
                # 4 way term
                csd.pos_c:cmean.pos_c:csd.conflict_c:cmean.conflict_c,
              family="binomial", data=filter(pp.data, timen > 2))
summary(fit.c11)

