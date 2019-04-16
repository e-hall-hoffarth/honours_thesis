setwd('/mnt/good/honours_thesis/')

library(stargazer)
library(ggplot2)
library(lfe)
library(zoo)
library(lubridate)

######
# Data Definitions
comp <- read.csv('data/COMPUSTAT_vars.csv')

n_breaches <- aggregate(comp$breachdate, by=list(comp$gvkey), FUN=(function(x) length(unique(x))))
names(n_breaches) <- c('gvkey', 'nbreaches')
comp <- merge(comp, n_breaches, by='gvkey', all.x = TRUE, all.y = FALSE)

# Run this for firstbreach only
comp$breachdate <- as.Date(comp$breachdate)
first_breaches <- aggregate(comp$breachdate, by=list(comp$gvkey), FUN=min)
names(first_breaches) <- c('gvkey', 'breachdate')
compfb <- merge(comp, first_breaches, by = c('gvkey', 'breachdate'), all.x = F, all.y = T)

subset_rev_6m <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$log_revtq) &
                      !is.infinite(comp$log_revtq) &
                      comp$quarters_since_breach %in% -40:2,]
subset_rev_1y <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$log_revtq) &
                      !is.infinite(comp$log_revtq) &
                      comp$quarters_since_breach %in% -40:4,]
subset_rev_2y <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$log_revtq) &
                      !is.infinite(comp$log_revtq) &
                      comp$quarters_since_breach %in% -40:8,]
subset_rev_3y <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$log_revtq) &
                      !is.infinite(comp$log_revtq) &
                      comp$quarters_since_breach %in% -40:12,]

subset_rev_6mfb <- compfb[!is.na(compfb$quarters_since_breach) &
                        !is.na(compfb$log_revtq) &
                        !is.infinite(compfb$log_revtq) &
                        compfb$quarters_since_breach %in% -40:2,]
subset_rev_1yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                        !is.na(compfb$log_revtq) &
                        !is.infinite(compfb$log_revtq) &
                        compfb$quarters_since_breach %in% -40:4,]
subset_rev_2yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                        !is.na(compfb$log_revtq) &
                        !is.infinite(compfb$log_revtq) &
                        compfb$quarters_since_breach %in% -40:8,]
subset_rev_3yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                        !is.na(compfb$log_revtq) &
                        !is.infinite(compfb$log_revtq) &
                        compfb$quarters_since_breach %in% -40:12,]

subset_rev_placebo_m24 <- comp[!is.na(comp$quarters_since_breach) &
                               !is.na(comp$log_revtq) &
                               !is.infinite(comp$log_revtq) &
                               comp$quarters_since_breach %in% -40:-4,]
subset_rev_placebo_m36 <- comp[!is.na(comp$quarters_since_breach) &
                                 !is.na(comp$log_revtq) &
                                 !is.infinite(comp$log_revtq) &
                                 comp$quarters_since_breach %in% -40:-8,]
subset_rev_placebo_p24 <- comp[!is.na(comp$quarters_since_breach) &
                                 !is.na(comp$log_revtq) &
                                 !is.infinite(comp$log_revtq) &
                                 comp$quarters_since_breach %in% -40:12,]
subset_rev_placebo_p36 <- comp[!is.na(comp$quarters_since_breach) &
                                 !is.na(comp$log_revtq) &
                                 !is.infinite(comp$log_revtq) &
                                 comp$quarters_since_breach %in% -40:16,]

subset_pr_6m <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$niq) & 
                    !is.infinite(comp$niq) &
                    comp$quarters_since_breach %in% -40:2,]
subset_pr_1y <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$niq) & 
                    !is.infinite(comp$niq) &
                    comp$quarters_since_breach %in% -40:4,]
subset_pr_2y <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$niq) & 
                    !is.infinite(comp$niq) &
                    comp$quarters_since_breach %in% -40:8,]
subset_pr_3y <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$niq) & 
                    !is.infinite(comp$niq) &
                    comp$quarters_since_breach %in% -40:12,]

subset_pr_6mfb <- compfb[!is.na(compfb$quarters_since_breach) &
                            !is.na(compfb$niq) &
                            !is.infinite(compfb$niq) &
                            compfb$quarters_since_breach %in% -40:2,]
subset_pr_1yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                            !is.na(compfb$niq) &
                            !is.infinite(compfb$niq) &
                            compfb$quarters_since_breach %in% -40:4,]
subset_pr_2yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                            !is.na(compfb$niq) &
                            !is.infinite(compfb$niq) &
                            compfb$quarters_since_breach %in% -40:8,]
subset_pr_3yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                            !is.na(compfb$niq) &
                            !is.infinite(compfb$niq) &
                            compfb$quarters_since_breach %in% -40:12,]

subset_pr_placebo_m24 <- comp[!is.na(comp$quarters_since_breach) &
                                 !is.na(comp$niq) &
                                 !is.infinite(comp$niq) &
                                 comp$quarters_since_breach %in% -40:-4,]
subset_pr_placebo_m36 <- comp[!is.na(comp$quarters_since_breach) &
                                 !is.na(comp$niq) &
                                 !is.infinite(comp$niq) &
                                 comp$quarters_since_breach %in% -40:-8,]
subset_pr_placebo_p24 <- comp[!is.na(comp$quarters_since_breach) &
                                 !is.na(comp$niq) &
                                 !is.infinite(comp$niq) &
                                 comp$quarters_since_breach %in% -40:12,]
subset_pr_placebo_p36 <- comp[!is.na(comp$quarters_since_breach) &
                                 !is.na(comp$niq) &
                                 !is.infinite(comp$niq) &
                                 comp$quarters_since_breach %in% -40:16,]

subset_xopr_6m <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$xoprq) & 
                      !is.infinite(comp$xoprq) &
                      comp$quarters_since_breach %in% -40:2,]
subset_xopr_1y <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$xoprq) & 
                      !is.infinite(comp$xoprq) &
                      comp$quarters_since_breach %in% -40:4,]
subset_xopr_2y <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$xoprq) & 
                      !is.infinite(comp$xoprq) &
                      comp$quarters_since_breach %in% -40:8,]
subset_xopr_3y <- comp[!is.na(comp$quarters_since_breach) &
                      !is.na(comp$xoprq) & 
                      !is.infinite(comp$xoprq) &
                      comp$quarters_since_breach %in% -40:12,]

subset_xopr_6mfb <- compfb[!is.na(compfb$quarters_since_breach) &
                           !is.na(compfb$xoprq) &
                           !is.infinite(compfb$xoprq) &
                           compfb$quarters_since_breach %in% -40:2,]
subset_xopr_1yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                           !is.na(compfb$xoprq) &
                           !is.infinite(compfb$xoprq) &
                           compfb$quarters_since_breach %in% -40:4,]
subset_xopr_2yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                           !is.na(compfb$xoprq) &
                           !is.infinite(compfb$xoprq) &
                           compfb$quarters_since_breach %in% -40:8,]
subset_xopr_3yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                           !is.na(compfb$xoprq) &
                           !is.infinite(compfb$xoprq) &
                           compfb$quarters_since_breach %in% -40:12,]

subset_xopr_placebo_m24 <- comp[!is.na(comp$quarters_since_breach) &
                                !is.na(comp$xoprq) &
                                !is.infinite(comp$xoprq) &
                                comp$quarters_since_breach %in% -40:-4,]
subset_xopr_placebo_m36 <- comp[!is.na(comp$quarters_since_breach) &
                                !is.na(comp$xoprq) &
                                !is.infinite(comp$xoprq) &
                                comp$quarters_since_breach %in% -40:-8,]
subset_xopr_placebo_p24 <- comp[!is.na(comp$quarters_since_breach) &
                                !is.na(comp$xoprq) &
                                !is.infinite(comp$xoprq) &
                                comp$quarters_since_breach %in% -40:12,]
subset_xopr_placebo_p36 <- comp[!is.na(comp$quarters_since_breach) &
                                !is.na(comp$xoprq) &
                                !is.infinite(comp$xoprq) &
                                comp$quarters_since_breach %in% -40:16,]

subset_nop_6m <- comp[!is.na(comp$quarters_since_breach) &
                         !is.na(comp$nopiq) & 
                         !is.infinite(comp$nopiq) &
                         comp$quarters_since_breach %in% -40:2,]
subset_nop_1y <- comp[!is.na(comp$quarters_since_breach) &
                         !is.na(comp$nopiq) & 
                         !is.infinite(comp$nopiq) &
                         comp$quarters_since_breach %in% -40:4,]
subset_nop_2y <- comp[!is.na(comp$quarters_since_breach) &
                         !is.na(comp$nopiq) & 
                         !is.infinite(comp$nopiq) &
                         comp$quarters_since_breach %in% -40:8,]
subset_nop_3y <- comp[!is.na(comp$quarters_since_breach) &
                         !is.na(comp$nopiq) & 
                         !is.infinite(comp$nopiq) &
                         comp$quarters_since_breach %in% -40:12,]

subset_nop_6mfb <- compfb[!is.na(compfb$quarters_since_breach) &
                             !is.na(compfb$nopiq) &
                             !is.infinite(compfb$nopiq) &
                             compfb$quarters_since_breach %in% -40:2,]
subset_nop_1yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                             !is.na(compfb$nopiq) &
                             !is.infinite(compfb$nopiq) &
                             compfb$quarters_since_breach %in% -40:4,]
subset_nop_2yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                             !is.na(compfb$nopiq) &
                             !is.infinite(compfb$nopiq) &
                             compfb$quarters_since_breach %in% -40:8,]
subset_nop_3yfb <- compfb[!is.na(compfb$quarters_since_breach) &
                             !is.na(compfb$nopiq) &
                             !is.infinite(compfb$nopiq) &
                             compfb$quarters_since_breach %in% -40:12,]

subset_nop_placebo_m24 <- comp[!is.na(comp$quarters_since_breach) &
                                  !is.na(comp$nopiq) &
                                  !is.infinite(comp$nopiq) &
                                  comp$quarters_since_breach %in% -40:-4,]
subset_nop_placebo_m36 <- comp[!is.na(comp$quarters_since_breach) &
                                  !is.na(comp$nopiq) &
                                  !is.infinite(comp$nopiq) &
                                  comp$quarters_since_breach %in% -40:-8,]
subset_nop_placebo_p24 <- comp[!is.na(comp$quarters_since_breach) &
                                  !is.na(comp$nopiq) &
                                  !is.infinite(comp$nopiq) &
                                  comp$quarters_since_breach %in% -40:12,]
subset_nop_placebo_p36 <- comp[!is.na(comp$quarters_since_breach) &
                                  !is.na(comp$nopiq) &
                                  !is.infinite(comp$nopiq) &
                                  comp$quarters_since_breach %in% -40:16,]

subset_test_3m<- comp[!is.na(comp$quarters_since_breach) &
                        comp$quarters_since_breach %in% -40:1,]
subset_test_6m<- comp[!is.na(comp$quarters_since_breach) &
                        comp$quarters_since_breach %in% -40:2,]
subset_test_1y<- comp[!is.na(comp$quarters_since_breach) &
                      comp$quarters_since_breach %in% -40:4,]
subset_test_2y<- comp[!is.na(comp$quarters_since_breach) &
                        comp$quarters_since_breach %in% -40:1,]
subset_test_3y<- comp[!is.na(comp$quarters_since_breach) &
                        comp$quarters_since_breach %in% -40:2,]

subset_test_3mfb<- compfb[!is.na(compfb$quarters_since_breach) &
                        compfb$quarters_since_breach %in% -40:1,]
subset_test_6mfb<- compfb[!is.na(compfb$quarters_since_breach) &
                        compfb$quarters_since_breach %in% -40:2,]
subset_test_1yfb<- compfb[!is.na(compfb$quarters_since_breach) &
                        compfb$quarters_since_breach %in% -40:4,]
subset_test_2yfb<- compfb[!is.na(compfb$quarters_since_breach) &
                        compfb$quarters_since_breach %in% -40:1,]
subset_test_3yfb<- compfb[!is.na(compfb$quarters_since_breach) &
                        compfb$quarters_since_breach %in% -40:2,]

subset_test_placebo_m24 <- comp[!is.na(comp$quarters_since_breach) &
                                  comp$quarters_since_breach %in% -40:-4,]
subset_test_placebo_m36 <- comp[!is.na(comp$quarters_since_breach) &
                                  comp$quarters_since_breach %in% -40:-8,]
subset_test_placebo_p24 <- comp[!is.na(comp$quarters_since_breach) &
                                  comp$quarters_since_breach %in% -40:12,]
subset_test_placebo_p36 <- comp[!is.na(comp$quarters_since_breach) &
                                  comp$quarters_since_breach %in% -40:16,]

rev_6m_mean = round(mean(subset_rev_6m$revtq),2)
rev_6m_sd = round(sd(subset_rev_6m$revtq),2)
rev_6m_meanfb = round(mean(subset_rev_6mfb$revtq),2)
rev_6m_sdfb = round(sd(subset_rev_6mfb$revtq),2)

logrev_6m_mean = round(mean(subset_rev_6m$log_revtq),2)
logrev_6m_sd = round(sd(subset_rev_6m$log_revtq),2)
logrev_6m_meanfb = round(mean(subset_rev_6mfb$log_revtq),2)
logrev_6m_sdfb = round(sd(subset_rev_6mfb$log_revtq),2)

rev_1y_mean = round(mean(subset_rev_1y$revtq),2)
rev_1y_sd = round(sd(subset_rev_1y$revtq),2)
rev_1y_meanfb = round(mean(subset_rev_1yfb$revtq),2)
rev_1y_sdfb = round(sd(subset_rev_1yfb$revtq),2)

logrev_1y_mean = round(mean(subset_rev_1y$log_revtq),2)
logrev_1y_sd = round(sd(subset_rev_1y$log_revtq),2)
logrev_1y_meanfb = round(mean(subset_rev_1yfb$log_revtq),2)
logrev_1y_sdfb = round(sd(subset_rev_1yfb$log_revtq),2)

rev_2y_mean = round(mean(subset_rev_2y$revtq),2)
rev_2y_sd = round(sd(subset_rev_2y$revtq),2)
rev_2y_meanfb = round(mean(subset_rev_2yfb$revtq),2)
rev_2y_sdfb = round(sd(subset_rev_2yfb$revtq),2)

logrev_2y_mean = round(mean(subset_rev_2y$log_revtq),2)
logrev_2y_sd = round(sd(subset_rev_2y$log_revtq),2)
logrev_2y_meanfb = round(mean(subset_rev_2yfb$log_revtq),2)
logrev_2y_sdfb = round(sd(subset_rev_2yfb$log_revtq),2)

rev_3y_mean = round(mean(subset_rev_3y$revtq),2)
rev_3y_sd = round(sd(subset_rev_3y$revtq),2)
rev_3y_meanfb = round(mean(subset_rev_3yfb$revtq),2)
rev_3y_sdfb = round(sd(subset_rev_3yfb$revtq),2)

logrev_3y_mean = round(mean(subset_rev_3y$log_revtq),2)
logrev_3y_sd = round(sd(subset_rev_3y$log_revtq),2)
logrev_3y_meanfb = round(mean(subset_rev_3yfb$log_revtq),2)
logrev_3y_sdfb = round(sd(subset_rev_3yfb$log_revtq),2)

pr_6m_mean <- round(mean(subset_pr_6m$niq),2)
pr_6m_sd <- round(sd(subset_pr_6m$niq), 2)
pr_6m_meanfb <- round(mean(subset_pr_6mfb$niq),2)
pr_6m_sdfb <- round(sd(subset_pr_6mfb$niq), 2)

pr_1y_mean <- round(mean(subset_pr_1y$niq),2)
pr_1y_sd <- round(sd(subset_pr_1y$niq), 2)
pr_1y_meanfb <- round(mean(subset_pr_1yfb$niq),2)
pr_1y_sdfb <- round(sd(subset_pr_1yfb$niq), 2)

pr_2y_mean <- round(mean(subset_pr_2y$niq),2)
pr_2y_sd <- round(sd(subset_pr_2y$niq), 2)
pr_2y_meanfb <- round(mean(subset_pr_2yfb$niq),2)
pr_2y_sdfb <- round(sd(subset_pr_2yfb$niq), 2)

pr_3y_mean <- round(mean(subset_pr_3y$niq),2)
pr_3y_sd <- round(sd(subset_pr_3y$niq), 2)
pr_3y_meanfb <- round(mean(subset_pr_3yfb$niq),2)
pr_3y_sdfb <- round(sd(subset_pr_3yfb$niq), 2)

xopr_6m_mean <- round(mean(subset_xopr_6m$xoprq),2)
xopr_6m_sd <- round(sd(subset_xopr_6m$xoprq), 2)
xopr_6m_meanfb <- round(mean(subset_xopr_6mfb$xoprq),2)
xopr_6m_sdfb <- round(sd(subset_xopr_6mfb$xoprq), 2)

xopr_1y_mean <- round(mean(subset_xopr_1y$xoprq),2)
xopr_1y_sd <- round(sd(subset_xopr_1y$xoprq), 2)
xopr_1y_meanfb <- round(mean(subset_xopr_1yfb$xoprq),2)
xopr_1y_sdfb <- round(sd(subset_xopr_1yfb$xoprq), 2)

xopr_2y_mean <- round(mean(subset_xopr_2y$xoprq),2)
xopr_2y_sd <- round(sd(subset_xopr_2y$xoprq), 2)
xopr_2y_meanfb <- round(mean(subset_xopr_2yfb$xoprq),2)
xopr_2y_sdfb <- round(sd(subset_xopr_2yfb$xoprq), 2)

xopr_3y_mean <- round(mean(subset_xopr_3y$xoprq),2)
xopr_3y_sd <- round(sd(subset_xopr_3y$xoprq), 2)
xopr_3y_meanfb <- round(mean(subset_xopr_3yfb$xoprq),2)
xopr_3y_sdfb <- round(sd(subset_xopr_3yfb$xoprq), 2)

mean_ceqq_1y <- round(mean(subset_test_1y$ceqq, na.rm=T), 2)
mean_xsgaq_1y <-round(mean(subset_test_1y$xsgaq, na.rm=T), 2)
mean_emp_1y <- round(mean(subset_test_1y$emp, na.rm=T), 2)
mean_trend_index_company_1y <- round(mean(subset_test_1y$trend_index_company, na.rm=T), 2)
mean_trend_index_tic_1y <- round(mean(subset_test_1y$trend_index_tic, na.rm=T), 2)
sd_ceqq_1y <- round(sd(subset_test_1y$ceqq, na.rm=T), 2)
sd_xsgaq_1y <-round(sd(subset_test_1y$xsgaq, na.rm=T), 2)
sd_emp_1y <- round(sd(subset_test_1y$emp, na.rm=T), 2)
sd_trend_index_company_1y <- round(sd(subset_test_1y$trend_index_company, na.rm=T), 2)
sd_trend_index_tic_1y <- round(sd(subset_test_1y$trend_index_tic, na.rm=T), 2)


es_controls <- read.csv('data/event_study_controls.csv')
n_breaches <- aggregate(comp$breachdate, by=list(comp$gvkey), FUN=(function(x) length(unique(x))))
names(n_breaches) <- c('GVKEY', 'nbreaches')
es_controls <- merge(es_controls, n_breaches, by='GVKEY', all.x = TRUE, all.y = FALSE)

es_controls$breach_number <- lapply(es_controls$ticker_occurance, FUN=(function(x) strsplit(x, "_")[2]))
es_controls$Total.Records_tolog <- es_controls$Total.Records
es_controls[!is.na(es_controls$Total.Records) & es_controls$Total.Records == 0,]$Total.Records_tolog <- 0.0001
es_controls$Total.Records_log <- log(es_controls$Total.Records_tolog)
es_controls$breachdate <- as.Date(es_controls$Date.Made.Public, format="%B %d, %Y")
es_controls$days_since_begin <- es_controls$breachdate - as.Date("2005-01-01")

######
# Absolute Revenue:
# Specification 1
stargazer(felm(revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y), 
          felm(revtq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          felm(revtq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          felm(revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Revenue'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', rev_1y_mean, rev_1y_mean, rev_1y_mean, rev_1y_mean, rev_1y_mean),
                           c('Dependant SD', rev_1y_sd, rev_1y_sd, rev_1y_sd, rev_1y_sd, rev_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/revenue_specification1_1y.tex',
          type='latex')

# Specification 2 (Event windows)
stargazer(felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_6m),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y), 
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_2y),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_3y),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Revenue (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', rev_6m_mean, rev_1y_mean, rev_2y_mean, rev_3y_mean),
                           c('Dependant SD', rev_6m_sd, rev_1y_sd, rev_2y_sd, rev_3y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/revenue_specification2.tex',
          type='latex')

# Specification 3 (First Breaches)
stargazer(felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_6mfb),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1yfb), 
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_2yfb),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_3yfb),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Revenue (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', rev_6m_meanfb, rev_1y_meanfb, rev_2y_meanfb, rev_3y_meanfb),
                           c('Dependant SD', rev_6m_sdfb, rev_1y_sdfb, rev_2y_sdfb, rev_3y_sdfb)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/revenue_specification3.tex',
          type='latex')

# Specificaiton 4 (Robustness)
stargazer(felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          felm(revtq ~ after_m24 + after_m24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_m24), 
          felm(revtq ~ after_m36 + after_m36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_m36), 
          felm(revtq ~ after_p24 + after_p24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_p24),
          felm(revtq ~ after_p36 + after_p36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_p36),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Revenue (offset of event date)'),
          column.labels = c('(0)', '(-2 Years)', '(-3 Years)', '(+2 Years)', '(+3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', rev_1y_mean, rev_1y_mean, rev_1y_mean, rev_1y_mean, rev_1y_mean),
                           c('Dependant SD', rev_1y_sd, rev_1y_sd, rev_1y_sd, rev_1y_sd, rev_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/revenue_specification4.tex',
          type='latex')

# Specification 5
stargazer(felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches == 1,]),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches == 2,]), 
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches == 3,]),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches >= 4,]),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Revenue (Number of Breaches for Firm)'),
          column.labels = c('(1)', '(2)', '(3)', '(4+)'),
          omit.stat = 'ser',
          add.lines = list(c('Number of Data Breaches ', length(unique(subset_rev_1y[subset_rev_1y$nbreaches == 1,]$Date.Made.Public)), length(unique(subset_rev_1y[subset_rev_1y$nbreaches == 2,]$Date.Made.Public)),length(unique(subset_rev_1y[subset_rev_1y$nbreaches == 3,]$Date.Made.Public)),length(unique(subset_rev_1y[subset_rev_1y$nbreaches >= 4,]$Date.Made.Public))),
                           c('Dependant Mean', round(mean(subset_rev_1y[subset_rev_1y$nbreaches == 1,]$revtq),2), round(mean(subset_rev_1y[subset_rev_1y$nbreaches == 2,]$revtq),2), round(mean(subset_rev_1y[subset_rev_1y$nbreaches == 3,]$revtq),2), round(mean(subset_rev_1y[subset_rev_1y$nbreaches >= 4,]$revtq),2)),
                           c('Dependant SD', round(sd(subset_rev_1y[subset_rev_1y$nbreaches == 1,]$revtq),2), round(sd(subset_rev_1y[subset_rev_1y$nbreaches == 2,]$revtq),2), round(sd(subset_rev_1y[subset_rev_1y$nbreaches == 3,]$revtq),2), round(sd(subset_rev_1y[subset_rev_1y$nbreaches >= 4,]$revtq),2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications',
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/revenue_specification5.tex',
          type='latex')


######
# Log Revenue:
# Specification 1
stargazer(felm(log_revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y), 
          felm(log_revtq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          felm(log_revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Log Revenue'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', logrev_1y_mean, logrev_1y_mean, logrev_1y_mean, logrev_1y_mean, logrev_1y_mean),
                           c('Dependant SD', logrev_1y_sd, logrev_1y_sd, logrev_1y_sd, logrev_1y_sd, logrev_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/logrevenue_specification1_1y.tex',
          type='latex')

# Specification 2 (Event windows)
stargazer(felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_6m),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y), 
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_2y),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_3y),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Log Revenue (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', logrev_6m_mean, logrev_1y_mean, logrev_2y_mean, logrev_3y_mean),
                           c('Dependant SD', logrev_6m_sd, logrev_1y_sd, logrev_2y_sd, logrev_3y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/logrevenue_specification2.tex',
          type='latex')

# Specification 3 (First Breach)
stargazer(felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_6mfb),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1yfb), 
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_2yfb),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_3yfb),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Log Revenue (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', logrev_6m_meanfb, logrev_1y_meanfb, logrev_2y_meanfb, logrev_3y_meanfb),
                           c('Dependant SD', logrev_6m_sdfb, logrev_1y_sdfb, logrev_2y_sdfb, logrev_3y_sdfb)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/logrevenue_specification3.tex',
          type='latex')

# Specificaiton 4 (Robustness)
stargazer(felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y),
          felm(log_revtq ~ after_m24 + after_m24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_m24), 
          felm(log_revtq ~ after_m36 + after_m36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_m36), 
          felm(log_revtq ~ after_p24 + after_p24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_p24),
          felm(log_revtq ~ after_p36 + after_p36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_placebo_p36),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Log Revenue (offset of event date)'),
          column.labels = c('(0)', '(-2 Years)', '(-3 Years)', '(+2 Years)', '(+3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', logrev_1y_mean, logrev_1y_mean, logrev_1y_mean, logrev_1y_mean, logrev_1y_mean),
                           c('Dependant SD', logrev_1y_sd, logrev_1y_sd, logrev_1y_sd, logrev_1y_sd, logrev_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/logrevenue_specification4.tex',
          type='latex')

# Specification 5
stargazer(felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches == 1,]),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches == 2,]), 
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches == 3,]),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_rev_1y[subset_rev_1y$nbreaches >= 4,]),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Log Revenue (Number of Breaches for Firm)'),
          column.labels = c('(1)', '(2)', '(3)', '(4+)'),
          omit.stat = 'ser',
          add.lines = list(c('Number of Data Breaches ', length(unique(subset_rev_1y[subset_rev_1y$nbreaches == 1,]$Date.Made.Public)), length(unique(subset_rev_1y[subset_rev_1y$nbreaches == 2,]$Date.Made.Public)),length(unique(subset_rev_1y[subset_rev_1y$nbreaches == 3,]$Date.Made.Public)),length(unique(subset_rev_1y[subset_rev_1y$nbreaches >= 4,]$Date.Made.Public))),
                           c('Dependant Mean', round(mean(subset_rev_1y[subset_rev_1y$nbreaches == 1,]$log_revtq),2), round(mean(subset_rev_1y[subset_rev_1y$nbreaches == 2,]$log_revtq),2), round(mean(subset_rev_1y[subset_rev_1y$nbreaches == 3,]$log_revtq),2), round(mean(subset_rev_1y[subset_rev_1y$nbreaches >= 4,]$log_revtq),2)),
                           c('Dependant SD', round(sd(subset_rev_1y[subset_rev_1y$nbreaches == 1,]$log_revtq),2), round(sd(subset_rev_1y[subset_rev_1y$nbreaches == 2,]$log_revtq),2), round(sd(subset_rev_1y[subset_rev_1y$nbreaches == 3,]$log_revtq),2), round(sd(subset_rev_1y[subset_rev_1y$nbreaches >= 4,]$log_revtq),2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications',
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/logrevenue_specification5.tex',
          type='latex')


######
# Profit
# Specification 1
stargazer(felm(niq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y), 
          felm(niq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y),
          felm(niq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y),
          felm(niq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Net Income'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', pr_1y_mean, pr_1y_mean, pr_1y_mean, pr_1y_mean, pr_1y_mean),
                           c('Dependant SD', pr_1y_sd, pr_1y_sd, pr_1y_sd, pr_1y_sd, pr_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/profit_specification1_1y.tex',
          type='latex')

# Specification 2 (Event windows)
stargazer(felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_6m),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y), 
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_2y),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_3y),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Net Income (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', pr_6m_mean, pr_1y_mean, pr_2y_mean, pr_3y_mean),
                           c('Dependant SD', pr_6m_sd, pr_1y_sd, pr_2y_sd, pr_3y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/profit_specification2.tex',
          type='latex')

# Specification 3 (First Breach)
stargazer(felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_6mfb),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1yfb), 
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_2yfb),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_3yfb),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Net Income (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', pr_6m_meanfb, pr_1y_meanfb, pr_2y_meanfb, pr_3y_meanfb),
                           c('Dependant SD', pr_6m_sdfb, pr_1y_sdfb, pr_2y_sdfb, pr_3y_sdfb)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/profit_specification3.tex',
          type='latex')

# Specificaiton 4 (Robustness)
stargazer(felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y),
          felm(niq ~ after_m24 + after_m24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_placebo_m24), 
          felm(niq ~ after_m36 + after_m36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_placebo_m36), 
          felm(niq ~ after_p24 + after_p24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_placebo_p24),
          felm(niq ~ after_p36 + after_p36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_placebo_p36),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Net Income (Offset of Event Date)'),
          column.labels = c('(0)', '(-2 Years)', '(-3 Years)', '(+2 Years)', '(+3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', pr_1y_mean, pr_1y_mean, pr_1y_mean, pr_1y_mean, pr_1y_mean),
                           c('Dependant SD', pr_1y_sd, pr_1y_sd, pr_1y_sd, pr_1y_sd, pr_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/profit_specification4.tex',
          type='latex')

# Specification 5
stargazer(felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y[subset_pr_1y$nbreaches == 1,]),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y[subset_pr_1y$nbreaches == 2,]), 
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y[subset_pr_1y$nbreaches == 3,]),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr_1y[subset_pr_1y$nbreaches >= 4,]),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Net Income (Number of Breaches for Firm)'),
          column.labels = c('(1)', '(2)', '(3)', '(4+)'),
          omit.stat = 'ser',
          add.lines = list(c('Number of Data Breaches ', length(unique(subset_pr_1y[subset_pr_1y$nbreaches == 1,]$Date.Made.Public)), length(unique(subset_pr_1y[subset_pr_1y$nbreaches == 2,]$Date.Made.Public)),length(unique(subset_pr_1y[subset_pr_1y$nbreaches == 3,]$Date.Made.Public)),length(unique(subset_pr_1y[subset_pr_1y$nbreaches >= 4,]$Date.Made.Public))),
                           c('Dependant Mean', round(mean(subset_pr_1y[subset_pr_1y$nbreaches == 1,]$niq),2), round(mean(subset_pr_1y[subset_pr_1y$nbreaches == 2,]$niq),2), round(mean(subset_pr_1y[subset_pr_1y$nbreaches == 3,]$niq),2), round(mean(subset_pr_1y[subset_pr_1y$nbreaches >= 4,]$niq),2)),
                           c('Dependant SD', round(sd(subset_pr_1y[subset_pr_1y$nbreaches == 1,]$niq),2), round(sd(subset_pr_1y[subset_pr_1y$nbreaches == 2,]$niq),2), round(sd(subset_pr_1y[subset_pr_1y$nbreaches == 3,]$niq),2), round(sd(subset_pr_1y[subset_pr_1y$nbreaches >= 4,]$niq),2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications',
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/profit_specification5.tex',
          type='latex')

######
# Operating expenses
# Specification 1
stargazer(felm(xoprq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y), 
          felm(xoprq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y),
          felm(xoprq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y),
          felm(xoprq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Operating Expenses'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', xopr_1y_mean, xopr_1y_mean, xopr_1y_mean, xopr_1y_mean, xopr_1y_mean),
                           c('Dependant SD', xopr_1y_sd, xopr_1y_sd, xopr_1y_sd, xopr_1y_sd, xopr_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/expense_specification1_1y.tex',
          type='latex')

# Specification 2 (Event windows)
stargazer(felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_6m),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y), 
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_2y),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_3y),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Operating Expenses (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', xopr_6m_mean, xopr_1y_mean, xopr_2y_mean, xopr_3y_mean),
                           c('Dependant SD', xopr_6m_sd, xopr_1y_sd, xopr_2y_sd, xopr_3y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/expense_specification2.tex',
          type='latex')

# Specification 3 (First Breach)
stargazer(felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_6mfb),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1yfb), 
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_2yfb),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_3yfb),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Operating Expenses (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', xopr_6m_meanfb, xopr_1y_meanfb, xopr_2y_meanfb, xopr_3y_meanfb),
                           c('Dependant SD', xopr_6m_sdfb, xopr_1y_sdfb, xopr_2y_sdfb, xopr_3y_sdfb)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/expense_specification3.tex',
          type='latex')


# Specificaiton 4 (Robustness)
stargazer(felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y),
          felm(xoprq ~ after_m24 + after_m24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_placebo_m24), 
          felm(xoprq ~ after_m36 + after_m36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_placebo_m36), 
          felm(xoprq ~ after_p24 + after_p24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_placebo_p24),
          felm(xoprq ~ after_p36 + after_p36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_placebo_p36),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Operating Expenses (offset of event date)'),
          column.labels = c('(0)', '(-2 Years)', '(-3 Years)', '(+2 Years)', '(+3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', xopr_1y_mean, xopr_1y_mean, xopr_1y_mean, xopr_1y_mean, xopr_1y_mean),
                           c('Dependant SD', xopr_1y_sd, xopr_1y_sd, xopr_1y_sd, xopr_1y_sd, xopr_1y_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/expense_specification4.tex',
          type='latex')

# Specification 5
stargazer(felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y[subset_xopr_1y$nbreaches == 1,]),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y[subset_xopr_1y$nbreaches == 2,]), 
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y[subset_xopr_1y$nbreaches == 3,]),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr_1y[subset_xopr_1y$nbreaches >= 4,]),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Operatiing Expenses (Number of Breaches for Firm)'),
          column.labels = c('(1)', '(2)', '(3)', '(4+)'),
          omit.stat = 'ser',
          add.lines = list(c('Number of Data Breaches ', length(unique(subset_xopr_1y[subset_xopr_1y$nbreaches == 1,]$Date.Made.Public)), length(unique(subset_xopr_1y[subset_xopr_1y$nbreaches == 2,]$Date.Made.Public)),length(unique(subset_xopr_1y[subset_xopr_1y$nbreaches == 3,]$Date.Made.Public)),length(unique(subset_xopr_1y[subset_xopr_1y$nbreaches >= 4,]$Date.Made.Public))),
                           c('Dependant Mean', round(mean(subset_xopr_1y[subset_xopr_1y$nbreaches == 1,]$xoprq),2), round(mean(subset_xopr_1y[subset_xopr_1y$nbreaches == 2,]$xoprq),2), round(mean(subset_xopr_1y[subset_xopr_1y$nbreaches == 3,]$xoprq),2), round(mean(subset_xopr_1y[subset_xopr_1y$nbreaches >= 4,]$xoprq),2)),
                           c('Dependant SD', round(sd(subset_xopr_1y[subset_xopr_1y$nbreaches == 1,]$xoprq),2), round(sd(subset_xopr_1y[subset_xopr_1y$nbreaches == 2,]$xoprq),2), round(sd(subset_xopr_1y[subset_xopr_1y$nbreaches == 3,]$xoprq),2), round(sd(subset_xopr_1y[subset_xopr_1y$nbreaches >= 4,]$xoprq),2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications',
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/expense_specification5.tex',
          type='latex')

######
# Non operating exenses
# Specification 1
stargazer(felm(nopiq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y),
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y), 
          felm(nopiq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y),
          felm(nopiq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y),
          felm(nopiq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Non Operating Expenses'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', round(mean(subset_nop_1y$nopiq, na.rm=T), 2), round(mean(subset_nop_1y$nopiq, na.rm=T), 2), round(mean(subset_nop_1y$nopiq, na.rm=T), 2), round(mean(subset_nop_1y$nopiq, na.rm=T), 2), round(mean(subset_nop_1y$nopiq, na.rm=T), 2)),
                           c('Dependant SD', round(sd(subset_nop_1y$nopiq, na.rm=T), 2), round(sd(subset_nop_1y$nopiq, na.rm=T), 2), round(sd(subset_nop_1y$nopiq, na.rm=T), 2), round(sd(subset_nop_1y$nopiq, na.rm=T), 2), round(sd(subset_nop_1y$nopiq, na.rm=T), 2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/nopexpense_specification1_1y.tex',
          type='latex')

# Specification 2 (Event windows)
stargazer(felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_6m),
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y), 
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_2y),
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_3y),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Non Operating Expenses (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', round(mean(subset_nop_6m$nopiq, na.rm=T), 2), round(mean(subset_nop_1y$nopiq, na.rm=T), 2), round(mean(subset_nop_2y$nopiq, na.rm=T), 2), round(mean(subset_nop_3y$nopiq, na.rm=T), 2)),
                           c('Dependant SD', round(sd(subset_nop_6m$nopiq, na.rm=T), 2), round(sd(subset_nop_1y$nopiq, na.rm=T), 2), round(sd(subset_nop_2y$nopiq, na.rm=T), 2), round(sd(subset_nop_3y$nopiq, na.rm=T), 2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/nopexpense_specification2.tex',
          type='latex')

# Specification 3 (First Breach)
stargazer(felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_6mfb),
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1yfb), 
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_2yfb),
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_3yfb),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Non Operating Expenses (Event Period)'),
          column.labels = c('(6 Months)', '(1 Year)', '(2 Years)', '(3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', round(mean(subset_nop_6mfb$nopiq, na.rm=T), 2), round(mean(subset_nop_1yfb$nopiq, na.rm=T), 2), round(mean(subset_nop_2yfb$nopiq, na.rm=T), 2), round(mean(subset_nop_3yfb$nopiq, na.rm=T), 2)),
                           c('Dependant SD', round(sd(subset_nop_6mfb$nopiq, na.rm=T), 2), round(sd(subset_nop_1yfb$nopiq, na.rm=T), 2), round(sd(subset_nop_2yfb$nopiq, na.rm=T), 2), round(sd(subset_nop_3yfb$nopiq, na.rm=T), 2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications'),
          out = 'tex/nopexpense_specification3.tex',
          type='latex')


# Specificaiton 4 (Robustness)
stargazer(felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y),
          felm(nopiq ~ after_m24 + after_m24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_placebo_m24), 
          felm(nopiq ~ after_m36 + after_m36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_placebo_m36), 
          felm(nopiq ~ after_p24 + after_p24_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_placebo_p24),
          felm(nopiq ~ after_p36 + after_p36_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_placebo_p36),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Non Operating Expenses (offset of event date)'),
          column.labels = c('(0)', '(-2 Years)', '(-3 Years)', '(+2 Years)', '(+3 Years)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', round(mean(subset_nop_1y$nopiq, na.rm=T), 2), round(mean(subset_nop_placebo_m24$nopiq, na.rm=T), 2), round(mean(subset_nop_placebo_m36$nopiq, na.rm=T), 2), round(mean(subset_nop_placebo_p24$nopiq, na.rm=T), 2), round(mean(subset_nop_placebo_p36$nopiq, na.rm=T), 2)),
                           c('Dependant SD', round(sd(subset_nop_1y$nopiq, na.rm=T), 2), round(sd(subset_nop_placebo_m24$nopiq, na.rm=T), 2), round(sd(subset_nop_placebo_m36$nopiq, na.rm=T), 2), round(sd(subset_nop_placebo_p24$nopiq, na.rm=T), 2), round(sd(subset_nop_placebo_p36$nopiq, na.rm=T), 2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/nopexpense_specification4.tex',
          type='latex')

# Specification 5
stargazer(felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y[subset_nop_1y$nbreaches == 1,]),
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y[subset_nop_1y$nbreaches == 2,]), 
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y[subset_nop_1y$nbreaches == 3,]),
          felm(nopiq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_nop_1y[subset_nop_1y$nbreaches >= 4,]),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Non Operatiing Expenses (Number of Breaches for Firm)'),
          column.labels = c('(1)', '(2)', '(3)', '(4+)'),
          omit.stat = 'ser',
          add.lines = list(c('Number of Data Breaches ', length(unique(subset_nop_1y[subset_nop_1y$nbreaches == 1,]$Date.Made.Public)), length(unique(subset_nop_1y[subset_nop_1y$nbreaches == 2,]$Date.Made.Public)),length(unique(subset_nop_1y[subset_nop_1y$nbreaches == 3,]$Date.Made.Public)),length(unique(subset_nop_1y[subset_nop_1y$nbreaches >= 4,]$Date.Made.Public))),
                           c('Dependant Mean', round(mean(subset_nop_1y[subset_nop_1y$nbreaches == 1,]$nopiq),2), round(mean(subset_nop_1y[subset_nop_1y$nbreaches == 2,]$nopiq),2), round(mean(subset_nop_1y[subset_nop_1y$nbreaches == 3,]$nopiq),2), round(mean(subset_nop_1y[subset_nop_1y$nbreaches >= 4,]$nopiq),2)),
                           c('Dependant SD', round(sd(subset_nop_1y[subset_nop_1y$nbreaches == 1,]$nopiq),2), round(sd(subset_nop_1y[subset_nop_1y$nbreaches == 2,]$nopiq),2), round(sd(subset_nop_1y[subset_nop_1y$nbreaches == 3,]$nopiq),2), round(sd(subset_nop_1y[subset_nop_1y$nbreaches >= 4,]$nopiq),2))),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications',
                    'Prediction period is up to 10 years before breach, and event period up to 1 year after'),
          out = 'tex/nopexpense_specification5.tex',
          type='latex')

######
# Other controls:
stargazer(felm(xsgaq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test_1y),
          felm(ceqq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test_1y),
          felm(epspiq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test_1y),
          felm(emp ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test_1y),
          felm(trend_index_company ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test_1y),
          felm(trend_index_tic ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test_1y),
          dep.var.labels = c('Sales, General and Other Expenses', 'Total Shareholders\' Equity', 'Earnings per Share {Basic)',
                             'Number of Employees', 'Google Searches (Company Name)', 'Google Searches (Stock Ticker)'),
          covariate.labels = c('After Breach', 'After Breach x Quarters Since Breach'),
          add.lines = list(c('Dependant Mean', mean_xsgaq_1y, mean_ceqq_1y, round(mean(subset_test_1y$epspiq, na.rm=T), 2), mean_emp_1y, mean_trend_index_company_1y, mean_trend_index_tic_1y),
                           c('Dependant SD', sd_xsgaq_1y, sd_ceqq_1y, round(sd(subset_test_1y$epspiq, na.rm=T), 2), sd_emp_1y, sd_trend_index_company_1y, sd_trend_index_tic_1y)),
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          omit.stat = 'ser',
          notes = c('Standard Errors clustered at the quarter level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period 1 year after'),
          out='tex/other_controls.tex',
          type='latex')

######
# Stock Market
stargazer(felm(car_mmodel_percent_5d ~ days_since_begin | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_5d ~ Total.Records_log | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_5d ~ nbreaches | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_5d ~ customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          dep.var.labels = c('CAPM CAR'),
          covariate.labels = c('Days Since beginning of sample', 'Records Leaked (log)', 'Number of breaches', 'Customer Data Leaked', 'Employee Data Leaked', 'Credit Card Leaked', 'SSN Leaked', 'Name Leaked', 'Address Leaked'),
          omit.stat = 'ser',
          notes = c('Year fixed effects in all specifications', 'CAR from 5 day event period'),
          #out='tex/stock_market_specification1.tex',
          type='text')


stargazer(felm(car_ffmodel_percent_5d ~ days_since_begin | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_5d ~ Total.Records_log | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_5d ~ nbreaches | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_5d ~ customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          dep.var.labels = c('Fam-French CAR'),
          covariate.labels = c('Days Since beginning of sample', 'Records Leaked (log)', 'Number of breaches', 'Customer Data Leaked', 'Employee Data Leaked', 'Credit Card Leaked', 'SSN Leaked', 'Name Leaked', 'Address Leaked'),
          omit.stat = 'ser',
          notes = c('Year fixed effects in all specifications', 'CAR from 5 day event period'),
          #out='tex/stock_market_specification1.tex',
          type='text')

######
# Plotting
# Absolute Revenue
ggplot(subset_rev_3y, aes(x = quarters_since_breach, y = resid_rev_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-50, 50)) +
  #scale_x_continuous(limits = c(-12,12)) +
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_rev_3y, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_rev_3y, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  #ggtitle('Mean Residual Revenue (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, family='serif'),
        axis.title=element_text(size=12, family='serif'))
ggsave('tex/mean_resid_revenue_3y.png')

# Log Revenue
ggplot(subset_rev_3y, aes(x = quarters_since_breach, y = resid_logrev_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-0.2, 0.2)) + 
  #scale_x_continuous(limits = c(-5,5)) +
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_rev_3y, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_rev_3y, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  ylab('') +
  xlab('Quarters Since Breach') +
  #ggtitle('Mean Residual Revenue (Log) (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, family='serif'),
        axis.title=element_text(size=12, family='serif'))
ggsave('tex/mean_resid_logrevenue_3y.png')

# Profit
ggplot(subset_pr_3y, aes(x = quarters_since_breach, y = resid_pr_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=100, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-10, 10)) + 
  #scale_x_continuous(limits = c(-5,5)) +
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_pr_3y, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_pr_3y, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  #ggtitle('Mean Residual Profit (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, family='serif'),
        axis.title=element_text(size=12, family='serif'))
ggsave('tex/mean_resid_profit_3y.png')

# Operating Expenses
ggplot(subset_xopr_3y, aes(x = quarters_since_breach, y = resid_xoprq_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=100, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-40, 40)) +
  scale_x_continuous(limits = c(-5,5)) +
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_xopr_3y, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_xopr_3y, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  #ggtitle('Mean Residual Operating Expenses (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, family='serif'),
        axis.title=element_text(size=12, family='serif'))
ggsave('tex/mean_resid_xoprq_3y.png')

# Non Operating Expenses
ggplot(subset_nop_3y, aes(x = quarters_since_breach, y = resid_nopiq_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=100, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-40, 40)) + 
  #scale_x_continuous(limits = c(-5,5)) +
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_nop_3y, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_nop_3y, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  #ggtitle('Mean Residual Operating Expenses (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, family='serif'),
        axis.title=element_text(size=12, family='serif'))
ggsave('tex/mean_resid_nopiq_3y.png')

subset_rev_3y$resid_rev_nott <- residuals(felm(revtq ~ 0 | factor(gvkey) + factor(quarters_since_begin) | 0 | 0, data = subset_rev_3y))
subset_pr_3y$resid_pr_nott <- residuals(felm(niq ~ 0 | factor(gvkey) + factor(quarters_since_begin) | 0 | 0, data = subset_pr_3y))
subset_xopr_3y$resid_xoprq_nott <- residuals(felm(xoprq ~ 0 | factor(gvkey) + factor(quarters_since_begin) | 0 | 0, data = subset_xopr_3y))
subset_nop_3y$resid_nopiq_nott <- residuals(felm(nopiq ~ 0 | factor(gvkey) + factor(quarters_since_begin) | 0 | 0, data = subset_nop_3y))

# All in one
ggplot() + 
  geom_point(aes(x = quarters_since_breach, y = resid_rev_nott), data=subset_rev_3y, alpha=0, color='grey') + 
  geom_point(aes(x = quarters_since_breach, y = resid_pr_nott), data=subset_pr_3y, alpha=0, color='grey') + 
  geom_point(aes(x = quarters_since_breach, y = resid_xoprq_nott), data=subset_xopr_3y, alpha=0, color='grey') + 
  geom_point(aes(x = quarters_since_breach, y = resid_nopiq_nott), data=subset_nop_3y, alpha=0, color='grey') + 
  stat_summary_bin(aes(x = quarters_since_breach, y = resid_rev_nott), data=subset_rev_3y, fun.y='mean', color='black', geom='line', alpha=1, size=1) + 
  stat_summary_bin(aes(x = quarters_since_breach, y = resid_pr_nott), data=subset_pr_3y, fun.y='mean', color='green', geom='line', alpha=1, size=1) + 
  stat_summary_bin(aes(x = quarters_since_breach, y = resid_xoprq_nott), data=subset_xopr_3y, fun.y='mean', color='red', geom='line', alpha=1, size=1) + 
  stat_summary_bin(aes(x = quarters_since_breach, y = resid_nopiq_nott), data=subset_nop_3y, fun.y='mean', color='yellow', geom='line', alpha=1, size=1) + 
  scale_y_continuous(limits = c(-10, 10)) +
  scale_x_continuous(limits = c(-12,12)) +
  geom_vline(xintercept = 0, color = 'black', size = 2, alpha = 0.5) + 
  geom_hline(yintercept = 0, color = 'black', size = 2, alpha = 0.5) +
  xlab('Quarters Since Breach') +
  ylab('') +
  guides(fill=T) +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, family='serif'),
        axis.title=element_text(size=12, family='serif'))

