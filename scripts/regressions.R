setwd('/mnt/good/honours_thesis/')
load('.RData')

library(stargazer)
library(ggplot2)
library(lfe)
library(zoo)
library(lubridate)

for (i in 1:nrow(comp)) {
  # print(paste('Rows remaining: ', nrow(comp)-i))
  gvkey = comp[i,'gvkey']
  datadate = comp[i, 'datadate']
  if (comp[i,'treat'] == 1) {
    company_slice = comp[comp$gvkey == gvkey,]
    breach_dates = company_slice$breachdate
    breach_dates = na.omit(breach_dates)
    idx = which.min(abs(datadate-breach_dates))
    last_breach = breach_dates[idx]
    comp[i,'breachdate'] = last_breach
    comp[i, 'time_since_breach'] = difftime(comp[i, 'quarter_as_date'], last_breach, units="days")
    
    # print(last_breach)
    # print(company_slice)
    # print(company_slice[company_slice$breachdate == last_breach,])
    comp[i, 'customer'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$customer)[1]
    comp[i, 'employee'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$employee)[1]
    comp[i, 'credit_card'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$credit_card)[1]
    comp[i, 'cvv'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$cvv)[1]
    comp[i, 'social_security'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$social_security)[1]
    comp[i, 'name'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$name)[1]
    comp[i, 'address'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$address)[1]
    comp[i, 'personal_information'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$personal_information)[1]
    comp[i, 'Total.Records'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$Total.Records)[1]
  } else if (comp[i,'treat'] == 0){
    comp[i,'breachdate'] = '2018-01-01'  
    
    comp[i, 'customer'] = 0
    comp[i, 'employee'] = 0
    comp[i, 'credit_card'] = 0
    comp[i, 'cvv'] = 0
    comp[i, 'social_security'] = 0
    comp[i, 'name'] = 0
    comp[i, 'address'] = 0
    comp[i, 'personal_information'] = 0
    comp[i, 'Total.Records'] = 0
  }
}

######
# Variable Creation

comp = read.csv('data/COMPUSTAT_merged_cleaned_trends_feb.csv')
comp <- comp[comp$curncdq == 'USD',]
comp$datadate = as.Date(comp$datadate)
comp$year <- format(as.Date(comp$datadate), "%Y")
comp$breachdate = as.Date(comp$Date.Made.Public, format="%Y-%m-%d", optional=T)
comp_match = subset(comp, (!is.na(comp$match)))
comp$treat <- ifelse(comp$gvkey %in% comp_match$gvkey,1,0)
comp$quarters_since_begin <- (as.yearqtr(comp$datafqtr, format('%YQ%q')) - as.yearqtr('2005 Q1'))*4
comp$quarter_as_date <- as.Date(as.yearqtr(comp$datafqtr, format('%YQ%q')))
comp$year <- format(comp$quarter_as_date, "%Y")

# Exclude yahoo breach
yahoo_gvkey <- comp[which.max(comp$Total.Records),]$gvkey
comp <- comp[comp$gvkey != yahoo_gvkey,]

comp$after <- ifelse(comp$datadate > comp$breachdate,1,0)
comp$after_m1 <- ifelse(comp$datadate %m+% months(-1) > comp$breachdate,1,0)
comp$after_m2 <- ifelse(comp$datadate %m+% months(-2) > comp$breachdate,1,0)
comp$after_m3 <- ifelse(comp$datadate %m+% months(-3) > comp$breachdate,1,0)
comp$after_m4 <- ifelse(comp$datadate %m+% months(-4) > comp$breachdate,1,0)
comp$after_m5 <- ifelse(comp$datadate %m+% months(-5) > comp$breachdate,1,0)
comp$after_m6 <- ifelse(comp$datadate %m+% months(-6) > comp$breachdate,1,0)
comp$after_p1 <- ifelse(comp$datadate %m+% months(1) > comp$breachdate,1,0)
comp$after_p2 <- ifelse(comp$datadate %m+% months(2) > comp$breachdate,1,0)
comp$after_p3 <- ifelse(comp$datadate %m+% months(3) > comp$breachdate,1,0)
comp$after_p4 <- ifelse(comp$datadate %m+% months(4) > comp$breachdate,1,0)
comp$after_p5 <- ifelse(comp$datadate %m+% months(5) > comp$breachdate,1,0)
comp$after_p6 <- ifelse(comp$datadate %m+% months(6) > comp$breachdate,1,0)

comp$time_since_begin <- comp$datadate - as.Date("2005-01-01")
comp$breachquarter <- as.yearqtr(comp$breachdate)
comp$quarters_since_breach <- (as.yearqtr(comp$datafqtr, format('%YQ%q')) - comp$breachquarter)*4

comp$customer_interact <- comp$customer * comp$after
comp$employee_interact <- comp$employee * comp$after
comp$customer_interact <- comp$customer * comp$after
comp$credit_card_interact <- comp$credit_card * comp$after
comp$cvv_interact <- comp$cvv * comp$after
comp$social_security_interact <- comp$social_security * comp$after
comp$name_interact <- comp$name * comp$after
comp$address_interact <- comp$address * comp$after
comp$personal_information_interact <- comp$personal_information * comp$ after
comp$Total.Records_interact <- comp$Total.Records * comp$after

comp$Total.Records_tolog <- comp$Total.Records
comp[!is.na(comp$Total.Records) & comp$Total.Records == 0,]$Total.Records_tolog <- 0.0001
comp$Total.Records_log <- log(comp$Total.Records_tolog)
comp$Total.Records_interact_log <- comp$Total.Records_log * comp$after
comp$trend_index_company_interact <- comp$trend_index_company * comp$after
comp$trend_index_tic_interact <- comp$trend_index_tic * comp$after

comp$xsgaq_tolog <- comp$xsgaq
comp[!is.na(comp$xsgaq) & comp$xsgaq == 0,]$xsgaq_tolog <- 0.0001
comp$xsgaq_log <- log(comp$xsgaq_tolog)
comp$xsgaq_interact <- comp$xsgaq * comp$after
comp$xsgaq_interact_log <- comp$xsgaq_log * comp$after

comp$revtq_tolog <- comp$revtq
comp[!is.na(comp$revtq) & comp$revtq == 0,]$revtq_tolog <- 0.0001
comp[!is.na(comp$revtq) & comp$revtq < 0,]$revtq_tolog <- NA
comp[!is.na(comp$revtq) & comp$revtq < 0,]$revtq <- NA
comp$log_revtq <- log(comp$revtq_tolog)

comp$after_quarter_interact_b <- comp$after * comp$quarters_since_begin
comp$after_quarter_interact_br <- comp$after * comp$quarters_since_breach
comp$rh1 <- ifelse(comp$rev_quart_1 == 1 | comp$rev_quart_2, 1, 0)
comp$rq1_interact <- comp$rev_quart_1*comp$after
comp$rq2_interact <- comp$rev_quart_2*comp$after
comp$rq3_interact <- comp$rev_quart_3*comp$after
comp$rq4_interact <- comp$rev_quart_4*comp$after
comp$rh1_interact <- comp$rh1 * comp$after

resid_logrev_tt <- lm(log_revtq ~ factor(gvkey) + factor(quarters_since_begin) + factor(gvkey)*time_since_begin, data = comp, na.action=na.exclude)
resid_rev_tt <- lm(revtq ~ factor(gvkey) + factor(quarters_since_begin) + factor(gvkey)*time_since_begin, data = comp, na.action=na.exclude)
resid_logrev_nott <- lm(log_revtq ~ factor(gvkey) + factor(quarters_since_begin), data = comp, na.action=na.exclude)
resid_rev_nott <- lm(revtq ~ factor(gvkey) + factor(quarters_since_begin), data = comp, na.action=na.exclude)
resid_pr_tt <- lm(niq ~ factor(gvkey) + factor(quarters_since_begin) + factor(gvkey)*time_since_begin, data = comp, na.action=na.exclude)
resid_pr_nott <- lm(niq ~ factor(gvkey) + factor(quarters_since_begin), data = comp, na.action=na.exclude)
resid_xoprq_tt <- lm(xoprq ~ factor(gvkey) + factor(quarters_since_begin) + factor(gvkey)*time_since_begin, data = comp, na.action=na.exclude)
resid_xoprq_nott <- lm(xoprq ~ factor(gvkey) + factor(quarters_since_begin), data = comp, na.action=na.exclude)
resid_xsgaq_tt <- lm(xsgaq ~ factor(gvkey) + factor(quarters_since_begin) + factor(gvkey)*time_since_begin, data = comp, na.action=na.exclude)
resid_xsgaq_nott <- lm(xsgaq ~ factor(gvkey) + factor(quarters_since_begin), data = comp, na.action=na.exclude)
resid_quarters_since_breach <- lm(quarters_since_breach ~ factor(gvkey) + factor(quarters_since_begin), data = comp, na.action=na.exclude)

comp$resid_logrev_tt <- residuals(resid_logrev_tt)
comp$resid_rev_tt <- residuals(resid_rev_tt)
comp$resid_logrev_nott <- residuals(resid_logrev_nott)
comp$resid_rev_nott <- residuals(resid_rev_nott)
comp$resid_pr_tt <- residuals(resid_pr_tt)
comp$resid_pr_nott <- residuals(resid_pr_nott)
comp$resid_xoprq_tt <- residuals(resid_xoprq_tt)
comp$resid_xoprq_nott <- residuals(resid_xoprq_nott)
comp$resid_xsgaq_tt <- residuals(resid_xsgaq_tt)
comp$resid_xsgaq_nott <- residuals(resid_xsgaq_nott)
comp$resid_quarters_since_begin <- reisduals(resid_quarters_since_breach)

comp_full25 <- merge(unique(comp[comp$quarters_since_breach <= -25,][c('gvkey', 'Date.Made.Public')]), unique(comp[comp$quarters_since_breach >= 25,][c('gvkey', 'Date.Made.Public')]))
comp_full25 <- comp[comp$gvkey %in% comp_full25$gvkey & comp$Date.Made.Public %in% comp_full25$Date.Made.Public & comp$quarters_since_breach %in% -25:25,]
comp_full20 <- merge(unique(comp[comp$quarters_since_breach <= -20,][c('gvkey', 'Date.Made.Public')]), unique(comp[comp$quarters_since_breach >= 20,][c('gvkey', 'Date.Made.Public')]))
comp_full20 <- comp[comp$gvkey %in% comp_full20$gvkey & comp$Date.Made.Public %in% comp_full20$Date.Made.Public & comp$quarters_since_breach %in% -20:20,]
comp_full15 <- merge(unique(comp[comp$quarters_since_breach <= -15,][c('gvkey', 'Date.Made.Public')]), unique(comp[comp$quarters_since_breach >= 15,][c('gvkey', 'Date.Made.Public')]))
comp_full15 <- comp[comp$gvkey %in% comp_full15$gvkey & comp$Date.Made.Public %in% comp_full15$Date.Made.Public & comp$quarters_since_breach %in% -15:15,]
comp_full10 <- merge(unique(comp[comp$quarters_since_breach <= -10,][c('gvkey', 'Date.Made.Public')]), unique(comp[comp$quarters_since_breach >= 10,][c('gvkey', 'Date.Made.Public')]))
comp_full10 <- comp[comp$gvkey %in% comp_full10$gvkey & comp$Date.Made.Public %in% comp_full10$Date.Made.Public & comp$quarters_since_breach %in% -10:10,]
comp_full5 <- merge(unique(comp[comp$quarters_since_breach <= -5,][c('gvkey', 'Date.Made.Public')]), unique(comp[comp$quarters_since_breach >= 5,][c('gvkey', 'Date.Made.Public')]))
comp_full5 <- comp[comp$gvkey %in% comp_full5$gvkey & comp$Date.Made.Public %in% comp_full5$Date.Made.Public & comp$quarters_since_breach %in% -5:5,]

write.csv(comp, 'data/COMPUSTAT_vars.csv')
write.csv(comp_full20, 'data/COMPUSTAT_full20.csv')
write.csv(comp_full15, 'data/COMPUSTAT_full15.csv')
write.csv(comp_full10, 'data/COMPUSTAT_full10.csv')
write.csv(comp_full5, 'data/COMPUSTAT_full5.csv')

# invshift <- nrow(comp) - 1
# comp$profit_proportion <- c(1, (head(comp, invshift)$niq - tail(comp, invshift)$niq)/tail(comp, invshift)$niq)

######
# Revenue OLS
subset <- comp[!is.na(comp$time_since_breach) & !is.na(comp$log_revtq) & !is.infinite(comp$log_revtq),]
ols_lm_log_tt <- felm(log_revtq ~ factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c1 <- felm(log_revtq ~ xsgay_log + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c2 <- felm(log_revtq ~ xsgay_log + Total.Records_log + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c3 <- felm(log_revtq ~ xsgay_log + Total.Records_log + trend_index_company + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c4 <- felm(log_revtq ~ xsgay_log + Total.Records_log + trend_index_company + customer + employee + credit_card + cvv + social_security + name + address + personal_information + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)

ols_lm_abs_tt <- felm(revtq ~ factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c1 <- felm(revtq ~ xsgay_log + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c2 <- felm(revtq ~ xsgay_log + Total.Records_log + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c3 <- felm(revtq ~ xsgay_log + Total.Records_log + trend_index_company + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c4 <- felm(revtq ~ xsgay_log + Total.Records_log + trend_index_company + customer + employee + credit_card + cvv + social_security + name + address + personal_information + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset)

# log_tt
stargazer(ols_lm_log_tt, ols_lm_log_tt_c1, ols_lm_log_tt_c2, ols_lm_log_tt_c3, ols_lm_log_tt_c4,
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'Yes', 'Yes', 'Yes', 'Yes')),
          type='text')
# abs_tt
stargazer(ols_lm_abs_tt, ols_lm_abs_tt_c1, ols_lm_abs_tt_c2, ols_lm_abs_tt_c3, ols_lm_abs_tt_c4,
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'Yes', 'Yes', 'Yes', 'Yes')),
          type='text')

######
# Revenue graphing
subset <- comp[!is.na(comp$quarters_since_breach) & 
               !is.na(comp$log_revtq) & 
               !is.infinite(comp$log_revtq) &
               comp$quarters_since_breach %in% -20:4,]

temp = cbind(aggregate(subset, list(q = subset$quarters_since_breach), mean, na.rm=T)[c('revtq', 'log_revtq', 'resid_rev_tt', 'resid_logrev_tt', 'resid_rev_nott', 'resid_logrev_nott')], sort(unique(subset$quarters_since_breach)))
names(temp) <- c('mean_revtq', 'mean_log_revtq','mean_resid_log_tt', 'mean_resid_abs_tt', 'mean_resid_log_nott', 'mean_resid_abs_nott', 'quarters_since_breach')
# subset <- merge(subset, temp, by='quarters_since_breach', all.x=T)

# TT with all data
ggplot(subset, aes(x = quarters_since_breach, y = resid_logrev_tt)) + geom_point() +
  scale_y_continuous(limits=c(-0.2, 0.2)) +
  stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm") +
  ylab('Residual Log Revenue (with time trends)') +
  xlab('Quarters Since Breach')
ggplot(subset, aes(x = quarters_since_breach, y = resid_rev_tt)) + geom_point() +
  scale_y_continuous(limits=c(-1500, 1000)) +
  stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm") +
  ylab('Residual Revenue (with time trends)') +
  xlab('Quarters Since Breach')

# NOTT with all data
ggplot(subset, aes(x = quarters_since_breach, y = resid_rev_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-50, 50)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  ylab('') +
  xlab('Quarters Since Breach') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) + 
  ggsave('tables/mean_resid_revenue.png')
ggplot(subset, aes(x = quarters_since_breach, y = resid_logrev_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point') + 
  scale_y_continuous(limits = c(-0.1, 0.1)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  ggtitle('Residual Revenue (Fixed Effects Removed)') + 
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif'))

# TT only means
ggplot(temp, aes(x = quarters_since_breach, y = mean_resid_log_tt)) + geom_point() +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp, quarters_since_breach >= 0), method = "lm")
ggplot(temp, aes(x = quarters_since_breach, y = mean_resid_abs_tt)) + geom_point() +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp, quarters_since_breach >= 0), method = "lm")
ggplot(temp, aes(x = quarters_since_breach, y = mean_revtq)) + geom_point() +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp, quarters_since_breach >= 0), method = "lm")

# NOTT only means
ggplot(temp, aes(x = quarters_since_breach, y = mean_resid_log_nott)) + geom_point() +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp, quarters_since_breach >= 0), method = "lm")
ggplot(temp, aes(x = quarters_since_breach, y = mean_resid_abs_nott)) + geom_point() +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp, quarters_since_breach <= 0), method = "lm", se=F) +
  geom_smooth(data=subset(temp, quarters_since_breach >= 0), method = "lm", se=F) +
  xlab('Quarters Since Breach') + 
  ylab('Mean Residual Revenue') + 
  ggtitle('Residual Revenue (Fixed Effects Removed)') + 
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) +
  ggsave('tables/mean_resid_rev.png')
ggplot(temp, aes(x = quarters_since_breach, y = mean_log_revtq)) + geom_point() +
  geom_vline(subset, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp, quarters_since_breach >= 0), method = "lm")

ggplot(subset) + geom_point(aes(x=quarters_since_breach, y=resid_abs_nott))

# Plot by quartile
ggplot(subset[subset$rev_quart_4 == 1,], aes(x = quarters_since_breach, y = resid_log_tt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_x_continuous(limits = c(-25, 25)) + scale_y_continuous(limits = c(0, 2)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_log), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm") + ylab('Residual Revenue (No time trends)') + xlab('Quarters Since Breach')
ggplot(subset[subset$rev_quart_4 == 1,], aes(x = quarters_since_breach, y = resid_abs_tt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_x_continuous(limits = c(-25, 25)) + scale_y_continuous(limits = c(0, 5000)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_log), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm") + ylab('Residual Revenue (No time trends)') + xlab('Quarters Since Breach')

# Constant Sample
subset_rev_const-sample <- comp[comp$gvkey %in% comp_full10,]
subset_rev_const-sample <- subset_rev_const-sample[subset_rev_const-sample$quarters_since_breach %in% -10:10,]
temp = cbind(aggregate(subset_rev_const-sample, list(q = subset_rev_const-sample$quarters_since_breach), mean, na.rm=T)['revtq'], -10:10)
names(temp) <- c('mean_revtq', 'quarters_since_breach')
subset_rev_const-sample <- merge(subset_rev_const-sample, temp, by='quarters_since_breach', all.x=T)
ggplot(subset_rev_const-sample) + geom_point(aes(x = quarters_since_breach, y = revtq), color='grey', alpha=0.8) + 
  scale_y_continuous(limits = c(0, 100000)) +
  geom_point(aes(x = quarters_since_breach, y = mean_revtq), color='blue', size=2) +
  geom_vline(subset_rev_const-sample, mapping = aes(x = quarters_since_breach, y = resid), xintercept = 0, color = "orange", size = 0.5, alpha=0.6) + 
  geom_smooth(data=subset(subset_rev_const-sample, quarters_since_breach <= 0), mapping = aes(x = quarters_since_breach, y = revtq),  method = "lm") + 
  geom_smooth(data=subset(subset_rev_const-sample, quarters_since_breach >= 0), mapping = aes(x = quarters_since_breach, y = revtq),  method = "lm")




######
# Revenue Regressions
# Original specification regression:
subset <- comp[!is.na(comp$quarters_since_breach) &
               !is.na(comp$log_revtq) &
               !is.infinite(comp$log_revtq) &
               comp$quarters_since_breach %in% -40:4,]

orig_lm_log_tt_ni <- felm(log_revtq ~ after | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_log_tt <- felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_log_tt_c1 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_log_tt_c2 <- felm(log_revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_log_tt_c3 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)

orig_lm_abs_tt_ni <- felm(revtq ~ after | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_abs_tt <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_abs_tt_c1 <- felm(revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_abs_tt_c2 <- felm(revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company_interact +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
orig_lm_abs_tt_c3 <- felm(revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)

orig_lm_log_nott_ni <- felm(log_revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_log_nott <- felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_log_nott_c1 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_log_nott_c2 <- felm(log_revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_log_nott_c3 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)

orig_lm_abs_nott_ni <- felm(revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_abs_nott <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_abs_nott_c1 <- felm(revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_abs_nott_c1_ni <- felm(revtq ~ after + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_abs_nott_c2 <- felm(revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company_interact +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
orig_lm_abs_nott_c3 <- felm(revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)

quart_lm_abs_tt <- felm(revtq ~ after+ rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin| 0 | gvkey, data = subset)
quart_lm_abs_tt_c1 <- felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset)
quart_lm_abs_nott <- felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)
quart_lm_abs_nott_c1 <- felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset)


abs_mean = round(mean(subset$revtq),2)
abs_sd = round(sd(subset$revtq),2)
log_mean = round(mean(subset$log_revtq),2)
log_sd = round(sd(subset$log_revtq),2)
# Variant 3 (ABS):
stargazer(orig_lm_abs_nott_ni, orig_lm_abs_nott, orig_lm_abs_nott_c2,quart_lm_abs_nott, quart_lm_abs_nott_c1,
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Revenue'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', abs_mean, abs_mean, abs_mean, abs_mean, abs_mean),
                           c('Dependant SD', abs_sd, abs_sd, abs_sd, abs_sd, abs_sd)),
          notes = c('Standard errors clustered at the company level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years before breach, and event period up to 10 years after'),
          out = 'tables/revenue_specification3_3y.html',
          type='html')

# Variant 3 (LOG):
stargazer(orig_lm_log_nott_ni, orig_lm_log_nott, orig_lm_log_nott_c2,
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'Customer Data Leaked x After breach', 'Employee Data Leaked x After breach', 'Credit Card Leaked x After breach', 'CVV Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach', 'PI Leaked x After breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3'),
          dep.var.labels = c('Revenue (log)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', log_mean, log_mean, log_mean, log_mean, log_mean),
                           c('Dependant SD', log_sd, log_sd, log_sd, log_sd, log_sd)),
          notes = c('Standard Errors clustered at the company level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years months before breach, and event period up to 10 years after'),
          out = 'tables/logrevenue_specification3_3y.html',
          type='html')

ggplot(subset, aes(x = quarters_since_breach, y = resid_rev_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-50, 50)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  ggtitle('Mean Residual Revenue (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) #+ 
  #ggsave('tables/mean_resid_revenue_3y.png')

ggplot(subset, aes(x = quarters_since_breach, y = resid_logrev_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-0.2, 0.2)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  ylab('') +
  xlab('Quarters Since Breach') +
  ggtitle('Mean Residual Revenue (Log) (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) + 
  ggsave('tables/mean__resid_logrevenue_3y.png')


subset <- comp[!is.na(comp$quarters_since_breach) &
                 !is.na(comp$log_revtq) &
                 !is.infinite(comp$log_revtq) &
                 comp$quarters_since_breach %in% -40:4,]
abs_mean = round(mean(subset$revtq),2)
abs_sd = round(sd(subset$revtq),2)
log_mean = round(mean(subset$log_revtq),2)
log_sd = round(sd(subset$log_revtq),2)
# Variant 5 (ABS):
stargazer(felm(revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          #felm(revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          felm(revtq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          #felm(revtq ~ after + after_quarter_interact + after*first_rev + after_quarter_interact*first_rev | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          #felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          #omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          #covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Revenue'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', abs_mean, abs_mean, abs_mean, abs_mean, abs_mean),
                           c('Dependant SD', abs_sd, abs_sd, abs_sd, abs_sd, abs_sd)),
          notes = c('Standard errors clustered at the company level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years before breach, and event period up to 10 years after'),
          type='text')

# Variant 5 (LOG):
stargazer(felm(log_revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          felm(log_revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company_interact +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'Customer Data Leaked x After breach', 'Employee Data Leaked x After breach', 'Credit Card Leaked x After breach', 'CVV Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach', 'PI Leaked x After breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3'),
          dep.var.labels = c('Revenue (log)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', log_mean, log_mean, log_mean, log_mean, log_mean),
                           c('Dependant SD', log_sd, log_sd, log_sd, log_sd, log_sd)),
          notes = c('Standard Errors clustered at the company level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years months before breach, and event period up to 10 years after'),
          type='text')


#####
# Constant Samples:
subset_full5 <- comp_full5[!is.na(comp_full5$quarters_since_breach) &
                           !is.na(comp_full5$log_revtq) &
                           !is.infinite(comp_full5$log_revtq),]
subset_full10 <- comp_full10[!is.na(comp_full10$quarters_since_breach) &
                             !is.na(comp_full10$log_revtq) &
                             !is.infinite(comp_full10$log_revtq),]
subset_full15 <- comp_full15[!is.na(comp_full15$quarters_since_breach) &
                             !is.na(comp_full15$log_revtq) &
                             !is.infinite(comp_full15$log_revtq),]
subset_full20 <- comp_full20[!is.na(comp_full20$quarters_since_breach) &
                             !is.na(comp_full20$log_revtq) &
                             !is.infinite(comp_full20$log_revtq),]

orig_lm_log_nott_f5 <- felm(log_revtq ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr), data = subset_full5)
orig_lm_abs_nott_f5 <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | datafqtr, data = subset_full5)
orig_lm_log_nott_f10 <- felm(log_revtq ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr), data = subset_full10)
orig_lm_abs_nott_f10 <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | datafqtr, data = subset_full10)
orig_lm_log_nott_f15 <- felm(log_revtq ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr), data = subset_full15)
orig_lm_abs_nott_f15 <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | datafqtr, data = subset_full15)
orig_lm_log_nott_f20 <- felm(log_revtq ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr), data = subset_full20)
orig_lm_abs_nott_f20 <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | datafqtr, data = subset_full20)

# Log
stargazer(orig_lm_log_nott, orig_lm_log_nott_f5, orig_lm_log_nott_f10, orig_lm_log_nott_f15, orig_lm_log_nott_f20, 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant', 'rev_quart_1', 'rev_quart_2', 'rev_quart_3'),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Revenue (log)'),
          notes = c('Standard Errors clustered at the company level', 'Company and quarter fixed effects in all specifications'),
          add.lines = list(c('Sample:', 'Full', 'Full 5', 'Full 10', 'Full 15', 'Full 20')),
          omit.stat = 'ser',
          type='text')

# ABS
stargazer(orig_lm_abs_nott, orig_lm_abs_nott_f5, orig_lm_abs_nott_f10, orig_lm_abs_nott_f15, orig_lm_abs_nott_f20, 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant', 'rev_quart_1', 'rev_quart_2', 'rev_quart_3'),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Revenue'),
          notes = c('Standard Errors clustered at the company level', 'Company and quarter fixed effects in all specifications'),
          add.lines = list(c('Sample:', 'Full', 'Full 5', 'Full 10', 'Full 15', 'Full 20')),
          omit.stat = 'ser',
          type='text')

# playzone 
stargazer(felm(revtq ~ after + rev_quart_1*after + rev_quart_2*after + rev_quart_3*after | factor(gvkey) + datafqtr| 0 | datafqtr, data = subset), 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'Yes')),
          type='text')


######
# Full sample graphing
subset <- comp[!is.na(comp$quarters_since_breach) & !is.na(comp$log_revtq) & !is.infinite(comp$log_revtq),]

resid_lm_log_tt <- felm(log_revtq ~ factor(gvkey) + factor(datafqtr) + factor(gvkey)*time_since_begin, data = subset)
resid_lm_abs_tt <- felm(revtq ~ factor(gvkey) + factor(datafqtr) + factor(gvkey)*time_since_begin, data = subset)
resid_lm_log_nott <- felm(log_revtq ~ factor(gvkey) + factor(datafqtr), data = subset)
resid_lm_abs_nott <- felm(revtq ~ factor(gvkey) + factor(datafqtr), data = subset)

subset$resid_log_tt <- residuals(resid_lm_log_tt)
subset$resid_abs_tt <- residuals(resid_lm_abs_tt)
subset$resid_log_nott <- residuals(resid_lm_log_nott)
subset$resid_abs_nott <- residuals(resid_lm_abs_nott)

temp = cbind(aggregate(subset, list(q = subset$quarters_since_breach), mean, na.rm=T)[c('revtq', 'resid_log_tt', 'resid_abs_tt', 'resid_log_nott', 'resid_abs_nott')], sort(unique(subset$quarters_since_breach)))
names(temp) <- c('mean_revtq', 'mean_resid_log_tt', 'mean_resid_abs_tt', 'mean_resid_log_nott', 'mean_resid_abs_nott', 'quarters_since_breach')
subset <- merge(subset, temp, by='quarters_since_breach', all.x=T)

# All data
ggplot(subset, aes(x = quarters_since_breach, y = resid_log_nott)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 2)) + geom_vline(subset, xintercept = 0, color = "grey", alpha = "0.5", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm") + ylab('Residual Log Revenue (No time trends)') + xlab('Quarters Since Breach')
ggplot(subset, aes(x = quarters_since_breach, y = resid_abs_nott)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 5000)) + geom_vline(subset, xintercept = 0, color = "grey", alpha = "0.5", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm") + ylab('Residual Revenue (No time trends)') + xlab('Quarters Since Breach')

# Means only
ggplot(temp, aes(x = quarters_since_breach, y = mean_resid_log_nott)) + geom_point() + geom_vline(subset, xintercept = 0, color = "grey", alpha = "0.5", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm")
ggplot(temp, aes(x = quarters_since_breach, y = mean_resid_abs_nott)) + geom_point() + geom_vline(subset, xintercept = 0, color = "grey", alpha = "0.5", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm")

#####
#Profit section:
subset_pr <- comp[!is.na(comp$quarters_since_breach) &
                  !is.na(comp$niq) & 
                  !is.infinite(comp$niq) &
                  comp$quarters_since_breach %in% -40:4,]
# resid_lm_profit_tt <- lm(niq ~ factor(gvkey) + factor(quarters_since_begin) + factor(gvkey)*time_since_begin, data = subset_pr)
# resid_lm_profit_nott <- lm(niq ~ factor(gvkey) + factor(quarters_since_begin), data = subset_pr)
# subset_pr$resid_profit_tt <- residuals(resid_lm_profit_tt)
# subset_pr$resid_profit_nott <- residuals(resid_lm_profit_nott)

temp_pr = cbind(aggregate(subset_pr, list(q = subset_pr$quarters_since_breach), mean, na.rm=T)[c('niq', 'resid_pr_tt', 'resid_pr_nott')], sort(unique(subset_pr$quarters_since_breach)))
names(temp_pr) <- c('mean_niq', 'mean_resid_profit_tt', 'mean_resid_profit_nott', 'quarters_since_breach')
# subset_pr <- merge(subset, temp, by='quarters_since_breach', all.x=T)

# TT all data
ggplot(subset_pr, aes(x = quarters_since_breach, y = resid_pr_tt)) + 
  geom_point(alpha=0.6, color='grey') + 
  stat_summary_bin(fun.y='mean', color='orange', bins=70, size=3, geom='point') + 
  #scale_x_continuous(limits = c(-10, 10)) + 
  scale_y_continuous(limits = c(-25, 25)) + 
  geom_vline(xintercept = 0, color = "blue", size = 0.5) + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach <= 0), method = "lm", size=1, color='red') + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach >= 0), method = "lm", size=1, color='red')

# NOTT all data
ggplot(subset_pr, aes(x = quarters_since_breach, y = resid_pr_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-20, 20)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  ylab('') +
  xlab('Quarters Since Breach') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) +
  ggsave('tables/mean_resid_profit.png')

# TT only means
ggplot(temp_pr, aes(x = quarters_since_breach, y = mean_resid_profit_tt)) + geom_point() +
  geom_vline(temp_pr, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp_pr, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp_pr, quarters_since_breach >= 0), method = "lm")
ggplot(temp_pr, aes(x = quarters_since_breach, y = mean_resid_profit_tt)) + geom_point() +
  geom_vline(temp_pr, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp_pr, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp_pr, quarters_since_breach >= 0), method = "lm")
ggplot(temp_pr, aes(x = quarters_since_breach, y = mean_niq)) + geom_point() +
  geom_vline(temp_pr, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp_pr, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp_pr, quarters_since_breach >= 0), method = "lm")

# NOTT only means
ggplot(temp_pr, aes(x = quarters_since_breach, y = mean_resid_profit_nott)) + geom_point() +
  geom_vline(temp_pr, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp_pr, quarters_since_breach <= 0), method = "lm", se=F) +
  geom_smooth(data=subset(temp_pr, quarters_since_breach >= 0), method = "lm", se=F) +
  xlab('Quarters Since Breach') + ylab('Mean Residual Profit') + 
  ggtitle('Residual Profit (Fixed Effects Removed)') + 
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) +
  ggsave('tables/mean_resid_profit.png')
ggplot(temp_pr, aes(x = quarters_since_breach, y = mean_niq)) + geom_point() +
  geom_vline(temp_pr, xintercept = 0, color = "orange", alpha = "0.5", size = 2) +
  geom_smooth(data=subset(temp_pr, quarters_since_breach <= 0), method = "lm") +
  geom_smooth(data=subset(temp_pr, quarters_since_breach >= 0), method = "lm")

# Constant Sample:
subset_pr_const <- comp[comp$gvkey %in% comp_full10,] 
subset_pr_const <- subset_pr_const[!is.na(subset_pr_const$time_since_breach) & !is.na(subset_pr_const$niq) & !is.infinite(subset_pr_const$niq),]
subset_pr_const <- subset_pr_const[subset_pr_const$quarters_since_breach %in% -10:10,]
resid_lm_profit_const <- felm(niq ~ 0 | factor(gvkey) + factor(datafqtr) | 0 | 0, data = subset_pr_const)
subset_pr_const$resid_profit <- residuals(resid_lm_profit_const)
ggplot(subset_pr_const, aes(x = quarters_since_breach, y = resid_profit)) + geom_point(color='grey', alpha=0.8) + 
  scale_y_continuous(limits = c(-500, 1000)) +
  stat_summary_bin(fun.y='mean', color='orange', bins=70, size=3, geom='point') + 
  geom_vline(subset_pr_const, mapping = aes(x = quarters_since_breach, y = resid), xintercept = 0, color = "blue", size = 0.5, alpha=0.6) + 
  geom_smooth(data=subset(subset_pr_const, quarters_since_breach <= 0),  method = "lm", color='red') + 
  geom_smooth(data=subset(subset_pr_const, quarters_since_breach >= 0),  method = "lm", color='red')


######
# Original specification:
subset_pr <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$niq) & 
                    !is.infinite(comp$niq) &
                    comp$quarters_since_breach %in% -40:8,]

orig_lm_pr_tt_ni <- felm(niq ~ after | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset_pr)
orig_lm_pr_tt <- felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset_pr)
orig_lm_pr_tt_c1 <- felm(niq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset_pr)
orig_lm_pr_tt_c2 <- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset_pr)
orig_lm_pr_tt_c3 <- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_begin | 0 | gvkey, data = subset_pr)
orig_lm_pr_nott_ni <- felm(niq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr)
orig_lm_pr_nott <- felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr)
orig_lm_pr_nott_c1 <- felm(niq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr)
orig_lm_pr_nott_c1_ni <- felm(niq ~ after + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr)
orig_lm_pr_nott_c2<- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr)
orig_lm_pr_nott_c3 <- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr)

quart_lm_pr_tt <- felm(niq ~ after + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_breach | 0 | gvkey, data=subset_pr)
quart_lm_pr_tt_c2 <- felm(niq ~ after + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) + factor(gvkey):quarters_since_breach | 0 | gvkey, data=subset_pr)
quart_lm_pr_nott <- felm(niq ~ after + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data=subset_pr)
quart_lm_pr_nott_c2 <- felm(niq ~ after + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data=subset_pr)

pr_mean <- round(mean(subset_pr$niq),2)
pr_sd <- round(sd(subset_pr$niq), 2)
# Variant 3:
stargazer(orig_lm_pr_nott_ni, orig_lm_pr_nott, orig_lm_pr_nott_c2, quart_lm_pr_nott, quart_lm_pr_nott_c2,
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Net Income'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', pr_mean, pr_mean, pr_mean, pr_mean, pr_mean),
                           c('Dependant SD', pr_sd, pr_sd, pr_sd, pr_mean, pr_sd)),
          notes = c('Standard errors clustered at the company level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years before breach, and event period 10 years after'),
          out = 'tables/profit_specification3_3y.html',
          type='html')

# Variant 4
subset_xopr <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$xoprq) &
                    !is.infinite(comp$xoprq) &
                    comp$quarters_since_breach %in% -20:4,]
orig_lm_xopr_nott_c1_ni <- felm(xoprq ~ after + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_xopr)
xopr_mean <- round(mean(subset_xopr$xoprq),2)
xopr_sd <- round(sd(subset_xopr$xoprq), 2)
stargazer(orig_lm_pr_nott_c1_ni, orig_lm_abs_nott_c1_ni, orig_lm_xopr_nott_c1_ni,
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          dep.var.labels = c('Net Income', 'Revenue', 'Operating Expenses'),
          covariate.labels = c('After Breach','After breach x Customer Data Leaked', 'After breach x Credit Card Leaked', 'After breach x SSN Leaked', 'After breach x Name Leaked', 'After breach x Address Leaked'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', pr_mean, abs_mean, xopr_mean),
                           c('Dependant SD', pr_sd, abs_sd, xopr_sd)),
          notes = c('Standard errors clustered at the company level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years before breach, and event period 10 years after'),
          out = 'tables/comb_specification4.html',
          type='html')

ggplot(subset_pr, aes(x = quarters_since_breach, y = resid_pr_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=100, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-30, 30)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  ggtitle('Mean Residual Profit (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) #+ 
  #ggsave('tables/mean_resid_profit_6m.png')

ggplot(subset, aes(x = quarters_since_breach, y = niq)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=80, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(5, 20)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  ylab('') +
  xlab('Quarters Since Breach') +
  # ggtitle('Mean Profit') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) + 
  ggsave('tables/mean_profit.png')

subset_pr <- comp[!is.na(comp$quarters_since_breach) &
                 !is.na(comp$niq) &
                 !is.infinite(comp$niq) &
                 comp$quarters_since_breach %in% -40:4,]
# Variant 5:
stargazer(felm(niq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr), 
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset_pr), 
          #omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          #covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Revenue'),
          omit.stat = 'ser',
          #add.lines = list(c('Dependant Mean', abs_mean, abs_mean, abs_mean, abs_mean, abs_mean),
          #                 c('Dependant SD', abs_sd, abs_sd, abs_sd, abs_sd, abs_sd)),
          notes = c('Standard errors clustered at the company level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years before breach, and event period up to 10 years after'),
          type='text')

######
# Constant Samples:
subset_pr_full5 <- comp_full5[!is.na(comp_full5$quarters_since_breach) &
                             !is.na(comp_full5$niq) &
                             !is.infinite(comp_full5$niq),]
subset_pr_full10 <- comp_full10[!is.na(comp_full10$quarters_since_breach) &
                               !is.na(comp_full10$niq) &
                               !is.infinite(comp_full10$niq),]
subset_pr_full15 <- comp_full15[!is.na(comp_full15$quarters_since_breach) &
                               !is.na(comp_full15$niq) &
                               !is.infinite(comp_full15$niq),]
subset_pr_full20 <- comp_full20[!is.na(comp_full20$quarters_since_breach) &
                               !is.na(comp_full20$niq) &
                               !is.infinite(comp_full20$niq),]

orig_lm_pr_nott_f5 <- felm(niq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_pr_full5)
orig_lm_pr_nott_f10 <- felm(niq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_pr_full10)
orig_lm_pr_nott_f15 <- felm(niq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_pr_full15)
orig_lm_pr_nott_f20 <- felm(niq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_pr_full20)

stargazer(orig_lm_pr_nott, orig_lm_pr_nott_f5, orig_lm_pr_nott_f10, orig_lm_pr_nott_f15, orig_lm_pr_nott_f20, 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant', 'rev_quart_1', 'rev_quart_2', 'rev_quart_3'),
          covariate.labels = c('After Breach', 'After Breach x Quarter'),
          dep.var.labels = c('Net Income'),
          notes = c('Standard Errors clustered at the company level', 'Company and quarter fixed effects in all specifications'),
          add.lines = list(c('Sample:', 'Full', 'Full 5', 'Full 10', 'Full 15', 'Full 20'),
                           c('Dependant mean', pr_mean, pr_mean, pr_mean, pr_mean, pr_mean)),
          omit.stat = 'ser',
          type='text')

# playzone 
stargazer(lm(profit_proportion ~ after + after_quarter_interact + xsgay_log + xsgay_interact_log + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset), 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'Yes')),
          type='text')


###### 
# Trend section:

subset_tr <- comp[!is.na(comp$time_since_breach) & !is.na(comp$trend_index_company) & !is.infinite(comp$trend_index_company),]
subset_tr$after_quarter_interact <- subset_tr$after * subset_tr$quarters_since_begin
resid_lm_tr <- lm(trend_index_company ~ factor(gvkey) + factor(datafqtr), data = subset_tr)
subset_tr$resid_tr <- residuals(resid_lm_tr)
ggplot(subset_tr, aes(x = quarters_since_breach, y = resid_tr)) + geom_point() + stat_summary_bin(fun.y='mean', bins=20, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 20)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset_tr, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset_tr, quarters_since_breach >= 0), method = "lm")

orig_lm_tr_nott <- lm(trend_index_company ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr), data = subset_tr)
stargazer(orig_lm_tr_nott, 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'No')),
          type='text')

###### 
#Advertising section:
subset_mrkt <- comp[!is.na(comp$time_since_breach) & !is.na(comp$xsgaq_log) & !is.infinite(comp$xsgaq_log),]
subset_mrkt$after_quarter_interact <- subset_mrkt$after * subset_mrkt$quarters_since_begin
resid_lm_mrkt <- lm(xsgaq ~ factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_breach, data = subset_mrkt)
resid_lm_mrkt_log <- lm(xsgaq_log ~ factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_breach, data = subset_mrkt)
subset_mrkt$resid_mrkt <- residuals(resid_lm_mrkt)
subset_mrkt$resid_mrkt_log <- residuals(resid_lm_mrkt_log)
ggplot(subset_mrkt, aes(x = quarters_since_breach, y = resid_mrkt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 2000)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset_mrkt, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset_mrkt, quarters_since_breach > 0), method = "lm")
ggplot(subset_mrkt, aes(x = quarters_since_breach, y = resid_mrkt_log)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 1.5)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset_mrkt, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset_mrkt, quarters_since_breach > 0), method = "lm")

orig_lm_mrkt <- lm(xsgaq ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_breach, data=subset_mrkt)
stargazer(orig_lm_mrkt, 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'Yes')),
          type='text')

# Original specification:
orig_lm_mrkt_tt <- lm(xsgay ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr) + factor(gvkey)*quarters_since_begin, data = subset_mrkt)
orig_lm_mrkt_nott <- lm(xsgay ~ after + after_quarter_interact + factor(gvkey) + factor(datafqtr), data = subset_mrkt)
stargazer(orig_lm_mrkt_nott, orig_lm_mrkt_tt, 
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes'), c('Company specific time trend:', 'No', 'Yes')),
          type='text')





######
# Other Controls
window_length = 10
y_scale = 0.1
cont <- 'niq'
subset_test <- comp[!is.na(comp$quarters_since_breach) &
                    #!is.na(comp[[cont]]) & 
                    #!is.infinite(comp[[cont]]),
                    comp$quarters_since_breach %in% -20:4,]
resid_lm_test <- lm(paste(cont, " ~ factor(gvkey) + factor(datafqtr)",sep = ""), data = subset_test)
subset_test$resid <- residuals(resid_lm_test)
temp = cbind(aggregate(subset_test, list(q = subset_test$quarters_since_breach), mean, na.rm=T)['resid'], -53:51)
names(temp) <- c('mean_resid', 'quarters_since_breach')
subset_test <- merge(subset_test, temp, by='quarters_since_breach', all.x=T)

ggplot(subset_test) + 
  geom_point(aes(x = quarters_since_breach, y = resid), color='grey', alpha='0.8') + 
  geom_point(aes(x = quarters_since_breach, y = mean_resid), color='red', size=2) + 
  scale_x_continuous(limits = c(-window_length, window_length)) + 
  scale_y_continuous(limits = c(max(mean(subset_test$resid) - sd(subset_test$resid)*y_scale, min(subset_test$resid)), mean(subset_test$resid) + sd(subset_test$resid)*y_scale)) +
  geom_vline(subset_rev_const-sample, mapping = aes(x = quarters_since_breach, y = resid), xintercept = 0, color = "orange", size = 0.5, alpha=0.6) + 
  geom_smooth(data=subset(subset_test, quarters_since_breach %in% 0:window_length), mapping = aes(x = quarters_since_breach, y = resid), method = "lm") + 
  geom_smooth(data=subset(subset_test, quarters_since_breach %in% -window_length:0), mapping = aes(x = quarters_since_breach, y = resid), method = "lm")


# Regression model:
stargazer(felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(atq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(actq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(chq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(ltq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(lctq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(uaptq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(dlcq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(xsgaq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(ceqq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(emp ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(trend_index_company ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(trend_index_tic ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          dep.var.labels = c('Revenue', 'Net Income', 'Total Assets', 'Current Assets', 'Cash',
                             'Total Liabilities', 'Current Liabilities', 'Accounts Payable', 'Debt', 'Operating Expenses',
                             'Sales, General and Other Expenses', 'Total Shareholders\' Equity', 'Employees',
                             'Google Searches (Company Name)', 'Google Searches (Stock Ticker)'),
          covariate.labels = c('After Breach', 'After Breach x Quarters Since Breach'),
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          omit.stat = 'ser',
          notes = c('Standard Errors clustered at the quarter level', 'Company and quarter fixed effects in all specifications'),
          out='tables/potential_controls.html',
          type='text')


mean_revtq <- round(subset_test$revtq, 2)
mean_niq <- round(subset_test$niq, 2)
mean_xoprq <- round(subset_test$xoprq, 2)
mean_xsgaq <- round(subset_test$xsgaq, 2)
mean_ceqq <- round(subset_test$ceqq, 2)
mean_emp <- round(subset_test$emp, 2)
mean_trend_index_company <- round(subset_test$trend_index_company, 2)
mean_trend_index_tic <- round(subset_test$trend_index_tic, 2)
stargazer(felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(xsgaq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(ceqq ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(emp ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(trend_index_company ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          felm(trend_index_tic ~ after + after_quarter_interact | factor(gvkey) + datafqtr | 0 | gvkey, data = subset_test),
          dep.var.labels = c('Operating Expenses', 'Sales, General and Other Expenses',
                             'Total Shareholders\' Equity', 'Number of Employees', 'Google Searches (Company Name)', 'Google Searches (Stock Ticker)'),
          covariate.labels = c('After Breach', 'After Breach x Quarters Since Breach'),
          add.lines = list(c('Dependant Mean', mean_xoprq, mean_xsgaq, mean_ceqq, mean_emp, mean_trend_index_company, mean_trend_index_tic)),
          omit=c('gvkey', 'datafqtr', 'quarters_since_begin', 'Constant'),
          omit.stat = 'ser',
          notes = c('Standard Errors clustered at the quarter level', 'Company and quarter fixed effects in all specifications', 'Prediction period is up to 10 years before breach, and event period 10 years after'),
          out='tables/other_controls.html',
          type='html')
######
library(eventstudies)
library(reshape2)
crsp = read.csv('data/CRSP_merged_feb.csv')
crsp$RET <- as.numeric(as.character(crsp$RET))
crsp$RETX <- as.numeric(as.character(crsp$RETX))
crsp$Date.Made.Public <- as.Date(crsp$Date.Made.Public, format='%B %d, %Y')
crsp$date <- as.Date(crsp$date)

for (i in 1:nrow(crsp)) {
  if(i %% 100 == 0){
    print(paste('Rows remaining: ', nrow(crsp)-i))
  }
  
  tic = crsp[i,'TICKER']
  date = crsp[i, 'date']
  
  company_slice = crsp[crsp$TICKER == tic,] 
  breach_dates = company_slice$Date.Made.Public
  breach_dates = na.omit(breach_dates)
  idx = which.min(abs(date-breach_dates))
  tic = paste0(tic, '_', idx)
  crsp[i,'ticker_occurance'] = tic
  last_breach = breach_dates[idx]
  
  crsp[i, 'breachdate'] = last_breach
  crsp[i, 'days_since_breach'] = date - last_breach
  crsp[i, 'customer'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$customer)[1]
  crsp[i, 'employee'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$employee)[1]
  crsp[i, 'credit_card'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$credit_card)[1]
  crsp[i, 'cvv'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$cvv)[1]
  crsp[i, 'social_security'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$social_security)[1]
  crsp[i, 'name'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$name)[1]
  crsp[i, 'address'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$address)[1]
  crsp[i, 'personal_information'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$personal_information)[1]
  crsp[i, 'Total.Records'] = na.omit(company_slice[company_slice$breachdate == last_breach,]$Total.Records)[1]
}

write.csv(crsp, 'data/CRSP_cleaned_mar.csv')


crsp <- read.csv('data/CRSP_cleaned_mar.csv')
crsp$RET <- as.numeric(as.character(crsp$RET))
crsp$RETX <- as.numeric(as.character(crsp$RETX))
crsp$Date.Made.Public <- as.Date(crsp$Date.Made.Public, format='%B %d, %Y')
crsp$date <- as.Date(crsp$date)
crsp$breachdate <- as.Date(crsp$breachdate)
crsp$RFRET <- crsp$RET - crsp$RF
crsp$RFRETX <- crsp$RETX - crsp$RF

estimation_window <- 150
prediction_window <- 10
crsp_estimation_window <- crsp[crsp$days_since_breach >= -1 * estimation_window & crsp$days_since_breach < 0,]
crsp_pred_window <- crsp[crsp$days_since_breach <= prediction_window & crsp$days_since_breach >= 0,]
crsp_event_window <- crsp[crsp$days_since_breach <= prediction_window & crsp$days_since_breach >= -1 * prediction_window,]

results <- data.frame(ticker_occurance = character(),
                      car_mmodel = double(),
                      car_ffmodel = double(),
                      aar_mmodel = double(),
                      aar_ffmodel = double(),
                      est_size = integer(),
                      pred_size = integer(),
                      stringsAsFactors = FALSE)
cars <- data.frame(ticker_occurance = character(),
                   days_since_breach = integer(),
                   car_mmodel = double(),
                   car_ffmodel = double(),
                   stringsAsFactors = FALSE)
sample_size <- 0
for (ticker_occurance in unique(crsp$ticker_occurance)) {
  subset_est <- crsp_estimation_window[crsp_estimation_window$ticker_occurance == ticker_occurance,]
  subset_pred <- crsp_pred_window[crsp_pred_window$ticker_occurance == ticker_occurance,]
  subset_evt <- crsp_event_window[crsp_event_window$ticker_occurance == ticker_occurance,]
  
  if(nrow(subset_est) <= estimation_window - (estimation_window/7)*3 - 1) next
  if(nrow(subset_pred) <= prediction_window - (prediction_window/7)*3 - 1) next
  
  mmodel <- lm(RFRET ~ Mkt.RF, data=subset_est)
  ffmodel <- lm(RFRET ~ Mkt.RF + SMB + HML + RMW + CMA, data=subset_est)
  subset_pred$mmodel_pred <- predict(mmodel, subset_pred)
  subset_pred$ffmodel_pred <- predict(ffmodel, subset_pred)
  
  subset_evt$mmodel_pred <- predict(mmodel, subset_evt)
  subset_evt$ffmodel_pred <- predict(ffmodel, subset_evt)
  subset_evt$abnormal_mmodel <- subset_evt$RFRET - subset_evt$mmodel_pred
  subset_evt$abnormal_ffmodel <- subset_evt$RFRET - subset_evt$ffmodel_pred
  subset_evt$car_mmodel <- cumsum(subset_evt$abnormal_mmodel)
  subset_evt$car_ffmodel <- cumsum(subset_evt$abnormal_ffmodel)
  subset_evt$ticker_occurance <- as.character(subset_evt$ticker_occurance)
  subset_evt <- subset_evt[,c('ticker_occurance', 'days_since_breach', 'car_mmodel', 'car_ffmodel', 'abnormal_mmodel', 'abnormal_ffmodel')]
  
  car_mmodel <- as.numeric(as.character(subset_evt[nrow(subset_evt),]$car_mmodel))
  car_ffmodel <- as.numeric(as.character(subset_evt[nrow(subset_evt),]$car_ffmodel))
  aar_mmodel <- mean(as.numeric(as.character(subset_evt[subset_evt$days_since_breach >= 0,]$abnormal_mmodel)), na.rm=TRUE)
  aar_ffmodel <- mean(as.numeric(as.character(subset_evt[subset_evt$days_since_breach >= 0,]$abnormal_ffmodel)), na.rm=TRUE)
  
  # Try to rule out events that aren't caused by the breach
  if(!(subset_evt[which.min(subset_evt$abnormal_mmodel),]$days_since_breach %in% -2:2 || subset_evt[which.max(subset_evt$abnormal_mmodel),]$days_since_breach %in% -2:2)) next
  
  sample_size <- sample_size + 1
  print(paste0('Firm: ', ticker_occurance, 
               ' CAR: ', car_mmodel, 
               ' Est window: ', nrow(subset_est),
               ' Pred window: ', nrow(subset_pred)))
  results[nrow(results) + 1,] <- list("", car_mmodel, car_ffmodel, aar_mmodel, aar_ffmodel, nrow(subset_est), nrow(subset_pred))
  results[nrow(results),'ticker_occurance'] <- ticker_occurance
  cars <- rbind(cars, subset_evt)
}

names(cars) <- c('ticker_occurance', 'days_since_breach', 'car_mmodel', 'car_ffmodel', 'aar_mmodel', 'aar_ffmodel')
cars <- na.omit(cars)
cars$car_mmodel <- as.numeric(as.character(cars$car_mmodel))
cars$car_ffmodel <- as.numeric(as.character(cars$car_ffmodel))
cars$days_since_breach <- as.numeric(as.character(cars$days_since_breach))
names(results) <- c('ticker_occurance', 'car_mmodel', 'car_ffmodel', 'aar_mmodel', 'aar_ffmodel', 'est_size', 'pred_size')
results <- na.omit(results)
results$car_mmodel <- as.numeric(as.character(results$car_mmodel))
results$car_ffmodel <- as.numeric(as.character(results$car_ffmodel))
results$aar_mmodel <- as.numeric(as.character(results$aar_mmodel))
results$aar_ffmodel <- as.numeric(as.character(results$aar_ffmodel))
mmodel_tstat <- sqrt(nrow(results)) * (mean(results$car_mmodel)/sd(results$car_mmodel))
ffmodel_tstat <- sqrt(nrow(results)) * (mean(results$car_ffmodel)/sd(results$car_ffmodel))

acar_mmodel <- aggregate(car_mmodel ~ days_since_breach, data = cars, FUN=mean)
acar_ffmodel <- aggregate(car_ffmodel ~ days_since_breach, data = cars, FUN=mean)
acar_mmodel$days_since_breach <- as.numeric(as.character(acar_mmodel$days_since_breach))
acar_ffmodel$days_since_breach <- as.numeric(as.character(acar_ffmodel$days_since_breach))

results$car_mmodel_percent <- 100*results$car_mmodel
results$car_ffmodel_percent <- 100*results$car_ffmodel
results$aar_mmodel_percent <- 100*results$aar_mmodel
results$aar_ffmodel_percent <- 100*results$aar_ffmodel

to_print = data.frame(market_model=character(), ff_model=character())
to_print <- rbind(to_print, list(nrow(results),nrow(results)))
to_print <- rbind(to_print, list(round(mean(results$car_mmodel_percent), 2), round(mean(results$car_ffmodel_percent), 2)))
to_print <- rbind(to_print, list(round(sd(results$car_mmodel_percent)/sqrt(nrow(results)), 2), round(sd(results$car_ffmodel_percent)/sqrt(nrow(results)), 2)))
to_print <- rbind(to_print, list(round(mmodel_tstat, 2), round(ffmodel_tstat), 2))
to_print <- rbind(to_print, list(round(pt(mmodel_tstat, nrow(results) - 1), 2), round(pt(ffmodel_tstat, nrow(results) - 1), 2)))
names(to_print) <- c('Market Model', 'Fama French Model')
row.names(to_print) <- c('n','mean', 'sd', 't-stat', 'p')

write.csv(results, paste0('data/stock_study_cars_', prediction_window,'day.csv'))
write.csv(to_print, paste0('data/stock_study_test_', prediction_window,'day.csv'), row.names = T, col.names = T)

ggplot(acar_mmodel, aes(x=days_since_breach, y=car_mmodel)) + geom_line() +
  geom_vline(xintercept = 0, color='blue') +
  ylab('Average Cumulative Abnormal Return') +
  xlab('Days Since Breach') +
  ggtitle('Event Study (Market Model)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif')) +
  ggsave('tables/mmodel_eventstudy.png')
ggplot(acar_ffmodel, aes(x=days_since_breach, y=car_ffmodel)) + geom_line() +
  geom_vline(xintercept = 0, color='blue') +
  ylab('Average Cumulative Abnormal Return') +
  xlab('Days Since Breach') +
  ggtitle('Event Study (Fama-French Model)') +
  theme(plot.title = element_text(hjust=0.5)) +
  ggsave('tables/ffmodel_eventstudy.png')



######
es_controls <- read.csv('data/event_study_controls.csv')
es_controls$Total.Records_tolog <- es_controls$Total.Records
es_controls[!is.na(es_controls$Total.Records) & es_controls$Total.Records == 0,]$Total.Records_tolog <- 0.0001
es_controls$Total.Records_log <- log(es_controls$Total.Records_tolog)
stargazer(felm(car_mmodel_percent_2d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_5d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_10d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_2d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_5d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_10d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          dep.var.labels = c('Mkt Model CAR (2 day)', 'Mkt Model CAR (5 day)', 'Mkt Model CAR (10 day)', 'FF Model CAR (2 day)', 'FF Model CAR (5 day)', 'FF Model CAR (10 day)'),
          covariate.labels = c('Records Leaked (log)', 'Customer Data Leaked', 'Employee Data Leaked', 'Credit Card Leaked', 'SSN Leaked', 'Name Leaked', 'Address Leaked'),
          omit.stat = 'ser',
          notes = c('Year fixed effects in all specifications'),
          type='text')

stargazer(felm(car_mmodel_percent_2d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | factor(TICKER) + Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_5d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | factor(TICKER) + Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_10d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | factor(TICKER) + Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_2d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | factor(TICKER) + Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_5d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | factor(TICKER) + Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_ffmodel_percent_10d ~ Total.Records_log + customer + employee + credit_card + social_security + name + address | factor(TICKER) + Year.of.Breach | 0 | 0, data = es_controls),
          dep.var.labels = c('Mkt Model CAR (2 day)', 'Mkt Model CAR (5 day)', 'Mkt Model CAR (10 day)', 'FF Model CAR (2 day)', 'FF Model CAR (5 day)', 'FF Model CAR (10 day)'),
          covariate.labels = c('Records Leaked (log)', 'Customer Data Leaked', 'Employee Data Leaked', 'Credit Card Leaked', 'SSN Leaked', 'Name Leaked', 'Address Leaked'),
          omit.stat = 'ser',
          notes = c('Company and quarter fixed effects in all specifications'),
          type='text')

stargazer(felm(car_mmodel_percent_2d ~ Total.Records_log + customer+ credit_card + social_security + name + address | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_5d ~ Total.Records_log + customer + credit_card + social_security + name + address |  Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_10d ~ Total.Records_log + customer + credit_card + social_security + name + address |  Year.of.Breach | 0 | 0, data = es_controls),
          dep.var.labels = c('Mkt Model CAR (2 day)', 'Mkt Model CAR (5 day)', 'Mkt Model CAR (10 day)', 'FF Model CAR (2 day)', 'FF Model CAR (5 day)', 'FF Model CAR (10 day)'),
          covariate.labels = c('Records Leaked (log)', 'Customer Data Leaked', 'Credit Card Leaked', 'SSN Leaked', 'Name Leaked', 'Address Leaked'),
          omit.stat = 'ser',
          notes = c('Year fixed effects in all specifications'),
          out='stock_controls.html',
          type='html')

factor(Type.of.breach)

stargazer(felm(car_mmodel_percent_2d ~ factor(Type.of.breach) | Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_5d ~ factor(Type.of.breach) |  Year.of.Breach | 0 | 0, data = es_controls),
          felm(car_mmodel_percent_10d ~ factor(Type.of.breach) |  Year.of.Breach | 0 | 0, data = es_controls),
          dep.var.labels = c('Mkt Model CAR (2 day)', 'Mkt Model CAR (5 day)', 'Mkt Model CAR (10 day)', 'FF Model CAR (2 day)', 'FF Model CAR (5 day)', 'FF Model CAR (10 day)'),
          #covariate.labels = c('Records Leaked (log)', 'Customer Data Leaked', 'Credit Card Leaked', 'SSN Leaked', 'Name Leaked', 'Address Leaked'),
          omit.stat = 'ser',
          notes = c('Year fixed effects in all specifications'),
          type='text')

