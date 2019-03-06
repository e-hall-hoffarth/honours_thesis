setwd('/mnt/good/honours_thesis/')
load('.RData')

library(stargazer)
library(ggplot2)
library(lfe)
library(zoo)

comp = read.csv('data/COMPUSTAT_merged_trends_feb.csv')
comp$datadate = as.Date(comp$datadate)
comp$breachdate = as.Date(comp$Date.Made.Public, format="%Y-%m-%d", optional=T)
comp_match = subset(comp, (!is.na(comp$match)))
comp$treat <- ifelse(comp$gvkey %in% comp_match$gvkey,1,0)
comp$quarters_since_begin <- (as.yearqtr(comp$datacqtr, format('%YQ%q')) - as.yearqtr('2005 Q1'))*4
comp$quarter_as_date <- as.Date(as.yearqtr(comp$datacqtr, format('%YQ%q')))

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

comp$after <- ifelse(comp$datadate > comp$breachdate,1,0)
comp$time_since_begin <- comp$datadate - as.Date("2005-01-01")
comp$breachquarter <- as.yearqtr(comp$breachdate)
comp$quarters_since_breach <- (as.yearqtr(comp$datacqtr, format('%YQ%q')) - comp$breachquarter)*4
comp$diffindiff <- comp$treat * comp$after

comp$customer_interact <- comp$customer * comp$diffindiff
comp$employee_interact <- comp$employee * comp$diffindiff
comp$customer_interact <- comp$customer * comp$diffindiff
comp$credit_card_interact <- comp$credit_card * comp$diffindiff
comp$cvv_interact <- comp$cvv * comp$diffindiff
comp$social_security_interact <- comp$social_security * comp$diffindiff
comp$name_interact <- comp$name * comp$diffindiff
comp$address_interact <- comp$address * comp$diffindiff
comp$personal_information_interact <- comp$personal_information * comp$ diffindiff
comp$Total.Records_interact <- comp$Total.Records * comp$diffindiff

comp[comp$Total.Records == 0,]$Total.Records <- 0.0001
comp$Total.Records_log <- log(comp$Total.Records)
comp$Total.Records_interact_log <- comp$Total.Records_log * comp$diffindiff
comp$trend_index_company_interact <- comp$trend_index_company * comp$diffindiff
comp$trend_index_tic_interact <- comp$trend_index_tic * comp$diffindiff

comp[!is.na(comp$xsgaq) & comp$xsgaq == 0,]$xsgaq <- 0.0001
comp$xsgaq_log <- log(comp$xsgaq)
comp$xsgaq_interact <- comp$xsgaq * comp$diffindiff
comp$xsgaq_interact_log <- comp$xsgaq_log * comp$diffindiff

comp[!is.na(comp$revtq) & comp$revtq == 0,]$revtq <- 0.0001
#comp[is.na(comp$revtq),]$revtq <- 0
comp[!is.na(comp$revtq) & comp$revtq < 0,]$revtq <- NA
comp$log_revtq <- log(comp$revtq)

comp$rq1_interact <- comp$rev_quart_1*comp$after
comp$rq2_interact <- comp$rev_quart_2*comp$after
comp$rq3_interact <- comp$rev_quart_3*comp$after
comp$rq4_interact <- comp$rev_quart_4*comp$after
comp$after_quarter_interact <- comp$after * comp$quarters_since_begin

comp_full10 <- intersect(comp[comp[c("quarters_since_breach", "gvkey")]$quarters_since_breach == -10,]$gvkey, comp[comp[c("quarters_since_breach", "gvkey")]$quarters_since_breach == 10,]$gvkey)
comp_full10 <- comp_full10[!is.na(comp_full10)]
comp_full5 <- intersect(comp[comp[c("quarters_since_breach", "gvkey")]$quarters_since_breach == -5,]$gvkey, comp[comp[c("quarters_since_breach", "gvkey")]$quarters_since_breach == 5,]$gvkey)
comp_full5 <- comp_full5[!is.na(comp_full5)]


write.csv(comp, 'data/COMPUSTAT_clean.csv')

# invshift <- nrow(comp) - 1
# comp$profit_proportion <- c(1, (head(comp, invshift)$niq - tail(comp, invshift)$niq)/tail(comp, invshift)$niq)

######
# Revenue OLS
subset <- comp[!is.na(comp$time_since_breach) & !is.na(comp$log_revtq) & !is.infinite(comp$log_revtq),]
ols_lm_log_tt <- felm(log_revtq ~ factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c1 <- felm(log_revtq ~ xsgay_log + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c2 <- felm(log_revtq ~ xsgay_log + Total.Records_log + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c3 <- felm(log_revtq ~ xsgay_log + Total.Records_log + trend_index_company + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_log_tt_c4 <- felm(log_revtq ~ xsgay_log + Total.Records_log + trend_index_company + customer + employee + credit_card + cvv + social_security + name + address + personal_information + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)

ols_lm_abs_tt <- felm(revtq ~ factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c1 <- felm(revtq ~ xsgay_log + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c2 <- felm(revtq ~ xsgay_log + Total.Records_log + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c3 <- felm(revtq ~ xsgay_log + Total.Records_log + trend_index_company + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)
ols_lm_abs_tt_c4 <- felm(revtq ~ xsgay_log + Total.Records_log + trend_index_company + customer + employee + credit_card + cvv + social_security + name + address + personal_information + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset)

# log_tt
stargazer(ols_lm_log_tt, ols_lm_log_tt_c1, ols_lm_log_tt_c2, ols_lm_log_tt_c3, ols_lm_log_tt_c4,
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'Yes', 'Yes', 'Yes', 'Yes')),
          type='text')
# abs_tt
stargazer(ols_lm_abs_tt, ols_lm_abs_tt_c1, ols_lm_abs_tt_c2, ols_lm_abs_tt_c3, ols_lm_abs_tt_c4,
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'Yes', 'Yes', 'Yes', 'Yes')),
          type='text')

######
# Revenue graphing
subset <- comp[!is.na(comp$time_since_breach) & !is.na(comp$log_revtq) & !is.infinite(comp$log_revtq),]

resid_lm_log_tt <- felm(log_revtq ~ factor(gvkey) + factor(datacqtr) + factor(gvkey)*time_since_begin, data = subset)
resid_lm_abs_tt <- felm(revtq ~ factor(gvkey) + factor(datacqtr) + factor(gvkey)*time_since_begin, data = subset)
resid_lm_log_nott <- felm(log_revtq ~ factor(gvkey) + factor(datacqtr), data = subset)
resid_lm_abs_nott <- felm(revtq ~ factor(gvkey) + factor(datacqtr), data = subset)

subset$resid_log_tt <- residuals(resid_lm_log_tt)
subset$resid_abs_tt <- residuals(resid_lm_abs_tt)
subset$resid_log_nott <- residuals(resid_lm_log_nott)
subset$resid_abs_nott <- residuals(resid_lm_abs_nott)

ggplot(subset, aes(x = quarters_since_breach, y = resid_log_tt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 1.5)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_log), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach > 0), method = "lm")
ggplot(subset, aes(x = quarters_since_breach, y = resid_abs_tt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 5000)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach > 0), method = "lm")

ggplot(subset, aes(x = quarters_since_breach, y = resid_log_nott)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 2)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_log), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach > 0), method = "lm") + ylab('Residual Revenue (No time trends)') + xlab('Quarters Since Breach')
ggplot(subset, aes(x = factor(quarters_since_breach), y = resid_abs_nott)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 5000)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2)

# Plot by quartile
ggplot(subset[subset$rev_quart_4 == 1,], aes(x = quarters_since_breach, y = resid_log_tt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_x_continuous(limits = c(-25, 25)) + scale_y_continuous(limits = c(0, 2)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_log), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach > 0), method = "lm") + ylab('Residual Revenue (No time trends)') + xlab('Quarters Since Breach')
ggplot(subset[subset$rev_quart_4 == 1,], aes(x = quarters_since_breach, y = resid_abs_tt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_x_continuous(limits = c(-25, 25)) + scale_y_continuous(limits = c(0, 5000)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_log), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset, quarters_since_breach > 0), method = "lm") + ylab('Residual Revenue (No time trends)') + xlab('Quarters Since Breach')

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





# Original specification regression:
subset <- comp[!is.na(comp$time_since_breach) & !is.na(comp$log_revtq) & !is.infinite(comp$log_revtq),]

orig_lm_log_tt <- felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)
orig_lm_log_tt_c1 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)
orig_lm_log_tt_c2 <- felm(log_revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)
orig_lm_log_tt_c3 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)

orig_lm_abs_tt <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)
orig_lm_abs_tt_c1 <- felm(revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)
orig_lm_abs_tt_c2 <- felm(revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company_interact +  trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)
orig_lm_abs_tt_c3 <- felm(revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset)

orig_lm_log_nott <- felm(log_revtq ~ after + after_quarter_interact + factor(gvkey) + factor(datacqtr), data = subset)
orig_lm_log_nott_c1 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset)
orig_lm_log_nott_c2 <- felm(log_revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset)
orig_lm_log_nott_c3 <- felm(log_revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset)

orig_lm_abs_nott <- felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset)
orig_lm_abs_nott_c1 <- felm(revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset)
orig_lm_abs_nott_c2 <- felm(revtq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company_interact +  trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset)
orig_lm_abs_nott_c3 <- felm(revtq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + trend_index_company_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset)

quart_lm_abs_tt <- felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_breach | 0 | datacqtr, data = subset)
quart_lm_abs_tt_c1 <- felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_breach| 0 | datacqtr, data = subset)
quart_lm_abs_nott <- felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + datacqtr| 0 | datacqtr, data = subset)
quart_lm_abs_nott_c1 <- felm(revtq ~ after + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company_interact + trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr| 0 | datacqtr, data = subset)


# log_tt
stargazer(orig_lm_log_tt, orig_lm_log_tt_c2, orig_lm_log_tt_c1, orig_lm_log_tt_c3, 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'Yes', 'Yes', 'Yes', 'Yes')),
          type='text')
# abs_tt
stargazer(orig_lm_abs_tt, orig_lm_abs_tt_c2, orig_lm_abs_tt_c1, orig_lm_abs_tt_c3, 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'Yes', 'Yes', 'Yes', 'Yes')),
          type='text')
#log_nott
stargazer(orig_lm_log_nott, orig_lm_log_nott_c1, orig_lm_log_nott_c2, orig_lm_log_nott_c3, 
           omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
           add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'No', 'No', 'No', 'No')),
           type='text')
#abs_nott
stargazer(orig_lm_abs_nott, orig_lm_abs_nott_c1, orig_lm_abs_nott_c2, orig_lm_abs_nott_c3, 
           omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
           add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes'), c('Company specific time trend:', 'No', 'No', 'No', 'No')),
           type='text')



# Variant 1:
stargazer(orig_lm_log_tt, orig_lm_log_tt_c2, orig_lm_log_nott, orig_lm_log_nott_c2, orig_lm_abs_tt, orig_lm_abs_tt_c2, orig_lm_abs_nott, orig_lm_abs_nott_c2, 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach'),
          dep.var.labels = c('Revenue (log)', 'Revenue'),
          add.lines = list(c('Quarter Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'), 
                           c('Company Fixed Effects:', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'), 
                           c('Company specific time trend:', 'Yes', 'Yes', 'No', 'No', 'Yes', 'Yes', 'No', 'No')),
          notes.label = 'Note: Standard Errors clustered at the quarter level',
          omit.stat = 'ser',
          out = 'tables/revenue_specifications.html',
          type='html')

# Variant 2:
stargazer(orig_lm_log_tt, orig_lm_log_tt_c2, orig_lm_abs_tt, orig_lm_abs_tt_c2, quart_lm_abs_tt, quart_lm_abs_tt_c1,
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant', 'rev_quart_1', 'rev_quart_2', 'rev_quart_3'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1', 'After x Revenue Quartile 2', 'After x Revenue Quartile 3'),
          dep.var.labels = c('Revenue (log)', 'Revenue'),
          notes = c('Standard Errors clustered at the quarter level', 'Company and quarter fixed effects in all specifications'),
          omit.stat = 'ser',
          out = 'tables/revenue_specifications2.html',
          type='html')


# playzone 
stargazer(felm(revtq ~ after + rev_quart_1*after + rev_quart_2*after + rev_quart_3*after | factor(gvkey) + datacqtr| 0 | datacqtr, data = subset), 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'Yes')),
          type='text')

#####
#Profit section:
subset_pr <- comp[!is.na(comp$time_since_breach) & !is.na(comp$niq) & !is.infinite(comp$niq),]
resid_lm_profit <- lm(niq ~ factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset_pr)
subset_pr$resid_profit <- residuals(resid_lm_profit)
ggplot(subset_pr[subset_pr$rev_quart_1 == 1,], aes(x = quarters_since_breach, y = resid_profit)) + geom_point() + stat_summary_bin(fun.y='mean', color='orange', size=4, geom='point') + scale_x_continuous(limits = c(-25, 25)) + scale_y_continuous(limits = c(-500, 500)) + geom_vline(xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset_pr, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset_pr, quarters_since_breach > 0), method = "lm")

# Original specification:
orig_lm_pr_tt <- felm(niq ~ after + after_quarter_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset_pr)
orig_lm_pr_tt_c1 <- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset_pr)
orig_lm_pr_tt_c2 <- felm(niq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset_pr)
orig_lm_pr_tt_c3 <- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_begin | 0 | datacqtr, data = subset_pr)
orig_lm_pr_nott <- felm(niq ~ after + after_quarter_interact + factor(gvkey) + factor(datacqtr), data = subset_pr)
orig_lm_pr_nott_c1 <- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_pr)
orig_lm_pr_nott_c2 <- felm(niq ~ after + after_quarter_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_pr)
orig_lm_pr_nott_c3 <- felm(niq ~ after + after_quarter_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_pr)

quart_lm_pr_abs_tt <- felm(niq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_breach | 0 | datacqtr, data=subset_pr)
quart_lm_pr_abs_tt_c1 <- felm(niq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact + Total.Records_interact_log + trend_index_company + trend_index_company_interact | factor(gvkey) + datacqtr + factor(gvkey):quarters_since_breach | 0 | datacqtr, data=subset_pr)

# Variant 1:
stargazer(orig_lm_pr_tt, orig_lm_pr_tt_c1, orig_lm_pr_nott, orig_lm_pr_nott_c1,
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach'),
          dep.var.labels = c('Net Income'),
          add.lines = list(c('Company specific time trend:', 'Yes', 'Yes', 'No', 'No')),
          notes = c('Standard Errors clustered at the quarter level', 'Company and quarter fixed effects in all specifications'),
          omit.stat = 'ser',
          out = 'tables/profit_specifications.html',
          type='html')

# Variant 2:
stargazer(orig_lm_pr_tt, orig_lm_pr_tt_c1, orig_lm_pr_nott, orig_lm_pr_nott_c1, quart_lm_pr_abs_tt, quart_lm_pr_abs_tt_c1,
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3'),
          dep.var.labels = c('Net Income'),
          omit.stat = 'ser',
          add.lines = list(c('Company specific time trend:', 'Yes', 'Yes', 'No', 'No', 'No', 'No')),
          notes = c('Standard Errors clustered at the quarter level', 'Company and quarter fixed effects in all specifications'),
          out = 'tables/profit_specification2.html',
          type='html')



# playzone 
stargazer(lm(profit_proportion ~ after + after_quarter_interact + xsgay_log + xsgay_interact_log + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset), 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'Yes')),
          type='text')







###### 
# Trend section:

subset_tr <- comp[!is.na(comp$time_since_breach) & !is.na(comp$trend_index_company) & !is.infinite(comp$trend_index_company),]
subset_tr$after_quarter_interact <- subset_tr$after * subset_tr$quarters_since_begin
resid_lm_tr <- lm(trend_index_company ~ factor(gvkey) + factor(datacqtr), data = subset_tr)
subset_tr$resid_tr <- residuals(resid_lm_tr)
ggplot(subset_tr, aes(x = quarters_since_breach, y = resid_tr)) + geom_point() + stat_summary_bin(fun.y='mean', bins=20, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 20)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset_tr, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset_tr, quarters_since_breach >= 0), method = "lm")

orig_lm_tr_nott <- lm(trend_index_company ~ after + after_quarter_interact + factor(gvkey) + factor(datacqtr), data = subset_tr)
stargazer(orig_lm_tr_nott, 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'No')),
          type='text')

###### 
#Advertising section:
subset_mrkt <- comp[!is.na(comp$time_since_breach) & !is.na(comp$xsgaq_log) & !is.infinite(comp$xsgaq_log),]
subset_mrkt$after_quarter_interact <- subset_mrkt$after * subset_mrkt$quarters_since_begin
resid_lm_mrkt <- lm(xsgaq ~ factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_breach, data = subset_mrkt)
resid_lm_mrkt_log <- lm(xsgaq_log ~ factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_breach, data = subset_mrkt)
subset_mrkt$resid_mrkt <- residuals(resid_lm_mrkt)
subset_mrkt$resid_mrkt_log <- residuals(resid_lm_mrkt_log)
ggplot(subset_mrkt, aes(x = quarters_since_breach, y = resid_mrkt)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 2000)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset_mrkt, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset_mrkt, quarters_since_breach > 0), method = "lm")
ggplot(subset_mrkt, aes(x = quarters_since_breach, y = resid_mrkt_log)) + geom_point() + stat_summary_bin(fun.y='mean', bins=50, color='orange', size=4, geom='point') + scale_y_continuous(limits = c(0, 1.5)) + geom_vline(subset, mapping = aes(x = time_since_breach, y = resid_abs), xintercept = 0, color = "blue", size = 2) + geom_smooth(data=subset(subset_mrkt, quarters_since_breach < 0), method = "lm") + geom_smooth(data=subset(subset_mrkt, quarters_since_breach > 0), method = "lm")

orig_lm_mrkt <- lm(xsgaq ~ after + after_quarter_interact + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_breach, data=subset_mrkt)
stargazer(orig_lm_mrkt, 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          add.lines = list(c('Year Fixed Effects:', 'Yes'), c('Company Fixed Effects:', 'Yes'), c('Company specific time trend:', 'Yes')),
          type='text')

# Original specification:
orig_lm_mrkt_tt <- lm(xsgay ~ after + after_quarter_interact + factor(gvkey) + factor(datacqtr) + factor(gvkey)*quarters_since_begin, data = subset_mrkt)
orig_lm_mrkt_nott <- lm(xsgay ~ after + after_quarter_interact + factor(gvkey) + factor(datacqtr), data = subset_mrkt)
stargazer(orig_lm_mrkt_nott, orig_lm_mrkt_tt, 
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin'),
          add.lines = list(c('Year Fixed Effects:', 'Yes', 'Yes'), c('Company Fixed Effects:', 'Yes', 'Yes'), c('Company specific time trend:', 'No', 'Yes')),
          type='text')





######
# Other Controls
window_length = 10
y_scale = 0.1
cont <- 'niq'
subset_test <- comp[!is.na(comp$time_since_breach) & !is.na(comp[[cont]]) & !is.infinite(comp[[cont]]),]
resid_lm_test <- lm(paste(cont, " ~ factor(gvkey) + factor(datacqtr)",sep = ""), data = subset_test)
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
stargazer(felm(revtq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(atq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(actq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(chq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(ltq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(lctq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(uaptq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(dlcq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(xsgaq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(ceqq ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(trend_index_company ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          felm(trend_index_tic ~ after + after_quarter_interact | factor(gvkey) + datacqtr | 0 | datacqtr, data = subset_test),
          dep.var.labels = c('Revenue', 'Net Income', 'Total Assets', 'Current Assets', 'Cash',
                             'Total Liabilities', 'Current Liabilities', 'Accounts Payable', 'Debt', 'Operating Expenses',
                             'Sales, General and Other Expenses', 'Total Shareholders\' Equity',
                             'Google Searches (Company Name)', 'Google Searches (Stock Ticker)'),
          covariate.labels = c('After Breach', 'After Breach x Quarters Since Breach'),
          omit=c('gvkey', 'datacqtr', 'quarters_since_begin', 'Constant'),
          omit.stat = 'ser',
          notes = c('Standard Errors clustered at the quarter level', 'Company and quarter fixed effects in all specifications'),
          out='tables/potential_controls.html',
          type='html')

######
library(eventstudies)
library(reshape2)
crsp = read.csv('data/CRSP_merged_trends.csv')
crsp$RET <- as.numeric(as.character(crsp$RET))
crsp$RETX <- as.numeric(as.character(crsp$RETX))
crsp$Date.Made.Public <- as.Date(crsp$Date.Made.Public, format='%B %d, %Y')
crsp$date <- as.Date(crsp$date)

crsp_match <- subset(crsp, (!is.na(crsp$match)))
crsp <- crsp[crsp$TICKER %in% crsp_match$TICKER,]


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
  crsp[i,'breachdate'] = last_breach
  crsp[i, 'days_since_breach'] = date - last_breach
  
  #print(last_breach)
  #print(company_slice)
  #print(company_slice[company_slice$breachdate == last_breach,])
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




event_list = crsp[!is.na(crsp$Date.Made.Public) & crsp$Date.Made.Public != '',][c('COMNAM','Date.Made.Public')]
names(event_list) <- c('name', 'when')

return_table <- dcast(crsp, date ~ COMNAM, value = 'RET')

es <- eventstudy(firm.returns = crsp,
                 eventList = crsp,
                 width = 10,
                 type = "None",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap")
