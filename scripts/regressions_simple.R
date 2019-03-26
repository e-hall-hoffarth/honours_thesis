setwd('/mnt/good/honours_thesis/')

library(stargazer)
library(ggplot2)
library(lfe)
library(zoo)
library(lubridate)

comp <- read.csv('data/COMPUSTAT_vars.csv')

######
# Regressions
# Revenue
subset <- comp[!is.na(comp$quarters_since_breach) &
                 !is.na(comp$log_revtq) &
                 !is.infinite(comp$log_revtq) &
                 comp$quarters_since_breach %in% -40:8,]
abs_mean = round(mean(subset$revtq),2)
abs_sd = round(sd(subset$revtq),2)
# Absolute Revenue:
stargazer(felm(revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(revtq ~ after + after_quarter_interact_b | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          felm(revtq ~ after + after_quarter_interact_b + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(revtq ~ after + after_quarter_interact_b + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(revtq ~ after + after_quarter_interact_b + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Revenue'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', abs_mean, abs_mean, abs_mean, abs_mean, abs_mean),
                           c('Dependant SD', abs_sd, abs_sd, abs_sd, abs_sd, abs_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 3 years after'),
          #out = 'tables/revenue_specification3_3y.html',
          type='text')

log_mean = round(mean(subset$log_revtq),2)
log_sd = round(sd(subset$log_revtq),2)
# Log Revenue:
stargazer(felm(log_revtq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(log_revtq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          felm(log_revtq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(log_revtq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Revenue (Log)'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', log_mean, log_mean, log_mean, log_mean),
                           c('Dependant SD', log_sd, log_sd, log_sd, log_sd)),
          notes = c('Standard Errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years months before breach, and event period up to 3 years after'),
          out = 'tables/logrevenue_specification3_3y.html',
          type='html')

# Profit
subset_pr <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$niq) & 
                    !is.infinite(comp$niq) &
                    comp$quarters_since_breach %in% -40:8,]
pr_mean <- round(mean(subset_pr$niq),2)
pr_sd <- round(sd(subset_pr$niq), 2)
stargazer(felm(niq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(niq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          felm(niq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(niq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(niq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Net Income'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', pr_mean, pr_mean, pr_mean, pr_mean, pr_mean),
                           c('Dependant SD', pr_sd, pr_sd, pr_sd, pr_sd, pr_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 3 years after'),
          out = 'tables/profit_specification3_3y.html',
          type='html')

# Expenses
subset_xopr <- comp[!is.na(comp$quarters_since_breach) &
                    !is.na(comp$xoprq) & 
                    !is.infinite(comp$xoprq) &
                    comp$quarters_since_breach %in% -40:8,]
xopr_mean <- round(mean(subset_pr$xoprq),2)
xopr_sd <- round(sd(subset_pr$xoprq), 2)
stargazer(felm(xoprq ~ after | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(xoprq ~ after + after_quarter_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset), 
          felm(xoprq ~ after + after_quarter_interact + Total.Records_interact_log +  trend_index_company + trend_index_company_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(xoprq ~ after + after_quarter_interact + rq1_interact + rq2_interact + rq3_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          felm(xoprq ~ after + after_quarter_interact + customer_interact + credit_card_interact + social_security_interact + name_interact + address_interact | factor(gvkey) + factor(quarters_since_begin) | 0 | gvkey, data = subset),
          covariate.labels = c('After Breach', 'After Breach x Quarter', 'Records Leaked (log) x After Breach', 'Google Search Index', 'Google Search Index x After Breach', 'After x Revenue Quartile 1','After x Revenue Quartile 2','After x Revenue Quartile 3', 'Customer Data Leaked x After breach', 'Credit Card Leaked x After breach', 'SSN Leaked x After breach', 'Name Leaked x After breach', 'Address Leaked x After breach'),
          dep.var.labels = c('Operating Expenses'),
          omit.stat = 'ser',
          add.lines = list(c('Dependant Mean', xopr_mean, xopr_mean, xopr_mean, xopr_mean, xopr_mean),
                           c('Dependant SD', xopr_sd, xopr_sd, xopr_sd, xopr_sd, xopr_sd)),
          notes = c('Standard errors clustered at the company level', 
                    'Company and quarter fixed effects in all specifications', 
                    'Prediction period is up to 10 years before breach, and event period up to 3 years after'),
          out = 'tables/expense_specification3_3y.html',
          type='html')

#####
# Plotting
# Absolute Revenue
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
        axis.title=element_text(size=12, face='bold', family='serif'))
ggsave('tables/mean_resid_revenue_2y.png')

# Log Revenue
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
        axis.title=element_text(size=12, face='bold', family='serif'))
ggsave('tables/mean_resid_logrevenue_2y.png')

# Profit
ggplot(subset_pr, aes(x = quarters_since_breach, y = resid_pr_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=100, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-10, 10)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_pr, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  ggtitle('Mean Residual Profit (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif'))
ggsave('tables/mean_resid_profit_2y.png')

# Expenses
ggplot(subset_xopr, aes(x = quarters_since_breach, y = resid_xoprq_nott)) + 
  geom_point(alpha=0, color='grey') + 
  stat_summary_bin(fun.y='mean', color='black', bins=100, geom='point', alpha=0.5, size=2) + 
  scale_y_continuous(limits = c(-40, 40)) + 
  geom_vline(xintercept = 0, color = "orange", size = 2, alpha = 0.5) + 
  geom_smooth(data=subset(subset_xopr, quarters_since_breach <= 0), method = "lm", size=1, color='blue', se=F) + 
  geom_smooth(data=subset(subset_xopr, quarters_since_breach >= 0), method = "lm", size=1, color='blue', se=F) +
  xlab('Quarters Since Breach') +
  ylab('') +
  ggtitle('Mean Residual Operating Expenses (Fixed Effects Removed)') +
  theme(plot.title = element_text(hjust = 0.5, size=24, face='bold', family="serif"),
        axis.text=element_text(size=12, face='bold', family='serif'),
        axis.title=element_text(size=12, face='bold', family='serif'))
ggsave('tables/mean_resid_xoprq_2y.png')
