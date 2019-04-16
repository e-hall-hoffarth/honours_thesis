setwd('/mnt/good/honours_thesis/')
library(stargazer)

comp = read.csv('data/COMPUSTAT_vars.csv')
crsp = read.csv('data/CRSP_cleaned_mar.csv')
breaches = read.csv('data/data_breaches_final.csv')
crsp$RETX <- as.numeric(as.character(crsp$RETX))
crsp$RET <- as.numeric(as.character(crsp$RET))

comp_match = subset(comp, (!is.na(comp$match)))
crsp_match = subset(crsp, (!is.na(crsp$match)))
breaches = breaches[breaches$match == 1,]
breaches = breaches[breaches$GVKEY %in% comp$gvkey,]

# Table 1A: Compustat Firm variables
stargazer(comp[c('niq','revtq','xoprq','nopiq','trend_index_company', 'trend_index_tic')],
          covariate.labels = c('Net income (Millions USD)',
                               'Revenue (Millions USD)',
                               'Operation Expenses (Millions USD)',
                               'Non Operating Income (Net NO Expenses) (Millions USD)',
                               'Google Trends Index (Company Name)',
                               'Google Trends Index (Company Ticker)'),
          summary.stat = c('n', 'mean', 'sd'),
          digits = 1,
          title = 'Table 1A: Summary Statistics for Fincial Variables',
          type='latex')

# Table 1B: Loss Type
customer = sum(breaches[breaches$customer == 1,]$customer)
credit_card = sum(breaches[breaches$credit_card == 1,]$credit_card)
social_security_number = sum(breaches[breaches$social_security == 1,]$social_security)
name = sum(breaches[breaches$name == 1,]$name)
address = sum(breaches[breaches$address == 1,]$address)
total_breaches = sum(breaches[breaches$match == 1,]$match)
records = breaches$Total.Records
mean_records = mean(records)
loss_type <- data.frame(customer, credit_card, social_security_number, name, address, total_breaches)
stargazer(loss_type,
          covariate.labels = c('Customer', 'Credit Card', 'Social Security Number', 'Name','Address', 'Total Breaches'),
          digits = 0,
          title = 'Table 1B: Types of Data Loss',
          summary.stat = c('mean'),
          type='latex')

# Table 1C: Magnitude of breaches
stargazer(comp_match[c('Total.Records')],
          covariate.labels = c('Records leaked per breach'),
          digits = 0,
          title = 'Table 1C: Magnitude of Data Loss',
          summary.stat = c('n', 'mean', 'min', 'max', 'sd'),
          type='latex')

# Table 1D: CRSP Firm Variables
stargazer(crsp[c('RETX','vwretx','Mkt.RF', 'SMB', 'HML')],
          covariate.labels = c('Daily Firm Return', 'Value Weighted Market Return', 'Risk Free Market Return', 'SMB factor', 'HML factor'),
          digits = 2,    
          summary.stat = c('n', 'mean', 'sd'),
          title = 'Table 1D: Summary Stock Market Data',
          type='latex')

# Table 1E: Controls
stargazer(comp[c('nominal_gdp', 'real_gdp_billions', 'inflation_rate_yoy')],
          covariate.labels = c('Nominal GDP (Billions USD)', 'Real GDP (index 2012)', 'Inflation (% yoy)'),
          summary.stat = c('n', 'mean', 'sd'),
          digits = 1,
          title = 'Table 1E: Summary Statisitics for Control Variables',
          type='html')

# Figure 1: Distribution of records leaked
hist(log(comp_match$Total.Records), 
     xlab='ln(Records Leaked)',
     ylab='Count', 
     main='Histogram of Records Leaked per Data Breach (Natural Log)',
     cex=1.5)


