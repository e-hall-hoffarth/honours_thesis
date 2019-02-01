library(stargazer)

comp = read.csv('data/COMPUSTAT_merged.csv')
crsp = read.csv('data/CRSP_merged.csv')
crsp$RETX <- as.numeric(as.character(crsp$RETX))
crsp$RET <- as.numeric(as.character(crsp$RET))

comp_match = subset(comp, (!is.na(comp$match)))
crsp_match = subset(crsp, (!is.na(crsp$match)))

# Table 1A: Compustat Firm variables
stargazer(comp[c('niq','revtq','xsgay','mkvaltq')],
          covariate.labels = c('Net income (Millions USD)','Revenue (Millions USD)','Sales, General, and Administrative Expenses (Millions USD)', 'Market Value (Millions USD)'),
          summary.stat = c('n', 'mean', 'sd'),
          digits = 1,
          title = 'Table 1A: Summary Statistics for Fincial Variables',
          type='html')

# Table 1B: Loss Type
customer = sum(comp_match[comp_match$customer == 1,]$customer)
employee = sum(comp_match[comp_match$employee == 1,]$employee)
credit_card = sum(comp_match[comp_match$credit_card == 1,]$credit_card)
cvv = sum(comp_match[comp_match$cvv == 1,]$cvv)
social_security_number = sum(comp_match[comp_match$social_security == 1,]$social_security)
name = sum(comp_match[comp_match$name == 1,]$name)
address = sum(comp_match[comp_match$address == 1,]$address)
personal_information = sum(comp_match[comp_match$personal_information == 1,]$personal_information)
total_breaches = sum(comp_match[comp_match$match == 1,]$match)
records = comp[!is.na(comp$match),]$Total.Records
mean_records = mean(records)
loss_type <- data.frame(customer, employee, credit_card, cvv, social_security_number, name, address, personal_information, total_breaches)
stargazer(loss_type,
          covariate.labels = c('Customer', 'Employee', 'Credit Card', 'CVV', 'Social Security Number', 'Name','Address', 'Personal Information', 'Total Breaches'),
          digits = 0,
          title = 'Table 1B: Types of Data Loss',
          summary.stat = c('mean'),
          type='html')

# Table 1C: Magnitude of breaches
stargazer(comp_match[c('Total.Records')],
          covariate.labels = c('Records leaked per breach'),
          digits = 0,
          title = 'Table 1C: Magnitude of Data Loss',
          summary.stat = c('n', 'mean', 'min', 'max', 'sd'),
          type='html')

# Table 1D: CRSP Firm Variables
stargazer(crsp[c('RETX','vwretx','Mkt.RF', 'SMB', 'HML')],
          covariate.labels = c('Daily Firm Return', 'Value Weighted Market Return', 'Risk Free Market Return', 'SMB factor', 'HML factor'),
          digits = 2,    
          summary.stat = c('n', 'mean', 'sd'),
          title = 'Table 1D: Summary Stock Market Data',
          type='html')

# Table 1E: Controls
stargazer(comp[c('nominal_gdp', 'real_gdp_billions', 'inflation_rate_yoy')],
          covariate.labels = c('Nominal GDP (Billions USD)', 'Real GDP (index 2012)', 'Inflation (% yoy)'),
          summary.stat = c('n', 'mean', 'sd'),
          digits = 1,
          title = 'Table 1E: Summary Statisitics for Control Variables',
          type='html')

# Figure 1: Distribution of records leaked
hist(log(comp_match$Total.Records), xlab='ln(Records Leaked)', ylab='Count', main='Figure 1: Histogram of Records Leaked per Data Breach (Natural Log)')


