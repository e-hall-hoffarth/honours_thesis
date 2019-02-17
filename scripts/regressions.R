library(stargazer)
library(lfe)
library(zoo)

# crsp = read.csv('data/CRSP_merged_trends.csv')
# crsp$RET <- as.numeric(as.character(crsp$RET))
# crsp$RETX <- as.numeric(as.character(crsp$RETX))
# crsp_match = subset(crsp, (!is.na(crsp$match)))

comp = read.csv('data/COMPUSTAT_merged_trends.csv')
comp$datadate = as.Date(comp$datadate)
comp$breachdate = as.Date(comp$Date.Made.Public, format="%Y-%m-%d", optional=T)
comp_match = subset(comp, (!is.na(comp$match)))
comp$treat <- ifelse(comp$gvkey %in% comp_match$gvkey,1,0)
comp$quarters_since_begin <- (as.yearqtr(comp$datacqtr, format('%YQ%q')) - as.yearqtr('2005 Q1'))*4

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
comp$diffindiff <- comp$treat * comp$after

comp$customer_interact <- comp$customer * comp$diffindiff
comp$employee_interact <- comp$employee * comp$diffindiff
comp$customer_interact <- comp$customer * comp$diffindiff
comp$credit_card_interact <- comp$credit_card * comp$diffindiff
comp$cvv_interact <- comp$cvv * comp$diffindiff
comp$social_security_interact <- comp$social_security * comp$diffindiff
comp$name_interact <- comp$name * comp$diffindiff
comp$address_interact <- comp$address * comp$diffindiff
comp$personal_information_interact <- comp$personal_information * comp$diffindiff
comp$Total.Records_interact <- comp$Total.Records * comp$diffindiff

nofe_model <- felm(revtq ~ after + diffindiff + G(gvkey), data=comp)
base_model <- felm(revtq ~ diffindiff + G(gvkey*quarters_since_begin) + G(gvkey) + G(datacqtr), data=comp[!is.na(comp$quarters_since_begin),])
types_model <- felm(revtq ~ diffindiff + G(datacqtr) + G(gvkey) + customer_interact + employee_interact + credit_card_interact + cvv_interact + social_security_interact + name_interact + address_interact + personal_information_interact, data=comp)
controls_model <- felm(revtq ~ diffindiff + Total.Records_interact + G(datacqtr) + G(gvkey), data=comp)
stargazer(nofe_model, base_model, controls_model, types_model,
          omit=c('datacqtr', 'gvkey', 'sic'), 
          add.lines = list(c("Year Fixed Effects:", "No", "Yes", "Yes", "Yes"), c("Company Fixed Effects:", "Yes", "Yes", "Yes", "Yes")),
          type='text')
