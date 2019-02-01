library(ggplot2)
library(reshape2)
library(dplyr)
setwd("~/Documents/ECON495/data_breach/")
data <- read.csv("Privacy_Rights_Clearinghouse-Data-Breaches-Export.csv", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE)
states <- read.csv("states.csv")
states$region <- tolower(states$region)
names(data) <- tolower(names(data))
USdata <- data[tolower(data$state) %in% unique(states$region),]
US_Firm_data <- USdata[USdata$type.of.organization %in% c("BSF", "BSR", "BSO"),]

by_year <- data.frame(year=integer(), total_breaches=integer(), total_records=integer())
for(year in na.omit(unique(US_Firm_data$year.of.breach))) {
  temp <- data[US_Firm_data$year.of.breach == year,]
  total_breaches <- nrow(temp)
  total_records <- sum(lapply(temp$total.records, FUN = function(x) as.numeric(sub(",", "", x, fixed=T))))
  
  by_year <- rbind(by_year, list(year, total_breaches, total_records))
}
names(by_year) <- c("Year", "Total Breaches", "Total Records Exposed")

by_firm <- data.frame(firm=character(), total_breaches=integer(), total_records=integer(), stringsAsFactors = F)
for(firm in na.omit(US_Firm_data$company)){
  temp <- US_Firm_data[US_Firm_data$company == firm,]
  total_breaches <- nrow(temp)
  total_records <- sum(as.numeric(na.omit(temp$total.records)))
  
  by_firm <- rbind(by_firm, list(as.character(firm), total_breaches, total_records))
} 
names(by_firm) <- c("Firm", "Total Breaches", "Total Records Exposed")

ggplot(by_year, aes(x = Year, y = `Total Records Exposed`)) +
  geom_line() + 
    ggtitle("Figure 2") +
      geom_smooth(method = lm, color="red") + 
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1), 
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              plot.title = element_text(size=20, hjust=0.5)) +
          scale_x_continuous(breaks = pretty(by_year$Year, n = 10)) + 
            scale_y_continuous(breaks = pretty(by_year$`Total Records Exposed`, n = 4)) +
              annotate("text", label = "Source: Privacy Rights Clearinghouse", x = 2015, y = 200000, size = 6)

ggsave("total_records_exposed.png")

ggplot(by_year, aes(x = Year, y = `Total Breaches`)) +
  geom_line() +
   ggtitle("Figure 1") +
    geom_smooth(method = lm, color="red") +
      theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1), 
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.title.y = element_text( margin = margin(t = 0, r = 20, b = 0, l = 0)),
            plot.title = element_text(size=20, hjust=0.5)) +
        scale_x_continuous(breaks = pretty(by_year$Year, n = 10)) + 
          scale_y_continuous(breaks = pretty(by_year$`Total Breaches`, n = 4)) +
            annotate("text", label = "Source: Privacy Rights Clearinghouse", x = 2015, y = 200, size = 6)

ggsave("total_breaches.png")


ggplot(melt(US_Firm_data), aes(value, fill = ))
