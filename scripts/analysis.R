library(ggplot2)
library(reshape2)
library(dplyr)

human_numbers <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}
human_num <- function(x){human_numbers(x, smbl = "")} 

setwd("/mnt/good/honours_thesis")
data <- read.csv("data/data_breaches_final.csv", stringsAsFactors = FALSE, strip.white = TRUE, header = TRUE)
states <- read.csv("data/states.csv")
states$state <- tolower(states$State)
names(data) <- tolower(names(data))
data <- data[data$gvkey != 62634,]
USdata <- data[tolower(data$state) %in% unique(states$state) & data$match == 1,]
by_year <- data.frame(year=integer(), total_breaches=integer(), total_records=integer())

for(year in na.omit(unique(USdata$year.of.breach))) {
  temp <- USdata[USdata$year.of.breach == year,]
  total_breaches <- nrow(temp)
  total_records <- sum(temp$total.records)
  by_year <- rbind(by_year, list(year, total_breaches, total_records))
}
names(by_year) <- c("Year", "Total Breaches", "Total Records Exposed")

by_firm <- data.frame(firm=character(), total_breaches=integer(), total_records=integer(), stringsAsFactors = F)
for(firm in na.omit(USdata$company)){
  temp <- USdata[USdata$company == firm,]
  total_breaches <- nrow(temp)
  total_records <- sum(as.numeric(na.omit(temp$total.records)))
  
  by_firm <- rbind(by_firm, list(as.character(firm), total_breaches, total_records))
} 
names(by_firm) <- c("Firm", "Total Breaches", "Total Records Exposed")

ggplot(by_year, aes(x = Year, y = `Total Records Exposed`)) +
  geom_line() + 
    ggtitle("Records Leaked by Year") +
    xlab('') +
    ylab('') +
      geom_smooth(method = lm, color="blue", se=F) + 
        theme(text = element_text(size=20, face='bold', family="serif"), 
              axis.text.x = element_text(angle=90, hjust=1, face='bold', family="serif"), 
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
              plot.title = element_text(size=24, hjust=0.5, face='bold', family="serif")) +
          scale_x_continuous(breaks = pretty(by_year$Year, n = 10)) + 
            scale_y_continuous(breaks = pretty(by_year$`Total Records Exposed`, n = 4), labels = human_num)
      
ggsave("tables/total_records_exposed.png")

ggplot(by_year, aes(x = Year, y = `Total Breaches`)) +
  geom_line() +
   ggtitle("Figure 1") +
    geom_smooth(method = lm, color="blue", se=F) +
      theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1), 
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.title.y = element_text( margin = margin(t = 0, r = 20, b = 0, l = 0)),
            plot.title = element_text(size=20, hjust=0.5)) +
        scale_x_continuous(breaks = pretty(by_year$Year, n = 10)) + 
          scale_y_continuous(breaks = pretty(by_year$`Total Breaches`, n = 5)) +
            annotate("text", label = "Source: Privacy Rights Clearinghouse", x = 2015, y = 20, size = 6)

ggsave("total_breaches.png")

