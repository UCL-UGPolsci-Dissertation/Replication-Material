#Set working directory
setwd("~/Documents/UCL/Year 3/Dissertation")

#Load all newspaper datasets
news1 <- read.csv('Datasets/newsdata1.csv')
news2 <- read.csv('Datasets/newsdata2.csv')
news3 <- read.csv('Datasets/newsdata3.csv')
guardian <- read.csv('Datasets/guardian_news.csv')

#Aggregate the first three datasets
news <- rbind(news1, news2)
news <- rbind(news, news3)

#For all dates that do not contain 2022, assign 31 Dec 2021
for (i in 1:length(news$Date)) {
  if (!grepl('2022', news$Date[i])) {
    news$Date[i] <- '31 Dec 2021'
  } else if (is.na(news$Date[i])) {
    news$Date[i] <- '31 Dec 2021'
  }
}

#Transform guardian date variable to match the others
guardian$first_publication_date <- as.POSIXlt(guardian$first_publication_date, format = "%Y-%m-%d %H:%M:%S")
guardian$first_publication_date <- format(guardian$first_publication_date, "%d %b %Y")

#Rename columns 
names(guardian) <- c('title', 'Source', 'Date', 'body')

#Rbind the guardiand dataset to the overall news dataset
news <- rbind(news, guardian)

#Save as csv
write.csv(news, 'newsdata.csv', row.names = FALSE)
