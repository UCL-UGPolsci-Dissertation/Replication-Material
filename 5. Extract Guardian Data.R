#Set working directory 
setwd("~/Documents/UCL/Year 3/Dissertation")

#Load Guardian API package
library('guardianapi')

#Set API Key
gu_api_key(check_env = FALSE)

#Get newspaper data
guardian_news_og <- gu_content(query = '"human right" OR "human rights" OR "civil rights" OR "civil right" OR "political rights" OR "political right"', 
                from_date = '2021-12-31', to_date = '2022-02-01')

#Only select articles
guardian_news <- guardian_news_og[guardian_news_og$type == 'article',]

#Only select a few variables
guardian_news <- guardian_news[, c('web_title', 'first_publication_date', 'body_text')]

#Create a new column which contains Guardian
guardian_news$source <- 'Guardian'

#Reorganise the order of the columns
guardian_news <- cbind(guardian_news[, c("web_title", "source", "first_publication_date", "body_text")])

#Save as csv
write.csv(guardian_news, 'guardian_news.csv', row.names = FALSE)

