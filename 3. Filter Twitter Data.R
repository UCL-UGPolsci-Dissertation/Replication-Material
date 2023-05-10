#Set working directory
setwd("~/Documents/UCL/Year 3/Dissertation/Twitter data/UK tweets")

#Read combined_data file 
data <- read.csv('combined_data.csv')

#Filter data according to selected keywords
filtered_data <- data[grepl('rights', data$tweet) | grepl('Rights', data$tweet) | grepl('RIGHTS', data$tweet) |
               grepl('freedom of', data$tweet) | grepl('Freedom of', data$tweet) | grepl('Freedom Of', data$tweet) | grepl('FREEDOM OF', data$tweet) |
               grepl('civil liberties', data$tweet) | grepl('Civil Liberties', data$tweet) |
               grepl('human right ', data$tweet) | grepl('Human Right ', data$tweet) | grepl('HUMAN RIGHT ', data$tweet) |
               grepl('civil right ', data$tweet) | grepl('political right ', data$tweet) |
               grepl('civil liberty', data$tweet) |
               grepl('United Nations', data$tweet) | grepl('@UNHumanRights', data$tweet) |
                 
               grepl('self-determin', data$tweet) | grepl('Self-determin', data$tweet) | grepl('Self-Determin', data$tweet) | grepl('self determin', data$tweet) | grepl('Self Determin', data$tweet) |
               grepl('oppress', data$tweet) | grepl('Oppress', data$tweet) | grepl('Rights', data$tweet) |
               grepl('political control', data$tweet) | 
               grepl('bodily autonomy', data$tweet) | grepl('Bodily autonomy', data$tweet) | grepl('Bodily Autonomy', data$tweet) | grepl('right to vote', data$tweet) |
               grepl('systemic racism', data$tweet) | grepl('Systemic racism', data$tweet) |
                 
               grepl('domestic remed', data$tweet) |
               grepl('rule of law', data$tweet) | grepl('Rule of law', data$tweet) | grepl('Rule of Law', data$tweet) | grepl('RULE OF LAW', data$tweet) | 
                 
               grepl('discriminat', data$tweet) | grepl('Discriminat', data$tweet) | grepl('DISCRIMINAT', data$tweet) |
               grepl('gender bias', data$tweet) | grepl('Gender bias', data$tweet) | grepl('GENDER BIAS', data$tweet) |
               grepl('gender inequal', data$tweet) | grepl('gender equal', data$tweet) | grepl('Gender equal', data$tweet) | grepl('Gender Equal', data$tweet) |
               grepl('gender equity', data$tweet) |
               grepl('sexism', data$tweet) | grepl('Sexism', data$tweet) |
                 
               grepl('right to life', data$tweet) | grepl('Right to life', data$tweet) | grepl('Right to Life', data$tweet) |
               grepl('extrajudicial killing', data$tweet) | grepl('Extrajudicial Killing', data$tweet) | grepl('extrajudicial execution', data$tweet) |
               grepl('genocide', data$tweet) | grepl('Genocide', data$tweet) | grepl('GENOCIDE', data$tweet) |
               grepl('mass killing', data$tweet) | grepl('Mass Killing', data$tweet) | grepl('mass atrocit', data$tweet) | grepl('Mass atrocit', data$tweet) |
               grepl('crime against humanity', data$tweet) | grepl('crimes against humanity', data$tweet) | grepl('Crimes against humanity', data$tweet) | grepl('Crime against humanity', data$tweet) | grepl('Crimes against Humanity', data$tweet) | grepl('Crimes Against Humanity', data$tweet) |grepl('Crime Against Humanity', data$tweet) | grepl('CRIMES AGAINST HUMANITY', data$tweet) |
               
               grepl('torture', data$tweet) | grepl('Torture', data$tweet) |
               grepl('inhuman treatment', data$tweet) |
               grepl('cruel punishment', data$tweet) |
               grepl('degrading treatment', data$tweet) |
               grepl('bodily integrity', data$tweet) |
               grepl('police brutality', data$tweet) | grepl('Police brutality', data$tweet) | grepl('police violence', data$tweet) | grepl('Police violence', data$tweet) |
               grepl('sexual violence', data$tweet) | grepl('Sexual violence', data$tweet) | grepl('Sexual Violence', data$tweet) |
               grepl('rape', data$tweet) | grepl('Rape', data$tweet) |
                 
                 
               grepl('slavery', data$tweet) | grepl('Slavery', data$tweet) | grepl('SLAVERY', data$tweet) |
               grepl('forced labour', data$tweet) | grepl('forced labor', data$tweet) | grepl('Forced Labor', data$tweet) | 
               grepl('human traffic', data$tweet) | grepl('Human traffic', data$tweet) | grepl('Human Traffic', data$tweet) | grepl('HUMAN TRAFFIC', data$tweet) |
               grepl('child traffic', data$tweet) | grepl('Child Traffic', data$tweet) |
               grepl('exploitation', data$tweet) | grepl('Exploitation', data$tweet) |
                 
               grepl('arbitrary arrest', data$tweet) | grepl('Arbitrary Arrest', data$tweet) | grepl('arbitrarily arrest', data$tweet) | grepl('unlawful arrest', data$tweet) |
               grepl('unlawful detention', data$tweet) | grepl('arbitrary detention', data$tweet) | grepl('Arbitrary Detention', data$tweet) | 
               grepl('fair trial', data$tweet) | grepl('Fair Trial', data$tweet) |
               grepl('due process', data$tweet) | grepl('Due process', data$tweet) |
               grepl('presumption of innocence', data$tweet) |
                 
               grepl('displac', data$tweet) | grepl('Displac', data$tweet) | 
               grepl('deport', data$tweet) | grepl('Deport', data$tweet) | grepl('DEPORT', data$tweet) |
               grepl('right to live', data$tweet) |
                 
               grepl('right to privacy', data$tweet) | 
               grepl('invasion of privacy', data$tweet) | 
               grepl('surveillance state', data$tweet) | 
               grepl('defamation', data$tweet) | grepl('Defamation', data$tweet) |
                 
               grepl('persecution', data$tweet) | grepl('Persecution', data$tweet) |
               grepl('religious freedom', data$tweet) |
               grepl('thought control', data$tweet) | grepl('Thought Control', data$tweet) |
                 
               grepl('censorship', data$tweet) | grepl('Censorship', data$tweet) | grepl('CENSORSHIP', data$tweet) | grepl('censur', data$tweet) | grepl('CENSUR', data$tweet) |
               grepl('media control', data$tweet) | grepl('control of the press', data$tweet) |
               grepl('repress', data$tweet) | grepl('Repress', data$tweet) |
                 
               grepl('protest ban', data$tweet) | grepl('protest bans', data$tweet) | grepl('Protest ban', data$tweet) |
               grepl('right to protest', data$tweet) | grepl('Right to protest', data$tweet) | grepl('Right to Protest', data$tweet) |
               grepl('strike ban', data$tweet) | grepl('right to strike', data$tweet) |
               
               grepl('forced marriage', data$tweet) | grepl('Forced marriage', data$tweet) |
               grepl('child bride', data$tweet) |
                 
               grepl('voting right ', data$tweet) | grepl('right to vote', data$tweet) |
               grepl('right to run', data$tweet) | grepl('right to be represented', data$tweet) | grepl('right to representation', data$tweet), ]


#Only select relevant variables
tweets <- filtered_data
tweets <- tweets[, c('created_at', 'id', 'tweet')]

#Change tweets data format 
tweets$created_at <- strptime(tweets$created_at, "%a %b %d %H:%M:%S %z %Y")
tweets$date <- format(tweets$created_at, "%d %b %Y")

#Reorganise the order of the columns
tweets <- cbind(tweets[, c("id", "created_at", "date", "tweet")])

#Write csv
write.csv(tweets, file = "tweets.csv", row.names = FALSE)
