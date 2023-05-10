setwd("~//Downloads/20220131")

get_data <- function(x) {
  
  
  ##STEP 1: Get Twitter data for one file within a day
  #Load library jsonlite
  library(jsonlite)
  
  #Create connection with file x
  con <- file(x, 'r')
  
  #Extract data
  json_data <- stream_in(con)
  
  #Close connection
  close(con)
  
  ##STEP 2: Subset Twitter data
  
  #Remove deleted tweets
  json_data <- json_data[!is.na(json_data$created_at),]
  
  #Extract date of creation data
  created_at <- json_data$created_at
  
  #Extract tweet ID
  id <- json_data$id
  
  #Create an empty vector which equates the number of tweets
  tweet <- vector(length = sum(nrow(json_data)))
  
  #Get full tweets
  #Per tweet
  for (i in 1:nrow(json_data)) {
    
    #If the full_text is already included in the main text variable, extract the main text variable
    if (is.na(json_data$extended_tweet$full_text[i])) {
      
      tweet[i] <- json_data$text[i]
      
      #Otherwise, extract the full_text data
    } else {
      
      tweet[i] <- json_data$extended_tweet$full_text[i]
      
    }
    
  }
  
  #Extract the username of tweet's author
  screen_name <- json_data$user$screen_name
  
  #Extract user's location
  location <- json_data$user$location
  
  #Extract the language variable
  language <- json_data$lang
  
  ##Combine data and subset it to only include relevant tweets
  #Bind all variables into one matrix
  data <- cbind(created_at, id, tweet, screen_name, location, language)
  
  #Only include tweets in Engish
  data <- data[language == 'en',]
  
  #Save data as a dataframe
  data <- as.data.frame(data)
  
  #Select tweets posted in the UK
  data <- data[grepl('London', data$location) |
                 
                 grepl('UK', data$location) |
                 
                 grepl('United Kingdom', data$location) |
                 
                 grepl('Scotland', data$location) |
                 
                 grepl('Wales', data$location) |
                 
                 grepl('England', data$location) |
                 
                 grepl('Northern Ireland', data$location), ]
}

#Get list of all files' name within the directory
all_files <- list.files()



#Filter the list to only include files that match data's pattern
file_names <- all_files[grep("^\\d{14}\\.json\\.gz$", all_files)]



# Loop over the file names to extract the data for each file
for (i in seq_along(file_names)) {
  
  #Create a variable name based on the file name
  var_name <- paste0('data', substr(file_names[i], 1, 14))
  
  # Get the data and assign it to a variable with the variable name
  assign(var_name, get_data(file_names[i]))
  
}


#
data_frames <- mget(ls(pattern = "data\\d+"))

# Combine the data frames
combined_data <- do.call(rbind, data_frames)

#Write a csv file which includes the data for analysis for one day
write.csv(combined_data, file = "combined_data0131.csv", row.names = FALSE)
