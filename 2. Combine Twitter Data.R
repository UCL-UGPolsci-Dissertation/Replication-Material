combine_data <- function(working_directory) {

#Read and combine all day datasets
  setwd(working_directory)
  
  all_files <- list.files()
  datasets <- list()

  for (i in seq_along(all_files)) {
    var_name <- paste0('data', substr(all_files[i], 14, 17))
    data <- assign(var_name, read.csv(all_files[i]))
    datasets <- append(datasets, list(data))
  }

#Merge all day datasets
combined_data <- do.call(rbind, datasets)
} 

combined_data <- combine_data("~/Documents/UCL/Year 3/Dissertation/Twitter data/UK tweets")

#Save as csv file
write.csv(combined_data, file = "combined_data.csv", row.names = FALSE)

