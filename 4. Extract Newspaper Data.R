setwd("~/Downloads")

library(pdftools)
library(stringr)

test <- pdf_text('PDF(1000).PDF')

##Extract Articles
# Define the end of document string
end_of_doc <- 'End of Document'

# Concatenate character vectors within the list into a single character vector
pdf_text_concat <- paste(test, collapse = "\n")

# Split text based on end of document string using strsplit
split_text <- strsplit(pdf_text_concat, end_of_doc, fixed = TRUE)

# Extract individual elements as a list of character vectors
elements_list <- lapply(split_text, function(x) trimws(unlist(x)))

# Convert list of character vectors to a single character vector
elements <- unlist(elements_list)

#Extract article's content
extracted_body <- lapply(elements, function(x) sub("(?s)(?i)^.*Body\\s{2,}(.*?)Classification.*$", "\\1", x, perl = TRUE))
extracted_body <- extracted_body[-length(extracted_body)]
##

##Extract the title, date and source
headlines <- pdf_text('Headlines2.PDF')
headlines_concat <- paste(headlines, collapse = "\n")

title <- unlist(strsplit(headlines_concat, "\n(?=\\s*\\d+\\.)", perl = TRUE))
title <- title[-1]
title <- title[lapply(title, function(x) nchar(trimws(x))) > 1]
title <- as.data.frame(title)

for (i in 1:length(title$title)) {
  
  if (grepl('Independent', title$title[i])) {
    title$Source[i] <- 'Independent';
  }
  
  else {
    title$Source[i] <- 'Times'
  }
}

title$title <- sub("(.*2022)(.*)", "\\1", title$title)

title$Date <- str_extract(title$title, "\\d+ \\w+ \\d+")

title$title <- sub("\\d+ \\w+ \\d+", "", title$title)

title$title <- gsub("^\\d+\\.", "", title$title)
title$title <- gsub("^[[:space:]]*\\d+\\.", "", title$title)
##

##Merge both datasets
body <- unlist(extracted_body)
body <- as.data.frame(body)

newsdata2 <- cbind(title, body)
write.csv(newsdata2, file = "newsdata2.csv", row.names = FALSE)
