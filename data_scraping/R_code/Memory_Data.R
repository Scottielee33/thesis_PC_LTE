if (!require("rvest")) install.packages("rvest")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")

computeData <- TRUE

if (computeData) {
  url <- "https://www.memorybenchmark.net/threaded_ddr5.html"
  page <- read_html(url)
  
  DDR5_names <- page %>%
    html_nodes(".prdname") %>%
    html_text()
  
  memorylink <- page %>%
    html_nodes("#mark a") %>%
    html_attr("href") %>% paste0("https://www.memorybenchmark.net/",.)
  memorylink <- as.matrix(memorylink)
  
  # Convert the vector into a matrix with 3 columns
  matrix_form <- matrix(DDR5_names, ncol = 1, byrow = TRUE)
  
  # Convert the matrix into a dataframe
  DDR5_df <- as.data.frame(matrix_form)
  DDR5_df$Link <- unlist(memorylink)
  
  # Optionally, set column names
  colnames(DDR5_df) <- c("Memory", "Link")
  
  release_dates <- vector("list", length = nrow(DDR5_df))
  scores <- vector("list", length = nrow(DDR5_df))
  current_price <- vector("list", length = nrow(DDR5_df))
  
  for (i in 1:nrow(DDR5_df)) {
    # Construct the URL for the GPU's page
    DDR5_url <- DDR5_df$Link[i]
    
    # Scrape the GPU's page
    DDR5_page <- read_html(DDR5_url)
    
    # Extract the specific information you're interested in
    # Note: Replace the '.selector' with the actual CSS selector of the data you want
    DDR5_detail <- DDR5_page %>%
      html_nodes('p+ .alt , .speedicon+ span , p~ .alt+ p') %>%
      html_text()
    
    # Store the extracted detail in the list
    score_temp <- as.numeric(DDR5_detail[3])
    current_price_temp <- str_trim(unlist(strsplit(DDR5_detail[2], ":"))[2])
    release_date_temp <- str_trim(unlist(strsplit(DDR5_detail[1], ":"))[2])
    
    release_dates[[i]] <- release_date_temp
    scores[[i]] <- score_temp
    current_price[[i]] <- current_price_temp
    
    # Optional: Print progress
    cat("Scraped data for", DDR5_df$Memory[i], "\n")
  }
  
  DDR5_df$ReleaseDates <- unlist(release_dates)
  DDR5_df$Scores <- unlist(scores)
  DDR5_df$CurrentPrice <- unlist(current_price)
  DDR5_df$Type <- "DDR5"
  
  
  url <- "https://www.memorybenchmark.net/threaded_ddr4.html"
  page <- read_html(url)
  
  DDR4_names <- page %>%
    html_nodes(".prdname") %>%
    html_text()
  
  memorylink <- page %>%
    html_nodes("#mark a") %>%
    html_attr("href") %>% paste0("https://www.memorybenchmark.net/",.)
  memorylink <- as.matrix(memorylink)
  
  # Convert the vector into a matrix with 3 columns
  matrix_form <- matrix(DDR4_names, ncol = 1, byrow = TRUE)
  
  # Convert the matrix into a dataframe
  DDR4_df <- as.data.frame(matrix_form)
  DDR4_df$Link <- unlist(memorylink)
  
  # Optionally, set column names
  colnames(DDR4_df) <- c("Memory", "Link")
  
  release_dates <- vector("list", length = nrow(DDR4_df))
  scores <- vector("list", length = nrow(DDR4_df))
  
  for (i in 1:nrow(DDR4_df)) {
    # Construct the URL for the GPU's page
    DDR4_url <- DDR4_df$Link[i]
    
    # Scrape the GPU's page
    DDR4_page <- read_html(DDR4_url)
    
    # Extract the specific information you're interested in
    # Note: Replace the '.selector' with the actual CSS selector of the data you want
    DDR4_detail <- DDR4_page %>%
      html_nodes('p+ .alt , .speedicon+ span , p~ .alt+ p') %>%
      html_text()
    
    # Store the extracted detail in the list
    score_temp <- as.numeric(DDR4_detail[3])
    current_price_temp <- str_trim(unlist(strsplit(DDR4_detail[2], ":"))[2])
    release_date_temp <- str_trim(unlist(strsplit(DDR4_detail[1], ":"))[2])
    
    release_dates[[i]] <- release_date_temp
    scores[[i]] <- score_temp
    current_price[[i]] <- current_price_temp
    
    # Optional: Print progress
    cat("Scraped data for", DDR4_df$Memory[i], "\n")
  }
  
  DDR4_df$ReleaseDates <- unlist(release_dates)
  DDR4_df$Scores <- unlist(scores)
  DDR4_df$CurrentPrice <- unlist(current_price)
  DDR4_df$Type <- "DDR4"
  
  print(DDR5_df)
  
  memory_df <- rbind(DDR4_df, DDR5_df)
  
  memory_df$ReleaseDate <- as.Date(memory_df$ReleaseDate, format = "%Y-%m-%d")
  cleaned_data_memory <- memory_df[!is.na(memory_df$ReleaseDate), ]
  
  cleaned_data_memory <- cleaned_data_memory %>% filter(CurrentPrice != "NA")
  
  cleaned_data_memory$Quarters <- quarters(cleaned_data_memory$ReleaseDate)
  cleaned_data_memory$Year <- format(cleaned_data_memory$ReleaseDate, "%Y")
  
  Memory_df <- cleaned_data_memory
  Memory_df$ReleaseDate <- NULL
  
  Memory_df <- Memory_df %>% 
    mutate(CurrentPrice = str_extract(CurrentPrice, "\\d+\\.\\d+"))
  
  Memory_df <- Memory_df %>% filter(CurrentPrice != "NA")
  
  write.csv(cleaned_data_memory, file = "Component_data/memory_data.csv", row.names = FALSE)
} else {
  Memory_df <- read.csv(file = "Component_data/memory_data.csv")
}
