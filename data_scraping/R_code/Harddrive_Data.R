if (!require("rvest")) install.packages("rvest") 
if (!require("stringr")) install.packages("stringr")

computeData <- TRUE

if (computeData){
  url <- "https://www.harddrivebenchmark.net/hdd_value.html"
  page <- read_html(url)
  
  Harddrive_names_scores <- page %>%
    html_nodes(".price-neww , .publift-widget-22645135309-button , .count , .prdname") %>%
    html_text()
  
  # Convert the vector into a matrix with 3 columns
  matrix_form <- matrix(Harddrive_names_scores, ncol = 3, byrow = TRUE)
  
  # Convert the matrix into a dataframe
  Harddrive_df <- as.data.frame(matrix_form)
  
  # Optionally, set column names
  colnames(Harddrive_df) <- c("Name", "Score", "CurrentPrice")
  Harddrive_df$Score <- NULL
  release_dates <- vector("list", length = nrow(Harddrive_df))
  current_prices <- vector("list", length = nrow(Harddrive_df))
  urls <- vector("list", length = nrow(Harddrive_df))
  scores <- vector("list", length = nrow(Harddrive_df))
  
  for (i in 1:nrow(Harddrive_df)) {
    # Extract CPU name and format it for URL
    harddrive_name <- gsub(" ", "+", Harddrive_df$Name[i])
    
    # Construct the URL for the CPU's page
    harddrive_url <- paste0("https://www.harddrivebenchmark.net/hdd.php?hdd=", harddrive_name)
    
    # Scrape the CPU's page
    harddrive_page <- read_html(harddrive_url)
    
    # Extract the specific information you're interested in
    # Note: Replace the '.selector' with the actual CSS selector of the data you want
    harddrive_detail <- harddrive_page %>%
      html_nodes('p:nth-child(3) , p:nth-child(6) , .speedicon+ span') %>%
      html_text()
    
    # Store the extracted detail in the list
    release_date_temp <- str_trim(unlist(strsplit(harddrive_detail[1], ":"))[2])
    current_price_temp <- str_trim(unlist(strsplit(harddrive_detail[2], ":"))[2])
    score_temp <- harddrive_detail[3]
    
    release_dates[[i]] <- release_date_temp
    current_prices[[i]] <- current_price_temp
    urls[[i]] <- harddrive_url
    scores[[i]] <- score_temp
    
    # Optional: Print progress
    cat("Scraped data for", Harddrive_df$Name[i], "\n")
  }
  
  Harddrive_df$ReleaseDate <- unlist(release_dates)
  Harddrive_df$CurrentPrice2 <- unlist(current_prices)
  Harddrive_df$URL <- unlist(urls)
  Harddrive_df$Score <- unlist(scores)
  Harddrive_df$ReleaseDate <- as.Date(Harddrive_df$ReleaseDate, format = "%Y-%m-%d")
  Harddrive_df$CurrentPrice <- gsub("\\*", "", Harddrive_df$CurrentPrice)
  cleaned_data_harddrive <- Harddrive_df[!is.na(Harddrive_df$ReleaseDate), ]
  cleaned_data_harddrive$ReleaseQuarters <- quarters(cleaned_data_harddrive$ReleaseDate)
  cleaned_data_harddrive$ReleaseYear <- format(cleaned_data_harddrive$ReleaseDate, "%Y")
  cleaned_data_harddrive$ReleaseDate <- NULL
  
  write.csv(cleaned_data_harddrive, file = "../scraped_data/harddrive_data_r.csv", row.names = FALSE)
} else {
  Harddrive_df <- read.csv(file = "Component_data/harddrive_data.csv")
}

