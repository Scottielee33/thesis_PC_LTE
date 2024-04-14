if (!require("rvest")) install.packages("rvest")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")

computeData <- TRUE

if (computeData) {
  webpagegpu <- read_html("https://www.videocardbenchmark.net/directCompute.html")
  
  gpunames <- webpagegpu %>%
    html_nodes("#mark .prdname") %>%
    html_text()
  gpunames <- as.matrix(gpunames)
  
  gpulink <- webpagegpu %>%
    html_nodes("#mark a") %>%
    html_attr("href") %>% paste0("https://www.videocardbenchmark.net/",.)
  gpulink <- as.matrix(gpulink)
  
  gpuscore <- webpagegpu %>%
    html_nodes("#mark .count") %>%
    html_text()
  gpuscore <- as.matrix(gpuscore)
  
  gpudata <- cbind(gpunames, gpuscore, gpulink)
  # Convert to a dataframe
  gpudata_df <- data.frame(gpudata, stringsAsFactors = FALSE)
  colnames(gpudata_df) <- c("Name", "Score", "Link")
  release_dates <- vector("list", length = nrow(gpudata_df))
  current_price <- vector("list", length = nrow(gpudata_df))
  
  for (i in 1:nrow(gpudata_df)) {
    # Construct the URL for the GPU's page
    gpu_url <- gpudata_df$Link[i]
    
    # Scrape the GPU's page 
    gpu_page <- read_html(gpu_url)
    
    # Extract the specific information you're interested in
    # Note: Replace the '.selector' with the actual CSS selector of the data you want
    gpu_detail <- gpu_page %>%
      html_nodes('.desc-foot p:nth-child(3) , .desc-foot .bg-table-row:nth-child(6)') %>%
      html_text()
    
    # Store the extracted detail in the list
    release_date_temp <- str_trim(unlist(strsplit(gpu_detail[1], ":"))[2])
    current_price_temp <- str_trim(unlist(strsplit(gpu_detail[2], ":"))[2])
    
    release_dates[[i]] <- release_date_temp
    current_price[[i]] <- current_price_temp
    
    # Optional: Print progress
    cat("Scraped data for", gpudata_df$Name[i], "and", current_price_temp, "\n")
  }
  
  gpudata_df$ReleaseDate <- unlist(release_dates)
  
  gpudata_df$CurrentPrice <- unlist(current_price)
  gpudata_df <- gpudata_df %>% filter(CurrentPrice != "NA")
  
  gpudata_df$ReleaseDate <- as.Date(gpudata_df$ReleaseDate, format = "%Y-%m-%d")
  cleaned_data_gpu <- gpudata_df[!is.na(gpudata_df$ReleaseDate), ]
  cleaned_data_gpu$Quarters <- quarters(cleaned_data_gpu$ReleaseDate)
  cleaned_data_gpu$Year <- format(cleaned_data_gpu$ReleaseDate, "%Y")
  
  GPU_df <- cleaned_data_gpu
  
  GPU_df <- separate(GPU_df, CurrentPrice, into = c("CurrentPrice", "USD", "DATE"), sep = " ")
  GPU_df$USD <- NULL
  
  GPU_df$CurrentPrice <- gsub("\\$", "", GPU_df$CurrentPrice)
  
  print(cleaned_data_gpu)
  write.csv(GPU_df, file = "../scraped_data/gpu_data_r.csv", row.names = FALSE)
} else {
  GPU_df <- read.csv(file = "../scraped_data/gpu_data_r.csv")
}
