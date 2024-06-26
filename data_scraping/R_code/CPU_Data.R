if (!require("rvest")) install.packages("rvest")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

computeData <- TRUE

if (computeData){
  url <- "https://www.cpubenchmark.net/singleThread.html"
  page <- read_html(url)
  
  names <- page %>%
    html_nodes("#desktop-thread .prdname") %>%
    html_text()
  names <- as.matrix(names)
  
  cpulink <- page %>%
    html_nodes("#desktop-thread a") %>%
    html_attr("href") %>% paste0("https://www.cpubenchmark.net/",.)
  cpulink <- as.matrix(cpulink)
  
  # Convert the vector into a matrix with 3 columns
  matrix_form_scores <- matrix(names, byrow = TRUE)
  
  # Convert the matrix into a dataframe
  CPU_scores_df <- as.data.frame(matrix_form_scores)
  
  # Optionally, set column names
  colnames(CPU_scores_df) <- c("CPU")
  CPU_scores_df$URL <- unlist(cpulink)
  
  
  release_dates <- vector("list", length = nrow(CPU_scores_df))
  current_prices <- vector("list", length = nrow(CPU_scores_df))
  cpu_scores <- vector("list", length = nrow(CPU_scores_df))
  
  for (i in 1:nrow(CPU_scores_df)) {
    # Construct the URL for the CPU's page
    cpu_url <- CPU_scores_df$URL[i]
    
    # Scrape the CPU's page
    cpu_page <- read_html(cpu_url)
    
    # Extract the specific information you're interested in
    # Note: Replace the '.selector' with the actual CSS selector of the data you want
    cpu_detail <- cpu_page %>%
      html_nodes('.alt:nth-child(2) , .desc-foot p:nth-child(5) , .speedicon+ span') %>%
      html_text()
    
    # Store the extracted detail in the list
    release_date_temp <- str_trim(unlist(strsplit(cpu_detail[1], ":"))[2])
    current_price_temp <- str_trim(unlist(strsplit(cpu_detail[2], ":"))[2])
    score_temp <- cpu_detail[3]
    
    release_dates[[i]] <- release_date_temp
    current_prices[[i]] <- current_price_temp
    cpu_scores[[i]] <- score_temp
    
    # Optional: Print progress
    cat("Scraped data for", CPU_scores_df$CPU[i], "\n")
  }
  
  CPU_scores_df$ReleaseDate <- unlist(release_dates)
  CPU_scores_df$CurrentPrice <- unlist(current_prices)
  CPU_scores_df$score <- unlist(cpu_scores)
  
  CPU_scores_df <- CPU_scores_df %>% filter(CurrentPrice != "NA")
  CPU_scores_df$CurrentPrice <- gsub("\\*", "", CPU_scores_df$CurrentPrice)
  CPU_df <- CPU_scores_df
  CPU_df <- CPU_df %>% 
    mutate(CurrentPrice = str_extract(CurrentPrice, "\\d+\\.\\d+"))
  
  CPU_df <- separate(data = CPU_df, col = ReleaseDate, into = c("ReleaseQuarters", "ReleaseYear"), sep = " ")
  
  write.csv(CPU_df, file = "../scraped_data/cpu_data_r.csv", row.names = FALSE)
} else {
  CPU_df <- read.csv(file = "../scraped_data/cpu_data_r.csv")
}

