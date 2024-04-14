if (!require("rvest")) install.packages("rvest")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

computeData <- FALSE

if (computeData){
  url <- "https://www.cpubenchmark.net/desktop.html"
  page <- read_html(url)
  
  CPU_names_scores <- page %>%
    html_nodes(".price-neww , .publift-widget-22645135309-button , .count , .prdname") %>%
    html_text()
  
  # Convert the vector into a matrix with 3 columns
  matrix_form_scores <- matrix(CPU_names_scores, ncol = 3, byrow = TRUE)
  
  # Convert the matrix into a dataframe
  CPU_scores_df <- as.data.frame(matrix_form_scores)
  
  # Optionally, set column names
  colnames(CPU_scores_df) <- c("CPU", "Score", "CurrentPrice")
  
  release_dates <- vector("list", length = nrow(CPU_scores_df))
  current_prices <- vector("list", length = nrow(CPU_scores_df))
  cpu_urls <- vector("list", length = nrow(CPU_scores_df))
  
  for (i in 1:nrow(CPU_scores_df)) {
    # Extract CPU name and format it for URL
    cpu_name <- gsub(" ", "+", CPU_scores_df$CPU[i])
    
    # Construct the URL for the CPU's page
    cpu_url <- paste0("https://www.cpubenchmark.net/cpu.php?cpu=", cpu_name)
    
    # Scrape the CPU's page
    cpu_page <- read_html(cpu_url)
    
    # Extract the specific information you're interested in
    # Note: Replace the '.selector' with the actual CSS selector of the data you want
    cpu_detail <- cpu_page %>%
      html_nodes('.desc-foot p:nth-child(5) , .alt:nth-child(2)') %>%
      html_text()
    
    # Store the extracted detail in the list
    release_date_temp <- str_trim(unlist(strsplit(cpu_detail[1], ":"))[2])
    current_price_temp <- str_trim(unlist(strsplit(cpu_detail[2], ":"))[2])
    
    release_dates[[i]] <- release_date_temp
    current_prices[[i]] <- current_price_temp
    cpu_urls[[i]] <- cpu_url
    
    # Optional: Print progress
    cat("Scraped data for", CPU_scores_df$CPU[i], "\n")
  }
  
  CPU_scores_df$ReleaseDate <- unlist(release_dates)
  CPU_scores_df$CurrentPrice2 <- unlist(current_prices)
  CPU_scores_df$URL <- unlist(cpu_urls)
  
  CPU_scores_df <- CPU_scores_df %>% filter(CurrentPrice != "NA")
  CPU_scores_df$CurrentPrice <- gsub("\\*", "", CPU_scores_df$CurrentPrice)
  CPU_df <- CPU_scores_df
  
  CPU_df <- separate(data = CPU_df, col = ReleaseDate, into = c("ReleaseQuarters", "ReleaseYear"), sep = " ")
  
  CPU_df$CurrentPrice <- gsub("\\$", "", CPU_df$CurrentPrice)
  
  write.csv(CPU_df, file = "Component_data/cpu_data_r.csv", row.names = FALSE)
} else {
  CPU_df <- read.csv(file = "../scraped_data/cpu_data_r.csv")
}