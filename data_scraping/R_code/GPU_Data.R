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
  G2D_score <- vector("list", length = nrow(gpudata_df))
  G3D_score <- vector("list", length = nrow(gpudata_df))
  
  for (i in 1:nrow(gpudata_df)) {
    # Construct the URL for the GPU's page
    gpu_url <- gpudata_df$Link[i]
    
    # Scrape the GPU's page 
    gpu_page <- read_html(gpu_url)
    
    # Extract the specific information you're interested in
    # Note: Replace the '.selector' with the actual CSS selector of the data you want
    gpu_detail <- gpu_page %>%
      html_nodes('.right-desc') %>%
      html_text()
    
    # Define a regex pattern to capture both numbers
    pattern <- "Average G3D Mark\\s*\\n\\s*(\\d+).*G2D Mark: (\\d+)"
    
    # Extract the numbers using str_match
    matches <- str_match(gpu_detail, pattern)
    
    # The first captured group is the number after "Average G3D Mark"
    g3d_mark <- matches[1, 2]
    
    # The second captured group is the number after "G2D Mark"
    g2d_mark <- matches[1, 3]
    
    G2D_score[[i]] <- g2d_mark
    G3D_score[[i]] <- g3d_mark
    
    # Optional: Print progress
    cat("Scraped data for", gpudata_df$Name[i], "\n")
  }
  
  gpudata_df$G2DScore <- unlist(G2D_score)
  gpudata_df$G3DScore <- unlist(G3D_score)
  
  GPU_df <- gpudata_df
  
  write.csv(GPU_df, file = "../scraped_data/gpu_data_r.csv", row.names = FALSE)
} else {
  GPU_df <- read.csv(file = "../scraped_data/gpu_data_r.csv")
}
