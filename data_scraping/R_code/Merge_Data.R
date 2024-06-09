if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("lubridate")) install.packages("lubridate")

# ---CPU---
CPU_R <- read.csv(file = "../scraped_data/cpu_data_r.csv")
CPU_Price <- read.csv(file = "../scraped_data/CPU_price_data.csv")

CPU_R <- distinct(CPU_R, CPU, .keep_all = TRUE)
CPU_Price <- distinct(CPU_Price, CPU, .keep_all = TRUE)

CPU_Price$ReleaseDate <- as.Date(CPU_Price$ReleaseDate, format = "%d %b %y")
CPU_Price$CurrentDate <- as.Date(CPU_Price$CurrentDate, format = "%d %b %y")

sum(duplicated(CPU_R$CPU))
sum(duplicated(CPU_Price$CPU))

CPU_R$URL <- NULL
CPU_Price$URL <- NULL
CPU_R <- rename(CPU_R, CurrentPrice1 = CurrentPrice)

CPU_complete <- left_join(CPU_Price, CPU_R, by = "CPU")

CPU_complete <- CPU_complete %>%
  mutate(ReleasePrice = ifelse(is.na(ReleasePrice) | ReleasePrice == "", CurrentPrice1, ReleasePrice),
         CurrentPrice = ifelse(is.na(CurrentPrice) | CurrentPrice == "", CurrentPrice1, CurrentPrice))

CPU_complete <- CPU_complete %>%
  mutate(ReleaseQuarterYear = paste0(ReleaseYear, "-", ifelse(ReleaseQuarters == "Q1", "03-31", 
                                                              ifelse(ReleaseQuarters == "Q2", "06-30", 
                                                                     ifelse(ReleaseQuarters == "Q3", "09-30", "12-31")))),
         ReleaseQuarterYear = as.Date(ReleaseQuarterYear, format = "%Y-%m-%d"))

CPU_complete$ReleaseQuarterYear <- as.character(CPU_complete$ReleaseQuarterYear)
CPU_complete$ReleaseDate <- as.character(CPU_complete$ReleaseDate)
CPU_complete$CurrentDate <- as.character(CPU_complete$CurrentDate)

CPU_complete <- CPU_complete %>%
  mutate(ReleaseDate = ifelse(is.na(ReleaseDate) | ReleaseDate == "", ReleaseQuarterYear, ReleaseDate),
         CurrentDate = ifelse(is.na(CurrentDate) | CurrentDate == "", ReleaseQuarterYear, CurrentDate))

CPU_complete$ReleaseQuarterYear <- NULL
CPU_complete$CurrentPrice1 <- NULL
CPU_complete$ReleaseQuarters <- NULL
CPU_complete$ReleaseYear <- NULL

CPU_complete$ReleaseDate <- as.Date(CPU_complete$ReleaseDate, format = "%Y-%m-%d")
CPU_complete$CurrentDate <- as.Date(CPU_complete$CurrentDate, format = "%Y-%m-%d")
CPU_complete$ReleasePrice <- as.numeric(gsub(",", "", CPU_complete$ReleasePrice))
CPU_complete$CurrentPrice <- as.numeric(gsub(",", "", CPU_complete$CurrentPrice))
CPU_complete$Score <- as.numeric(gsub(",", "", CPU_complete$Score))

CPU_complete <- CPU_complete[!grepl("threadripper|xeon", CPU_complete$CPU, ignore.case = TRUE), ]

CPU_complete$Year <- year(CPU_complete$ReleaseDate)

# Calculate the number of CPUs for each year
CPU_counts <- CPU_complete %>%
  mutate(Year = year(ReleaseDate)) %>%
  group_by(Year) %>%
  summarise(Count = n())

# Define a threshold for the minimum number of CPUs a year should have
threshold <- 10

# Group the years with fewer CPUs than the threshold together
CPU_counts$GroupedYear <- cumsum(CPU_counts$Count >= threshold)

# Join the grouped years back to the original data
CPU_complete <- CPU_complete %>%
  left_join(CPU_counts %>% select(Year, GroupedYear), by = "Year")

CPU_complete <- CPU_complete %>%
  group_by(GroupedYear) %>%
  filter(n() >= 10)

# Calculate the Quantiles for each group of years
CPU_complete <- CPU_complete %>%
  group_by(GroupedYear) %>%
  mutate(
    Quantile = cut(
      ReleasePrice, 
      breaks = c(-Inf, quantile(ReleasePrice, probs = c(0.33, 0.66)), Inf), 
      labels = c("First Quantile", "Second Quantile", "Third Quantile"), 
      include.lowest = TRUE
    )
  )

CPU_complete$Year <- NULL
CPU_complete$GroupedYear <- NULL

# ---GPU---
GPU_R <- read.csv(file = "../scraped_data/gpu_data_r.csv")
GPU_Price <- read.csv(file = "../scraped_data/GPU_price_data.csv")

GPU_Price <- rename(GPU_Price, GPU = Name)
GPU_R <- rename(GPU_R, GPU = Name)

GPU_R <- distinct(GPU_R, GPU, .keep_all = TRUE)
GPU_Price <- distinct(GPU_Price, GPU, .keep_all = TRUE)

GPU_Price$ReleaseDate <- as.Date(GPU_Price$ReleaseDate, format = "%d %b %y")
GPU_Price$CurrentDate <- as.Date(GPU_Price$CurrentDate, format = "%d %b %y")

sum(duplicated(GPU_R$GPU))
sum(duplicated(GPU_Price$GPU))

GPU_R$Link <- NULL
GPU_Price$Link <- NULL
GPU_R$DATE <- NULL
GPU_R$ReleaseDate <- NULL
GPU_R <- rename(GPU_R, CurrentPrice1 = CurrentPrice)
GPU_R$CurrentPrice1 <- as.character(GPU_R$CurrentPrice1)

GPU_complete <- left_join(GPU_R, GPU_Price, by = "GPU")

GPU_complete$ReleasePrice <- dplyr::na_if(GPU_complete$ReleasePrice, "")
GPU_complete$CurrentPrice <- dplyr::na_if(GPU_complete$CurrentPrice, "")

GPU_complete <- GPU_complete %>%
  mutate(ReleasePrice = ifelse(is.na(ReleasePrice) | ReleasePrice == "", CurrentPrice1, ReleasePrice),
         CurrentPrice = ifelse(is.na(CurrentPrice) | CurrentPrice == "", CurrentPrice1, CurrentPrice))

GPU_complete <- GPU_complete %>%
  mutate(ReleaseQuarterYear = paste0(Year, "-", ifelse(Quarters == "Q1", "03-31", 
                                                              ifelse(Quarters == "Q2", "06-30", 
                                                                     ifelse(Quarters == "Q3", "09-30", "12-31")))),
         ReleaseQuarterYear = as.Date(ReleaseQuarterYear, format = "%Y-%m-%d"))

GPU_complete$ReleaseQuarterYear <- as.character(GPU_complete$ReleaseQuarterYear)
GPU_complete$ReleaseDate <- as.character(GPU_complete$ReleaseDate)
GPU_complete$CurrentDate <- as.character(GPU_complete$CurrentDate)

GPU_complete <- GPU_complete %>%
  mutate(ReleaseDate = ifelse(is.na(ReleaseDate) | ReleaseDate == "", ReleaseQuarterYear, ReleaseDate),
         CurrentDate = ifelse(is.na(CurrentDate) | CurrentDate == "", ReleaseQuarterYear, CurrentDate))

GPU_complete$ReleaseQuarterYear <- NULL
GPU_complete$CurrentPrice1 <- NULL
GPU_complete$Quarters <- NULL
GPU_complete$Year <- NULL

GPU_complete$ReleaseDate <- as.Date(GPU_complete$ReleaseDate, format = "%Y-%m-%d")
GPU_complete$CurrentDate <- as.Date(GPU_complete$CurrentDate, format = "%Y-%m-%d")
GPU_complete$ReleasePrice <- as.numeric(gsub(",", "", GPU_complete$ReleasePrice))
GPU_complete$CurrentPrice <- as.numeric(gsub(",", "", GPU_complete$CurrentPrice))
GPU_complete$Score <- as.numeric(gsub(",", "", GPU_complete$Score))

GPU_complete <- GPU_complete[!grepl("RTX A|quadro|WX|FirePro|NVS|Tesla", GPU_complete$GPU, ignore.case = TRUE), ]

GPU_complete$Year <- year(GPU_complete$ReleaseDate)

# Calculate the number of GPUs for each year
GPU_counts <- GPU_complete %>%
  mutate(Year = year(ReleaseDate)) %>%
  group_by(Year) %>%
  summarise(Count = n())

# Define a threshold for the minimum number of GPUs a year should have
threshold <- 10

# Group the years with fewer GPUs than the threshold together
GPU_counts$GroupedYear <- cumsum(GPU_counts$Count >= threshold)

# Join the grouped years back to the original data
GPU_complete <- GPU_complete %>%
  left_join(GPU_counts %>% select(Year, GroupedYear), by = "Year")

# Calculate the quantiles for each group of years
GPU_complete <- GPU_complete %>%
  group_by(GroupedYear) %>%
  mutate(
    Quantile = cut(
      ReleasePrice, 
      breaks = c(-Inf, quantile(ReleasePrice, probs = c(0.33, 0.66)), Inf), 
      labels = c("First Quantile", "Second Quantile", "Third Quantile"), 
      include.lowest = TRUE
    )
  )

GPU_complete$Year <- NULL
GPU_complete$GroupedYear <- NULL

# ---Disk---
Disk_R <- read.csv(file = "../scraped_data/harddrive_data_r.csv")
Disk_Price <- read.csv(file = "../scraped_data/disk_price_data.csv")

Disk_Price <- rename(Disk_Price, Disk = Name)
Disk_R <- rename(Disk_R, Disk = Name)

Disk_R <- distinct(Disk_R, Disk, .keep_all = TRUE)
Disk_Price <- distinct(Disk_Price, Disk, .keep_all = TRUE)

Disk_Price$ReleaseDate <- as.Date(Disk_Price$ReleaseDate, format = "%d %b %y")
Disk_Price$CurrentDate <- as.Date(Disk_Price$CurrentDate, format = "%d %b %y")

sum(duplicated(Disk_R$Disk))
sum(duplicated(Disk_Price$Disk))

Disk_R$URL <- NULL
Disk_Price$URL <- NULL
Disk_R <- rename(Disk_R, CurrentPrice1 = CurrentPrice)
Disk_R$CurrentPrice1 <- as.character(Disk_R$CurrentPrice1)
Disk_R$CurrentPrice2 <- NULL

Disk_complete <- left_join(Disk_R, Disk_Price, by = "Disk")

Disk_complete <- Disk_complete %>%
  mutate(ReleaseQuarterYear = paste0(ReleaseYear, "-", ifelse(ReleaseQuarters == "Q1", "03-31", 
                                                              ifelse(ReleaseQuarters == "Q2", "06-30", 
                                                                     ifelse(ReleaseQuarters == "Q3", "09-30", "12-31")))),
         ReleaseQuarterYear = as.Date(ReleaseQuarterYear, format = "%Y-%m-%d"))

Disk_complete$ReleaseQuarterYear <- as.character(Disk_complete$ReleaseQuarterYear)
Disk_complete$ReleaseDate <- as.character(Disk_complete$ReleaseDate)
Disk_complete$CurrentDate <- as.character(Disk_complete$CurrentDate)

Disk_complete <- Disk_complete %>%
  mutate(ReleaseDate = ifelse(is.na(ReleaseDate) | ReleaseDate == "", ReleaseQuarterYear, ReleaseDate),
         CurrentDate = ifelse(is.na(CurrentDate) | CurrentDate == "", ReleaseQuarterYear, CurrentDate))

Disk_complete <- Disk_complete %>%
  mutate(ReleasePrice = ifelse(is.na(ReleasePrice) | ReleasePrice == "", CurrentPrice1, ReleasePrice),
         CurrentPrice = ifelse(is.na(CurrentPrice) | CurrentPrice == "", CurrentPrice1, CurrentPrice))

Disk_complete$ReleaseQuarterYear <- NULL
Disk_complete$CurrentPrice1 <- NULL
Disk_complete$ReleaseQuarters <- NULL
Disk_complete$ReleaseYear <- NULL

Disk_complete$ReleaseDate <- as.Date(Disk_complete$ReleaseDate, format = "%Y-%m-%d")
Disk_complete$CurrentDate <- as.Date(Disk_complete$CurrentDate, format = "%Y-%m-%d")
Disk_complete$ReleasePrice <- as.numeric(gsub(",", "", Disk_complete$ReleasePrice))
Disk_complete$CurrentPrice <- as.numeric(gsub(",", "", Disk_complete$CurrentPrice))
Disk_complete$Score <- as.numeric(gsub(",", "", Disk_complete$Score))

Disk_complete$Year <- year(Disk_complete$ReleaseDate)

# Calculate the number of Disks for each year
Disk_counts <- Disk_complete %>%
  mutate(Year = year(ReleaseDate)) %>%
  group_by(Year) %>%
  summarise(Count = n())

# Define a threshold for the minimum number of Disks a year should have
threshold <- 10

# Group the years with fewer Disks than the threshold together
Disk_counts$GroupedYear <- cumsum(Disk_counts$Count >= threshold)

# Join the grouped years back to the original data
Disk_complete <- Disk_complete %>%
  left_join(Disk_counts %>% select(Year, GroupedYear), by = "Year")

# Calculate the Quantiles for each group of years
Disk_complete <- Disk_complete %>%
  group_by(GroupedYear) %>%
  mutate(
    Quantile = cut(
      ReleasePrice, 
      breaks = c(-Inf, quantile(ReleasePrice, probs = c(0.33, 0.66)), Inf), 
      labels = c("First Quantile", "Second Quantile", "Third Quantile"), 
      include.lowest = TRUE
    )
  )

Disk_complete$Year <- NULL
Disk_complete$GroupedYear <- NULL
# ---RAM---
Memory_R <- read.csv(file = "../scraped_data/memory_data_r.csv")
Memory_Price <- read.csv(file = "../scraped_data/memory_price_data.csv")

Memory_R <- distinct(Memory_R, Memory, .keep_all = TRUE)
Memory_Price <- distinct(Memory_Price, Memory, .keep_all = TRUE)

Memory_Price$ReleaseDate <- as.Date(Memory_Price$ReleaseDate, format = "%d %b %y")
Memory_Price$CurrentDate <- as.Date(Memory_Price$CurrentDate, format = "%d %b %y")

sum(duplicated(Memory_R$Memory))
sum(duplicated(Memory_Price$Memory))

Memory_R$Link <- NULL
Memory_Price$Link <- NULL
Memory_R <- rename(Memory_R, CurrentPrice1 = CurrentPrice)
Memory_R$CurrentPrice1 <- as.character(Memory_R$CurrentPrice1)
Memory_R$CurrentPrice2 <- NULL
Memory_R$ReleaseDate <- NULL
Memory_R$ReleaseDates <- NULL

Memory_R$CurrentPrice1 <- str_extract(Memory_R$CurrentPrice1, "\\d+\\.?\\d*")
Memory_complete <- left_join(Memory_R, Memory_Price, by = "Memory")

Memory_complete <- Memory_complete %>%
  mutate(ReleaseQuarterYear = paste0(Year, "-", ifelse(Quarters == "Q1", "03-31", 
                                                              ifelse(Quarters == "Q2", "06-30", 
                                                                     ifelse(Quarters == "Q3", "09-30", "12-31")))),
         ReleaseQuarterYear = as.Date(ReleaseQuarterYear, format = "%Y-%m-%d"))

Memory_complete$ReleaseQuarterYear <- as.character(Memory_complete$ReleaseQuarterYear)
Memory_complete$ReleaseDate <- as.character(Memory_complete$ReleaseDate)
Memory_complete$CurrentDate <- as.character(Memory_complete$CurrentDate)

Memory_complete <- Memory_complete %>%
  mutate(ReleaseDate = ifelse(is.na(ReleaseDate) | ReleaseDate == "", ReleaseQuarterYear, ReleaseDate),
         CurrentDate = ifelse(is.na(CurrentDate) | CurrentDate == "", ReleaseQuarterYear, CurrentDate))

Memory_complete <- Memory_complete %>%
  mutate(ReleasePrice = ifelse(is.na(ReleasePrice) | ReleasePrice == "", CurrentPrice1, ReleasePrice),
         CurrentPrice = ifelse(is.na(CurrentPrice) | CurrentPrice == "", CurrentPrice1, CurrentPrice))

Memory_complete$ReleaseQuarterYear <- NULL
Memory_complete$CurrentPrice1 <- NULL
Memory_complete$Quarters <- NULL
Memory_complete$Year <- NULL

Memory_complete$ReleaseDate <- as.Date(Memory_complete$ReleaseDate, format = "%Y-%m-%d")
Memory_complete$CurrentDate <- as.Date(Memory_complete$CurrentDate, format = "%Y-%m-%d")
Memory_complete$ReleasePrice <- as.numeric(gsub(",", "", Memory_complete$ReleasePrice))
Memory_complete$CurrentPrice <- as.numeric(gsub(",", "", Memory_complete$CurrentPrice))
Memory_complete$Score <- as.numeric(gsub(",", "", Memory_complete$Score))

Memory_complete$Scores <- NULL

Quantiles <- quantile(Memory_complete$ReleasePrice, probs = c(0.33, 0.66))

Memory_complete$Quantile <- cut(Memory_complete$ReleasePrice, breaks=c(-Inf, Quantiles, Inf), 
                       labels=c("First Quantile", "Second Quantile", "Third Quantile"), include.lowest=TRUE)

Memory_complete$Year <- year(Memory_complete$ReleaseDate)

# Calculate the number of Memorys for each year
Memory_counts <- Memory_complete %>%
  mutate(Year = year(ReleaseDate)) %>%
  group_by(Year) %>%
  summarise(Count = n())

# Define a threshold for the minimum number of Memorys a year should have
threshold <- 10

# Group the years with fewer Memorys than the threshold together
Memory_counts$GroupedYear <- cumsum(Memory_counts$Count >= threshold)

# Join the grouped years back to the original data
Memory_complete <- Memory_complete %>%
  left_join(Memory_counts %>% select(Year, GroupedYear), by = "Year")

# Calculate the Quantiles for each group of years
Memory_complete <- Memory_complete %>%
  group_by(GroupedYear) %>%
  mutate(
    Quantile = cut(
      ReleasePrice, 
      breaks = c(-Inf, quantile(ReleasePrice, probs = c(0.33, 0.66)), Inf), 
      labels = c("First Quantile", "Second Quantile", "Third Quantile"), 
      include.lowest = TRUE
    )
  )

Memory_complete$Year <- NULL
Memory_complete$GroupedYear <- NULL

Memory_complete$Size <- stringr::str_extract(Memory_complete$Memory, "\\d+(?=GB\\b)")

# --------
write.csv(CPU_complete, file = "../final_data_no_inflation/CPU.csv")
write.csv(GPU_complete, file = "../final_data_no_inflation/GPU.csv")
write.csv(Disk_complete, file = "../final_data_no_inflation/Disk.csv")
write.csv(Memory_complete, file = "../final_data_no_inflation/Memory.csv")
