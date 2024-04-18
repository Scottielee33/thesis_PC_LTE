if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

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

write.csv(CPU_complete, file = "../../final_data/CPU.csv")
write.csv(GPU_complete, file = "../../final_data/GPU.csv")
write.csv(Disk_complete, file = "../../final_data/Disk.csv")
write.csv(Memory_complete, file = "../../final_data/Memory.csv")
