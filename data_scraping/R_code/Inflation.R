if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("lubridate")) install.packages("lubridate")

CPU <- read.csv(file = "../final_data_no_inflation/CPU.csv", row.names = NULL)
GPU <- read.csv(file = "../final_data_no_inflation/GPU.csv", row.names = NULL)
Memory <- read.csv(file = "../final_data_no_inflation/Memory.csv", row.names = NULL)
Disk <- read.csv(file = "../final_data_no_inflation/Disk.csv", row.names = NULL)

# ---Inflation---
# Creating a vector of years
years <- 2008:2024

# Creating a vector of inflation rates
inflation_rates <- c(3.84, 0.36, 1.64, 3.16, 2.07, 1.46, 1.62, 0.12, 1.26, 2.13, 2.44, 1.81, 1.23, 4.70, 8.00, 4.1, 3.5)

# Creating the data frame
inflation_df <- data.frame(Year = years, InflationRate = inflation_rates)

# Calculate cumulative inflation multipliers
# Convert annual percentage rates to multipliers
inflation_multipliers <- (1 + inflation_rates / 100)

# Calculate cumulative products by chaining the multipliers from the base year
cumulative_multipliers <- cumprod(inflation_multipliers)

# Add the cumulative multipliers to the data frame
inflation_df$MultiplierFrom2008 <- cumulative_multipliers

CPU$ReleaseDate <- as.Date(CPU$ReleaseDate, format = "%Y-%m-%d")
CPU$ReleaseYear <- as.integer(format(CPU$ReleaseDate, "%Y"))
CPU <- merge(CPU, inflation_df, by.x="ReleaseYear", by.y="Year")
CPU$AdjustedReleasePrice <- CPU$ReleasePrice / CPU$MultiplierFrom2008
CPU$InflationRate <- NULL
CPU$MultiplierFrom2008 <- NULL
CPU$ReleaseYear <- NULL

CPU$CurrentDate <- as.Date(CPU$CurrentDate, format = "%Y-%m-%d")
CPU$CurrentYear <- as.integer(format(CPU$CurrentDate, "%Y"))
CPU <- merge(CPU, inflation_df, by.x="CurrentYear", by.y="Year")
CPU$AdjustedCurrentPrice <- CPU$CurrentPrice / CPU$MultiplierFrom2008
CPU$InflationRate <- NULL
CPU$MultiplierFrom2008 <- NULL
CPU$CurrentYear <- NULL

GPU$ReleaseDate <- as.Date(GPU$ReleaseDate, format = "%Y-%m-%d")
GPU$ReleaseYear <- as.integer(format(GPU$ReleaseDate, "%Y"))
GPU <- merge(GPU, inflation_df, by.x="ReleaseYear", by.y="Year")
GPU$AdjustedReleasePrice <- GPU$ReleasePrice / GPU$MultiplierFrom2008
GPU$InflationRate <- NULL
GPU$MultiplierFrom2008 <- NULL
GPU$ReleaseYear <- NULL

GPU$CurrentDate <- as.Date(GPU$CurrentDate, format = "%Y-%m-%d")
GPU$CurrentYear <- as.integer(format(GPU$CurrentDate, "%Y"))
GPU <- merge(GPU, inflation_df, by.x="CurrentYear", by.y="Year")
GPU$AdjustedCurrentPrice <- GPU$CurrentPrice / GPU$MultiplierFrom2008
GPU$InflationRate <- NULL
GPU$MultiplierFrom2008 <- NULL
GPU$CurrentYear <- NULL

Memory$ReleaseDate <- as.Date(Memory$ReleaseDate, format = "%Y-%m-%d")
Memory$ReleaseYear <- as.integer(format(Memory$ReleaseDate, "%Y"))
Memory <- merge(Memory, inflation_df, by.x="ReleaseYear", by.y="Year")
Memory$AdjustedReleasePrice <- Memory$ReleasePrice / Memory$MultiplierFrom2008
Memory$InflationRate <- NULL
Memory$MultiplierFrom2008 <- NULL
Memory$ReleaseYear <- NULL

Memory$CurrentDate <- as.Date(Memory$CurrentDate, format = "%Y-%m-%d")
Memory$CurrentYear <- as.integer(format(Memory$CurrentDate, "%Y"))
Memory <- merge(Memory, inflation_df, by.x="CurrentYear", by.y="Year")
Memory$AdjustedCurrentPrice <- Memory$CurrentPrice / Memory$MultiplierFrom2008
Memory$InflationRate <- NULL
Memory$MultiplierFrom2008 <- NULL
Memory$CurrentYear <- NULL

Disk$ReleaseDate <- as.Date(Disk$ReleaseDate, format = "%Y-%m-%d")
Disk$ReleaseYear <- as.integer(format(Disk$ReleaseDate, "%Y"))
Disk <- merge(Disk, inflation_df, by.x="ReleaseYear", by.y="Year")
Disk$AdjustedReleasePrice <- Disk$ReleasePrice / Disk$MultiplierFrom2008
Disk$InflationRate <- NULL
Disk$MultiplierFrom2008 <- NULL
Disk$ReleaseYear <- NULL

Disk$CurrentDate <- as.Date(Disk$CurrentDate, format = "%Y-%m-%d")
Disk$CurrentYear <- as.integer(format(Disk$CurrentDate, "%Y"))
Disk <- merge(Disk, inflation_df, by.x="CurrentYear", by.y="Year")
Disk$AdjustedCurrentPrice <- Disk$CurrentPrice / Disk$MultiplierFrom2008
Disk$InflationRate <- NULL
Disk$MultiplierFrom2008 <- NULL
Disk$CurrentYear <- NULL

write.csv(CPU, file = "../../final_data/CPU.csv")
write.csv(GPU, file = "../../final_data/GPU.csv")
write.csv(Memory, file = "../../final_data/Memory.csv")
write.csv(Disk, file = "../../final_data/Disk.csv")