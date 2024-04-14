if (!require("dplyr")) install.packages("dplyr")

# CPU
CPU_R <- read.csv(file = "../scraped_data/cpu_data_r.csv")
CPU_Price <- read.csv(file = "../scraped_data/CPU_price_data.csv")

CPU_R <- distinct(CPU_R, CPU, .keep_all = TRUE)
CPU_Price <- distinct(CPU_Price, CPU, .keep_all = TRUE)

CPU_Price$ReleaseDate <- as.Date(CPU_Price$ReleaseDate, format = "%d %b %y")
CPU_Price$CurrentDate <- as.Date(CPU_Price$CurrentDate, format = "%d %b %y")

sum(duplicated(CPU_R$CPU))
sum(duplicated(CPU_Price$CPU))

CPU_R$CurrentPrice2 <- NULL
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

CPU_complete$ReleaseDate <- ifelse(is.na(CPU_complete$ReleaseDate), CPU_complete$ReleaseQuarterYear, CPU_complete$ReleaseDate)
