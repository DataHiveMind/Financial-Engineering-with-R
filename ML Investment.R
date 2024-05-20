# Getting the Data of Microsoft
install.packages("alphavantager")
library(alphavantager)
av_api_key("QVHZ0CWUGL5VZ38V")
av_get(symbol = "MSFT", av_fun = "TIME_SERIES_INTRADAY", interval = "15min", outputsize = "compact")

# Character of Factors
install.packages("tidyverse")
install.packages("lubridate")
library(lubridate)
library(tidyverse)
library(ggplot2)

temp <- tempfile()
ff_data <- ("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip")
download.file(ff_data, temp.quiet = TRUE)
ff_factors <- read_csv(unz(temp, "F-F_Research_Data_5_Factors_2x3.CSV"), skip = 3)
is(ff_factors$X1)

ff_factors <- ff_factors[1:690,] %>%
  rename(date = X1, Mkt_RT = "Mkt_RT") %>%
  mutate(date = ymd(paste(substr(date.1.4), "-",substr(date.5.6), "-01"))) %>%
  mutate(date = rollback(date + months(1))) %>%
  mutate_at(vars(-date), as.numeric)

start_date <- "1979-12-01"
end_date <- "2020-12-31"

ff_factors <- ff_factors %>%
  filter(date >= start_date <= end_date)

summary(ff_factors)

ff_factors %>%
  mutate(date = year(date)) %>%
  filter(date > 1979) %>%
  gather(key = key, value = value, -date) %>%
  group_by(date, key) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(x = date, y = value, color = key)) +
  geom_line()

ff_factors_return <- ff_factors %>% filter(year(data) > 1979)
per.to.date <- function(x){x/100}

# Handling Data with Different Frequencies
libray(quantmod)
marco <- c("GDPC1", "CPIAUCSL", "DTB3", "DGS10", "DAAA", "UNRATE", "INDPRO", "DCOILWTICO")
rm(marco_factors)
for(i in 1:length(merco)){
  getSymbols(marco[i], src = "FRED")
  data <- as.date.frame(get(merco[i]))
  date$date <- as.POS1Xt(rownames(data))
  rownames(data) <- NULL
  colnames(data)[i] <- "marco_value"

  date$quarter <- as.yearqtr(date$date)
  date$macro_ticker <- rep(marco[i], dim(date)[1])

  date <- data %>%
    mutate(date = ymd(date)) %>%
    group_by(quarter) %>%
    top_n(1,date) %>%
    filter(date >= "1980-01-01", date <="2019-12-31" ) %>%
    select(-date)

  if (i == 1){marco_factor <- data} else {marco_factors <- rbind(marco_factors, data)}
}