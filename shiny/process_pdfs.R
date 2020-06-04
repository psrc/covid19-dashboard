# PDF Reading
library(data.table)
library(lubridate)
library(tabulizer)

# Local Working Directory
datadir <-"C:/coding/covid19-dashboard/shiny/data"
output_file <- file.path(datadir,"unemployment_claims.csv")

esd_url <- "https://esdorchardstorage.blob.core.windows.net/esdwa/Default/ESDWAGOV/newsroom/Statistics/wkly-initial-claims-count-table.pdf"
matrix_results <- extract_tables(esd_url)
unemployment_claims <- setDT(as.data.frame(matrix_results[[2]]))

# Process the 2019 data into a usable table for plotting
working <- unemployment_claims[-1,c(1:3,5:7)]
nms <- c("date","Initial Claims","Change From Prior Week","Percentage Change From Prior Week","4-Week Average","day")
setnames(working,nms)
working$date <- mdy(working$date)
working$day <- mdy(working$day)
unemployment_2019 <- melt(working, id.vars=c("date","day"))
unemployment_2019$value <- gsub(",","",unemployment_2019$value)
unemployment_2019$value <- as.numeric(unemployment_2019$value)

# Process the 2020 data into a usable table for plotting
working <- unemployment_claims[-1,c(7:11)]
nms <- c("date","Initial Claims","Change From Prior Week","Percentage Change From Prior Week","4-Week Average")
setnames(working,nms)
working$date <- mdy(working$date)
working$day <- working$date
unemployment_2020 <- melt(working, id.vars=c("date","day"))
unemployment_2020$value <- gsub(",","",unemployment_2020$value)
unemployment_2020$value <- as.numeric(unemployment_2020$value)

# Combine 2019 and 2020 and output to csv
unemployment <- rbind(unemployment_2019,unemployment_2020)
unemployment <- unemployment[variable %in% c("Initial Claims")]
unemployment$year <- year(unemployment$date)
fwrite(unemployment,output_file)
