# General Data Analysis Libraries
library(data.table)
library(lubridate)
library(shiny)

# Plotting Libraries
library(ggplot2)
library(scales)
library(plotly)

# Web scraping
library('rvest')

#################################################################################################################
#################################################################################################################
### Input Files
#################################################################################################################
#################################################################################################################
# Local Working Directory
setwd("C:/coding/covid19-dashboard/shiny")

# Shiny Server Working Directory
# setwd("/home/shiny/apps/covid19-dashboard/shiny")

transit_file <- file.path(getwd(),"data/TransitTable_crosstab.csv")
ferry_file <- file.path(getwd(),"data/FerriesTable_crosstab.csv")
rail_file <- file.path(getwd(),"data/RailTable_crosstab.csv")
unemployment_file <- file.path(getwd(),"data/unemployment.csv")
volume_file <- file.path(getwd(),"data/VolumeNumTableCountLocation_data.csv")

#################################################################################################################
#################################################################################################################
### Custom Colors
#################################################################################################################
#################################################################################################################

psrc_colors <- c(
  "CoastRhodo" = "#91268F",
  "CedarShake" = "#F05A28",
  "DouglasFirShoot" = "#8CC63E",    
  "FerryWake" = "#00A7A0",
  "DarkGrey" = "#76787A",    
  "LightGrey" = "#BBBDC0"
)

#################################################################################################################
#################################################################################################################
### Functions
#################################################################################################################
#################################################################################################################

create_line_chart <- function(w_tbl, w_title, w_label, w_dec, w_colors, w_group, w_factor, w_suff) {

  w_chart <- ggplotly(ggplot(data=w_tbl, aes(y=`value`, x=`day`, group=get(w_group), color=factor(get(w_group)),text = paste0(month(`date`),"-",day(`date`),"-",year(`date`)," ",w_title,": ",prettyNum(round(`value`*w_factor, w_dec), big.mark = ","),w_suff)))+
                          geom_line(size=1.2) +
                          scale_color_manual(values=w_colors)+
                          scale_x_date(labels = date_format("%B")) +
                          scale_y_continuous(labels = w_label) +
                          ylab(w_title)+
                          theme_light() +
                          theme(legend.title = element_blank(),
                               axis.text=element_text(size=10),
                               axis.text.x.bottom=element_text(size=10),
                               axis.title.y =element_text(size=10,face="bold"),
                               axis.title.x = element_blank(),
                               panel.grid.major = element_line(colour="#BBBDC0",size = 0.25),
                               panel.grid.minor = element_line(colour="#BBBDC0",size = 0.25),
                               panel.border = element_blank(),
                               axis.line = element_blank(),
                               legend.position="bottom")
                       ,tooltip = c("text")) %>% layout(hovermode = "x")

  return(w_chart)
}

return_estimate <- function(w_tbl, w_date) {

  working_estimate <- as.numeric(w_tbl[`date` %in% as.Date(w_date), sum(`value`)])
  
  return(working_estimate)
}

return_single_estimate <- function(w_tbl, w_date, w_var) {
  
  working_estimate <- as.numeric(w_tbl[`day` %in% as.Date(w_date) & `variable` %in% w_var, sum(`value`)]) * 100
  
  return(working_estimate)
}

return_matching_day <- function(w_tbl, w_day, w_year) {
  
  working_estimate <- as.numeric(w_tbl[`day` %in% as.Date(w_day) & `year` %in% c(w_year), sum(`value`)])
  
  return(working_estimate)
}

#################################################################################################################
#################################################################################################################
### TSA Screening Data (available via webscraping daily)
#################################################################################################################
#################################################################################################################

# Specifying the url for TSA website
tsa_url <- 'https://www.tsa.gov/coronavirus/passenger-throughput'

# Reading the HTML code from the website and storing in a datatable
tsa_stats_webpage <- read_html(tsa_url)
tsa_stats_html <- html_nodes(tsa_stats_webpage,'tr')
tsa_stats_text <- html_text(tsa_stats_html)

# Cleanup the list for reading into a data.table
tsa_list <- strsplit(tsa_stats_text, "\n")
tsa_list <- tsa_list[c(-1)]
tsa_list <- tsa_list[lapply(tsa_list,length)>0]

# Read HTML text into a data.table and minor cleanup
passengers <- as.data.frame(tsa_list,stringsAsFactors=FALSE)
passengers <- setDT(as.data.frame(t(passengers)))
nms <- c("date","2020","2019")
setnames(passengers,nms)
passengers$`2020` <- gsub(",","",passengers$`2020`)
passengers$`2019` <- gsub(",","",passengers$`2019`)

# Convert to Long Form for use in graphic creation
passengers <- melt(passengers, id.vars=c("date"))
passengers$date <- mdy(passengers$date)
passengers$data_date <- paste(month(passengers$date),"/",day(passengers$date),"/",passengers$variable)
passengers$data_date <- mdy(passengers$data_date)
passengers$value <- as.numeric(passengers$value)
passengers <- passengers[, c('variable') := NULL]
passengers$year <- year(passengers$data_date)
nms <- c("day","value","date","year")
setnames(passengers,nms)

latest_month <- max(month(passengers$date))
only_latest <- passengers[month(date) %in% latest_month]
latest_day <- max(day(only_latest$date))

min_tsa <- min(passengers$value)
max_tsa <- max(passengers$value)

#################################################################################################################
#################################################################################################################
### Transit Data
#################################################################################################################
#################################################################################################################
transit_data <- setDT(read.csv(transit_file,stringsAsFactors=FALSE))
cols <- c("Current","Community.Transit","Everett.Transit","King.County.Metro","Kitsap.Transit..Excludes.Fast.Foot.Ferry.","Pierce.Transit","Sound.Transit")
transit_data <- transit_data[,..cols]
nms <- c("day","Community Transit", "Everett Transit", "King County Metro", "Kitsap Transit", "Pierce Transit","Sound Transit")
setnames(transit_data,nms)
transit_data$day <- mdy(transit_data$day)

# Convert to Long Form for use in graphic creation
transit <- melt(transit_data,id.vars=c("day"))
transit$value <- gsub("%","",transit$value)
transit$value <- as.numeric(transit$value)
transit$value <- transit$value / 100
transit$date <- transit$day

ct_latest_month <- max(month(transit$day))
ct_only_latest <- transit[month(day) %in% ct_latest_month & `variable` %in% c("Community Transit")]
ct_only_latest <- ct_only_latest[!is.na(ct_only_latest$value),]
ct_latest_day <- max(day(ct_only_latest$day))

et_latest_month <- max(month(transit$day))
et_only_latest <- transit[month(day) %in% et_latest_month & `variable` %in% c("Everett Transit")]
et_only_latest <- et_only_latest[!is.na(et_only_latest$value),]
et_latest_day <- max(day(et_only_latest$day))

kcm_latest_month <- max(month(transit$day))
kcm_only_latest <- transit[month(day) %in% kcm_latest_month & `variable` %in% c("King County Metro")]
kcm_only_latest <- kcm_only_latest[!is.na(kcm_only_latest$value),]
kcm_latest_day <- max(day(kcm_only_latest$day))

kt_latest_month <- max(month(transit$day))
kt_only_latest <- transit[month(day) %in% kt_latest_month & `variable` %in% c("Kitsap Transit")]
kt_only_latest <- kt_only_latest[!is.na(kt_only_latest$value),]
kt_latest_day <- max(day(kt_only_latest$day))

pt_latest_month <- max(month(transit$day))
pt_only_latest <- transit[month(day) %in% pt_latest_month & `variable` %in% c("Pierce Transit")]
pt_only_latest <- pt_only_latest[!is.na(pt_only_latest$value),]
pt_latest_day <- max(day(pt_only_latest$day))

st_latest_month <- max(month(transit$day))
st_only_latest <- transit[month(day) %in% st_latest_month & `variable` %in% c("Sound Transit")]
st_only_latest <- st_only_latest[!is.na(st_only_latest$value),]
st_latest_day <- max(day(st_only_latest$day))

#################################################################################################################
#################################################################################################################
### Ferry Data
#################################################################################################################
#################################################################################################################
ferry_data <- setDT(read.csv(ferry_file,stringsAsFactors=FALSE))
cols <- c("Current","Bainbridge","Bremerton","Fauntleroy","Kingston","Mukilteo","Point_Defiance")
ferry_data <- ferry_data[,..cols]
nms <- c("day","Bainbridge","Bremerton","Fauntleroy","Kingston","Mukilteo","Point Defiance")
setnames(ferry_data,nms)
ferry_data$day <- mdy(ferry_data$day)

# Convert to Long Form for use in graphic creation
ferry <- melt(ferry_data,id.vars=c("day"))
ferry$value <- gsub("%","",ferry$value)
ferry$value <- as.numeric(ferry$value)
ferry$value <- ferry$value / 100
ferry$date <- ferry$day

ferry_latest_month <- max(month(ferry$day))
ferry_only_latest <- ferry[month(day) %in% ferry_latest_month]
ferry_latest_day <- max(day(ferry_only_latest$day))

#################################################################################################################
#################################################################################################################
### Rail Data
#################################################################################################################
#################################################################################################################
rail_data <- setDT(read.csv(rail_file,stringsAsFactors=FALSE))
cols <- c("Current","Amtrak")
rail_data <- rail_data[,..cols]
nms <- c("day","Amtrak")
setnames(rail_data,nms)
rail_data$day <- mdy(rail_data$day)

# Convert to Long Form for use in graphic creation
rail <- melt(rail_data,id.vars=c("day"))
rail$value <- gsub("%","",rail$value)
rail$value <- as.numeric(rail$value)
rail$value <- rail$value / 100
rail$date <- rail$day

rail_latest_month <- max(month(rail$day))
rail_only_latest <- rail[month(day) %in% rail_latest_month]
rail_latest_day <- max(day(rail_only_latest$day))

#################################################################################################################
#################################################################################################################
### Unemployment Data
#################################################################################################################
#################################################################################################################
unemployment <- setDT(read.csv(unemployment_file,stringsAsFactors=FALSE))
nms <- c("date","day","value","variable")
setnames(unemployment,nms)

# Clean for use in graphic creation
unemployment$date <- mdy(unemployment$date)
unemployment$day <- mdy(unemployment$day)
unemployment$value <- gsub(",","",unemployment$value)
unemployment <- unemployment[variable %in% c("Initial Claims")]
unemployment$value <- as.numeric(unemployment$value)
unemployment$year <- year(unemployment$date)

# Current Year
esd_current <- unemployment[year(date) %in% c(2020)]
esd_latest_month_current <- max(month(esd_current$date))
esd_only_latest_current <- esd_current[month(date) %in% esd_latest_month_current]
esd_latest_day_current <- max(day(esd_only_latest_current$date))

# Prior Year
esd_prior <- unemployment[year(date) %in% c(2019)]
esd_latest_month_prior <- max(month(esd_prior$date))
esd_only_latest_prior <- esd_prior[month(date) %in% esd_latest_month_prior]
esd_latest_day_prior <- max(day(esd_only_latest_prior$date))

#################################################################################################################
#################################################################################################################
### Traffic Volume Data
#################################################################################################################
#################################################################################################################
volume_data <- setDT(read.csv(volume_file,stringsAsFactors=FALSE))
nms <- c("Highway","County","Location","Date_2019","Date_2020","Measure","Day_of_Week","value","value_2019")
setnames(volume_data,nms)

# Trim to PSRC Region and cleanup
volume_data <- volume_data[County %in% c("King","Kitsap","Pierce","Snohomish")]
volume_data <- volume_data[Measure %in% c("2019 Volumes"), date := Date_2019]
volume_data <- volume_data[Measure %in% c("2020 Volumes"), date := Date_2020]
volume_data$day <- volume_data$Date_2020
cols <- c("date","day","Location","value")
volumes <- volume_data[,..cols]
volumes$date <- mdy(volumes$date)
volumes$day <- mdy(volumes$day)
volumes$year <- year(volumes$date)

# create list for drop down of count locations
count_locations <- sort(unique(volumes$Location))

volumes_latest_month <- max(month(volumes$day))
volumes_only_latest <- volumes[month(day) %in% volumes_latest_month]
volumes_latest_day <- max(day(volumes_only_latest$day))

