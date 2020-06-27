# General Data Analysis Libraries
library(data.table)
library(lubridate)
library(shiny)
library(dplyr)

# Plotting Libraries
library(ggplot2)
library(scales)
library(plotly)

# Web scraping
library(rvest)

# PDF Processing and Cleaning
library(pdftools)
library(tidyr)

# Spatial Analysis and Map Making
library(leaflet)
library(sf)

#################################################################################################################
#################################################################################################################
### Input Files
#################################################################################################################
#################################################################################################################
# Local Working Directory
#wrkdir <-"C:/coding/covid19-dashboard/shiny"

# Shiny Server Working Directory
wrkdir <- "/home/shiny/apps/covid19-dashboard/shiny"

transit_file <- file.path(wrkdir,"data/TransitTable_data.csv")
ferry_file <- file.path(wrkdir,"data/FerriesTable_data.csv")
rail_file <- file.path(wrkdir,"data/AmtrakTable_data.csv")
volume_file <- file.path(wrkdir,"data/VolumeNumTableCountLocation_data.csv")
freight_file <- file.path(wrkdir,"data/Freight_Table_data.csv")
nonmotorized_file <- file.path(wrkdir,"data/TableCounterLocaBikePedCount_data.csv")
nonmotorized_file_SDOT <- file.path(wrkdir,"data/TableCounterLocaBikePedCountSDOT_data.csv")
ptba_shapefile <- file.path(wrkdir,"data/shapefiles/psrc_ptba_wgs84.shp")

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

create_line_chart <- function(w_tbl, w_title, w_label, w_dec, w_colors, w_group, w_factor, w_suff, w_tit = "") {
  
  w_chart <- ggplotly(ggplot(data=w_tbl, aes(y=`value`, x=`day`, group=get(w_group), color=factor(get(w_group)),text = paste0(month(`date`),"-",day(`date`),"-",year(`date`)," ",w_title,": ",prettyNum(round(`value`*w_factor, w_dec), big.mark = ","),w_suff)))+
                          geom_line(size=1.2) + 
                          ggtitle(paste(w_tit))+
                          scale_color_manual(values=w_colors)+
                          scale_x_date(labels = date_format("%B")) +
                          scale_y_continuous(labels = w_label) +
                          ylab(w_title)+
                          theme_light() +
                          theme(
                               axis.text=element_text(size=10),
                               axis.text.x.bottom=element_text(size=10),
                               axis.title.y =element_text(size=10,face="bold"),
                               axis.title.x = element_blank(),
                               panel.grid.major = element_line(colour="#BBBDC0",size = 0.25),
                               panel.grid.minor = element_line(colour="#BBBDC0",size = 0.25),
                               panel.border = element_blank(),
                               axis.line = element_blank(),
                               legend.position="bottom",
                               legend.title = element_blank())
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

find_place_data <- function(plc_shp, wrk_nm, wrk_typ) {
  temp <- plc_shp
  temp <- setDT(temp)
  wrk_coord <- as.numeric(temp[NAME == wrk_nm,get(wrk_typ)])
  return(wrk_coord)
}

create_place_map <- function(wrk_shp,wrk_plc) {
  
  # Trim for current place
  current = wrk_shp[wrk_shp$NAME == wrk_plc,]
  
  working_map <- leaflet(data=current) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Transit Service Area"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addPolygons(fillColor = "76787A",
              weight = 4,
              opacity = 1.0,
              color = "#91268F",
              dashArray = "4",
              fillOpacity = 0.0,
              group = "Transit Service Area")%>%
    setView(lng=find_place_data(wrk_shp,wrk_plc,"LON"), lat=find_place_data(wrk_shp,wrk_plc,"LAT"), zoom=find_place_data(wrk_shp,wrk_plc,"ZOOM"))
  
  return(working_map)

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
nms <- c("day","variable","value","agency_id")
setnames(transit_data,nms)

# Clean up data
transit_data$day <- gsub("Sun, ","",transit_data$day)
transit_data$day <- gsub("Mon, ","",transit_data$day)
transit_data$day <- gsub("Tue, ","",transit_data$day)
transit_data$day <- gsub("Wed, ","",transit_data$day)
transit_data$day <- gsub("Thu, ","",transit_data$day)
transit_data$day <- gsub("Fri, ","",transit_data$day)
transit_data$day <- gsub("Sat, ","",transit_data$day)
transit_data$day <- mdy(transit_data$day)
transit_data$date <- transit_data$day

transit_data$value <- gsub("%","",transit_data$value)
transit_data$value <- as.numeric(transit_data$value)
transit_data$value <- transit_data$value / 100

transit_data$variable <- gsub("Kitsap Transit \\(Excludes Fast Foot Ferry\\)","Kitsap Transit",transit_data$variable)
transit_data$variable <- gsub("Kitsap Transit - Fast Foot Ferry only","Kitsap Fast Ferry",transit_data$variable)

psrc_agencies <- c("Community Transit","Everett Transit","King County Metro","Kitsap Transit", "Pierce Transit", "Sound Transit")
transit <- transit_data[variable %in% psrc_agencies]

# Latest Data by Operator
working <- na.omit(transit)
working <- working[variable %in% c("Community Transit")]
ct_latest_month <- max(month(working$day))
ct_only_latest <- working[month(day) %in% ct_latest_month]
ct_latest_day <- max(day(ct_only_latest$day))

working <- na.omit(transit)
working <- working[variable %in% c("Everett Transit")]
et_latest_month <- max(month(working$day))
et_only_latest <- working[month(day) %in% et_latest_month]
et_latest_day <- max(day(et_only_latest$day))

working <- na.omit(transit)
working <- working[variable %in% c("King County Metro")]
kcm_latest_month <- max(month(working$day))
kcm_only_latest <- working[month(day) %in% kcm_latest_month]
kcm_latest_day <- max(day(kcm_only_latest$day))

working <- na.omit(transit)
working <- working[variable %in% c("Kitsap Transit")]
kt_latest_month <- max(month(working$day))
kt_only_latest <- working[month(day) %in% kt_latest_month]
kt_latest_day <- max(day(kt_only_latest$day))

working <- na.omit(transit)
working <- working[variable %in% c("Kitsap Fast Ferry")]
ktf_latest_month <- max(month(working$day))
ktf_only_latest <- working[month(day) %in% ktf_latest_month]
ktf_latest_day <- max(day(ktf_only_latest$day))

working <- na.omit(transit)
working <- working[variable %in% c("Pierce Transit")]
pt_latest_month <- max(month(working$day))
pt_only_latest <- working[month(day) %in% pt_latest_month]
pt_latest_day <- max(day(pt_only_latest$day))

working <- na.omit(transit)
working <- working[variable %in% c("Sound Transit")]
st_latest_month <- max(month(working$day))
st_only_latest <- working[month(day) %in% st_latest_month]
st_latest_day <- max(day(st_only_latest$day))

working <- na.omit(transit_data)
working <- working[variable %in% c("Average")]
all_tran_latest_month <- max(month(working$day))
all_tran_only_latest <- working[month(day) %in% all_tran_latest_month]
all_tran_latest_day <- max(day(all_tran_only_latest$day))

#################################################################################################################
#################################################################################################################
### Ferry Data
#################################################################################################################
#################################################################################################################
ferry_data <- setDT(read.csv(ferry_file,stringsAsFactors=FALSE))
nms <- c("day","variable","metric","value")
setnames(ferry_data,nms)
ferry_data$day <- mdy(ferry_data$day)
ferry_data$date <- ferry_data$day

psrc_ferry <- c("Edmonds -  Kingston","Fauntleroy - Vashon - Southworth","Mukilteo - Clinton","Point Defiance - Tahlequah","Seattle - Bainbridge Island","Seattle - Bremerton")
ferry <- ferry_data[variable %in% psrc_ferry & metric %in% "Percentage Change"]

ferry_latest_month <- max(month(ferry$day))
ferry_only_latest <- ferry[month(day) %in% ferry_latest_month]
ferry_latest_day <- max(day(ferry_only_latest$day))

ferry_ridership_2020 <- ferry_data[variable %in% psrc_ferry & metric %in% "2020 Ridership"]
ferry_ridership_2019 <- ferry_data[variable %in% psrc_ferry & metric %in% "2019     Ridership"]

#################################################################################################################
#################################################################################################################
### Rail Data
#################################################################################################################
#################################################################################################################
rail_data <- setDT(read.csv(rail_file,stringsAsFactors=FALSE))
nms <- c("Date_2019","Date_2020","Measure","day_of_week","value")
setnames(rail_data,nms)

# cleanup
rail_data$Date_2019 <- gsub(".*, ", "",rail_data$Date_2019)
rail_data$Date_2020 <- gsub(".*, ", "",rail_data$Date_2020)
rail_data <- rail_data[Measure %in% c("2019 Ridership"), date := Date_2019]
rail_data <- rail_data[Measure %in% c("2020 Ridership"), date := Date_2020]
rail_data$day <- rail_data$Date_2020
rail_data <- na.omit(rail_data)

cols <- c("date","day","value")
rail <- rail_data[,..cols]
rail$date <- mdy(rail$date)
rail$day <- mdy(rail$day)
rail$year <- year(rail$date)

rail_latest_month <- max(month(rail$day))
rail_only_latest <- rail[month(day) %in% rail_latest_month]
rail_latest_day <- max(day(rail_only_latest$day))

#################################################################################################################
#################################################################################################################
### Unemployment Data
#################################################################################################################
#################################################################################################################
esd_url <- "https://esdorchardstorage.blob.core.windows.net/esdwa/Default/ESDWAGOV/newsroom/Statistics/wkly-initial-claims-count-table.pdf"

# Clean up the raw pdf 
unemployment <- pdf_text(esd_url) %>% readr::read_lines()
unemployment <-  unemployment[7:58]
unemployment_claims <- setDT(as.data.frame(unemployment))
unemployment_claims$unemployment <- gsub("\\s+", " ",unemployment_claims$unemployment)
unemployment_claims$unemployment <- gsub(",", "",unemployment_claims$unemployment)
unemployment_claims$unemployment <- trimws(unemployment_claims$unemployment, "l")
unemployment_claims <- unemployment_claims %>% separate(unemployment, c("v1", "v2","v3", "v4", "v5", "v6", "v7","v8", "v9", "v10"), sep=" ")
unemployment_claims <- na.omit(unemployment_claims)

# Process the 2019 data into a usable table for plotting
working <- unemployment_claims[,c(1:6)]
nms <- c("date","Initial Claims","Weekly Change", "Percent Change", "4 Week Average","day")
setnames(working,nms)
working$date <- mdy(working$date)
working$day <- mdy(working$day)
working$`Percent Change` <- as.character((as.numeric(working$`Percent Change`))/100)
unemployment_2019 <- melt(working, id.vars=c("date","day"))
unemployment_2019$value <- as.numeric(unemployment_2019$value)

# Process the 2020 data into a usable table for plotting
working <- unemployment_claims[,c(6:10)]
nms <- c("date","Initial Claims","Weekly Change", "Percent Change", "4 Week Average")
setnames(working,nms)
working$date <- mdy(working$date)
working$day <- mdy(working$day)
working$day <- working$date
working$`Percent Change` <- as.character((as.numeric(working$`Percent Change`))/100)
unemployment_2020 <- melt(working, id.vars=c("date","day"))
unemployment_2020$value <- as.numeric(unemployment_2020$value)

# Combine 2019 and 2020
unemployment <- rbind(unemployment_2019,unemployment_2020)
unemployment$year <- year(unemployment$date)
unemployment <- unemployment[variable %in% c("Initial Claims","Weekly Change", "4 Week Average")]

# create list for drop down
claim_types <- sort(unique(unemployment$variable))

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

#################################################################################################################
#################################################################################################################
### Truck Volume Data
#################################################################################################################
#################################################################################################################
truck_data <- setDT(read.csv(freight_file,stringsAsFactors=FALSE))
nms <- c("Highway","County","Location","date","value")
setnames(truck_data,nms)

# Trim to PSRC Region and cleanup
truck_data <- truck_data[County %in% c("King","Kitsap","Pierce","Snohomish")]
truck_data$date <- mdy(truck_data$date)
truck_data$day <- truck_data$date
cols <- c("date","day","Location","value")
trucks <- truck_data[,..cols]
trucks$year <- year(trucks$date)
trucks$value <- gsub("%","",trucks$value)
trucks$value <- as.numeric(trucks$value)
trucks$value <- trucks$value / 100

# create list for drop down of count locations
truck_count_locations <- sort(unique(trucks$Location))

trucks_latest_month <- max(month(trucks$day))
trucks_only_latest <- trucks[month(day) %in% trucks_latest_month]
trucks_latest_day <- max(day(trucks_only_latest$day))

#################################################################################################################
#################################################################################################################
### Nonmotorized Data
#################################################################################################################
#################################################################################################################
nonmotor_data <- setDT(read.csv(nonmotorized_file,stringsAsFactors=FALSE))
#nonmotor_data <- setDT(nonmotor_data_upd)
nms <- c("County","City","Location","Type","date","value")
setnames(nonmotor_data,nms)

# Trim to PSRC Region and cleanup
nonmotor_data <- nonmotor_data[County %in% c("King","Kitsap","Pierce","Snohomish")]
nonmotor_data$date <- mdy(nonmotor_data$date)

nonmotor_data$day <- nonmotor_data$date
cols <- c("County","City","Type","date","day","Location","value")
nonmotor <- nonmotor_data[,..cols]
nonmotor$year <- year(nonmotor$date)
nonmotor$value <- gsub("%","",nonmotor$value)
nonmotor$value <- as.numeric(nonmotor$value)
nonmotor$value <- nonmotor$value / 100
nonmotor_wsdot = nonmotor
nonmotor_wsdot$roll_mean = 0
nonmotor_wsdot$dataSource = "WSDOT"

#working with SDOT data
nonmotor_Seattle <- setDT(read.csv(nonmotorized_file_SDOT,stringsAsFactors=FALSE))
nonmotor_Seattle$date = mdy(nonmotor_Seattle$date)
nonmotor_Seattle$day = nonmotor_Seattle$date
nonmotor_Seattle$year <- year(nonmotor_Seattle$date)

#change 2019 'day' dates to 2020 - this will help to plot 2019 and 2020 bike counts on the same chart
for (row in 1:nrow(nonmotor_Seattle)){
  if (nonmotor_Seattle$year[row] == 2019){
    nonmotor_Seattle$day[row] = nonmotor_Seattle$day[row] %m+% years (1)
  }
}

nonmotor_Seattle$value <- as.numeric(nonmotor_Seattle$roll_mean)
nonmotor_Seattle$dataSource = "SDOT"
nonmotor_Seattle = na.omit(nonmotor_Seattle)
nonmotor_Seattle = as.data.frame(nonmotor_Seattle)
nonmotor_Seattle = as.data.table(nonmotor_Seattle)

nonmotor_SDOT_trail_list <- sort(unique(nonmotor_Seattle$dataSource))

nonmotor = rbind(nonmotor_wsdot,nonmotor_Seattle)

# create list for drop down of count locations
nonmotor_SDOT_trail_list <- sort(unique(nonmotor_Seattle$Location))
nonmotor_WSDOT_trail_list <- sort(unique(nonmotor_wsdot$Location))

place_choices = list('SDOT' = nonmotor_SDOT_trail_list,
                     'WSDOT' = nonmotor_WSDOT_trail_list)


nonmotor_count_locations <- sort(unique(nonmotor$Location))

nonmotor_latest_month <- max(month(nonmotor$day))
nonmotor_only_latest <- nonmotor[month(nonmotor$date) %in% nonmotor_latest_month,]
nonmotor_latest_day <- max(day(nonmotor_only_latest$day))
