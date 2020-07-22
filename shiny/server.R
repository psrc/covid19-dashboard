# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # headings
    output$airtravel_heading <- renderText({
        paste("Daily Passenger Screenings at our Nation's Airports")
    })
    
    output$TSASummary <- renderText({
        paste("Daily passenger screenings by the Transportation Security Adminstration have ranged from a maximum of ", format(round(max_tsa,-2), nsmall = 0, big.mark = ",")," to a minimum of ",format(round(min_tsa,-2), nsmall = 0, big.mark = ","), " since March 1st. At it's lowest point on April 14th, daily passenger screenings in 2020 were only ", round((return_estimate(passengers,"2020-04-14")/return_estimate(passengers,"2019-04-14"))*100,1),"% of the total passenger screenings from 2019. Passenger volumes at our nation's airports have been gradually increasing since late April and as of ",latest_month,"-",latest_day," passsenger screenings were ", round((return_estimate(passengers,ydm(paste("2020-",latest_day,"-",latest_month)))/return_estimate(passengers,ydm(paste("2019-",latest_day,"-",latest_month))))*100,1),"% of 2019 screenings.")
    })
    
    output$SEASummary <- renderText({
        paste("Daily passenger screenings by the Transportation Security Adminstration at Sea-Tac International Airport have ranged from a maximum of ", format(round(max_sea,-2), nsmall = 0, big.mark = ",")," to a minimum of ",format(round(min_sea,-2), nsmall = 0, big.mark = ","), " since the first week of January. At it's lowest point in the middle of April, daily passenger screenings in 2020 were ", round((return_estimate(seatac,"2020-04-15")/return_estimate(seatac,"2019-04-15"))*100,1),"% of the total passenger screenings from 2019. Passenger volumes at Sea-Tac International Airport have been gradually increasing since late April and as of ",sea_latest_month,"-",sea_latest_day," passsenger screenings were ", round((return_estimate(seatac,ydm(paste("2020-",sea_latest_day,"-",sea_latest_month)))/return_estimate(seatac,ydm(paste("2019-",sea_latest_day,"-",sea_latest_month))))*100,1),"% of 2019 screenings.")
    })
    
    output$TSABackground <- renderText({
        paste("TSA understands that the novel coronavirus (COVID-19) continues to weigh heavily on the minds of travelers and the general public. They have established a webpage to provide resources and information to assist passengers who find they must travel during this challenging time. For the latest press releases and statements related to COVID-19, please visit their website at https://www.tsa.gov/coronavirus.")
    })
    
    output$SEABackground <- renderText({
        paste("The Port of Seattle has been responding to the outbreak of 2019 Novel Coronavirus (COVID-19) since late January after public health officials confirmed the first case of the virus in the United States in Washington state. Since then the Port implemented protocols to maintain the health, safety, and well-being of our employees, travelers, and community members who use Port facilities while maintaining the essential functions of the Port. The Port of Seattle supports efforts to limit the spread of COVID-19 while maintaining essential operations. Summary data for airport operations are released weekly on Tuesday.")
    })
    
    
    output$TSABackground2 <- renderText({
        paste("To help provide relevant information about the impact of COVID-19 on air travel, the TSA provides daily updates at 9am EDT of passenger screenings from the previous day with a comparison to to same weekday in 2019.")
    })

    output$esdBackground <- renderText({
        paste("Initial unemployment claims are reported for each week based on the last day of the work week and are generally available early in the next week. Due to the high volume of claims and issues of fraud, over 20,000 calls are going into their phone center each day and wait times are very long, making it difficult for people to reach ESD at this time.")
    })

    output$TransitBackground <- renderText({
        paste("This data is provided to the Washington State Department of Transportation by regional transit agencies. Transit agencies that do not offer weekend service will show these days as gaps in their data. Normal is average daily ridership for the same day of week in the same month of the prior year - 2019. In order to meet the unprecedented need for near-real time information, almost no quality control is occurring relative to this data. Use with caution.")
    })

    output$FerryBackground <- renderText({
        paste("This data is provided by Washington State Department of Transportation Ferries division. Normal is average daily ridership for the same day of week in the same month of the prior year - 2019. In order to meet the unprecedented need for near-real time information, almost no quality control is occurring relative to this data. Use with caution.")
    })
    
    output$RailBackground <- renderText({
        paste("This data is provided by Washington State Department of Transportation. Normal is average daily ridership for the same day of week in the same month of the prior year - 2019. In order to meet the unprecedented need for near-real time information, almost no quality control is occurring relative to this data. Use with caution.")
    })

    output$VolumeBackground <- renderText({
        paste("This data is provided by Washington State Department of Transportation. Normal is average daily traffic for the same day of week in the same month of the prior year - 2019. In order to meet the unprecedented need for near-real time information, almost no quality control is occurring relative to this data. Use with caution.")
    })

    output$TruckBackground <- renderText({
        paste("This data is provided by Washington State Department of Transportation. Normal is average daily traffic for the same day of week in the same month of the prior year - 2019. In order to meet the unprecedented need for near-real time information, almost no quality control is occurring relative to this data. Use with caution.")
    })

    output$NonMotorBackground <- renderText({
        paste("This data is provided by Washington State Department of Transportation. Normal is the average for the same day of week in the same month of the prior year - 2019. In order to meet the unprecedented need for near-real time information, almost no quality control is occurring relative to this data. Use with caution.")
    })
    
        
    output$TransitSummary <- renderText({
        paste("Daily weekday transit boardings continue to be less than half of the daily transit boardings from 2019 for all operators in the region. One interesting item to note is that weekend boarding levels have averaged closer to 2019 levels than weekdays, likely reflecting the higher percentage of non-work transit trips that occur on weekends.")
    })
    
    # TSA Data
    output$chart_tsa <- renderPlotly({create_line_chart(passengers,"Daily Passenger Screenings",scales::comma, 0, c('#E3C9E3','#91268F'),"year",1,"")})
    
    output$TSA_March <- renderText({paste("March 1st: ", format(round(return_estimate(passengers,"2019-03-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-03-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-03-01")/return_estimate(passengers,"2019-03-01"))*100,0),"%")})
    output$TSA_April <- renderText({paste("April 1st: ", format(round(return_estimate(passengers,"2019-04-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-04-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-04-01")/return_estimate(passengers,"2019-04-01"))*100,0),"%")})
    output$TSA_May <- renderText({paste("May 1st: ", format(round(return_estimate(passengers,"2019-05-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-05-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-05-01")/return_estimate(passengers,"2019-05-01"))*100,0),"%")})
    output$TSA_June <- renderText({paste("June 1st: ", format(round(return_estimate(passengers,"2019-06-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-06-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-06-01")/return_estimate(passengers,"2019-06-01"))*100,0),"%")})
    output$TSA_July <- renderText({paste("July 1st: ", format(round(return_estimate(passengers,"2019-07-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-07-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-07-01")/return_estimate(passengers,"2019-07-01"))*100,0),"%")})
    output$TSA_August <- renderText({paste("August 1st: ", format(round(return_estimate(passengers,"2019-08-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-08-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-08-01")/return_estimate(passengers,"2019-08-01"))*100,0),"%")})
    output$TSA_September <- renderText({paste("September 1st: ", format(round(return_estimate(passengers,"2019-09-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-09-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-09-01")/return_estimate(passengers,"2019-09-01"))*100,0),"%")})
    output$TSA_October <- renderText({paste("October 1st: ", format(round(return_estimate(passengers,"2019-10-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-10-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-10-01")/return_estimate(passengers,"2019-10-01"))*100,0),"%")})
    output$TSA_November <- renderText({paste("November 1st: ", format(round(return_estimate(passengers,"2019-11-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-11-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-11-01")/return_estimate(passengers,"2019-11-01"))*100,0),"%")})
    output$TSA_December <- renderText({paste("December 1st: ", format(round(return_estimate(passengers,"2019-12-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-12-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-12-01")/return_estimate(passengers,"2019-12-01"))*100,0),"%")})
    output$TSA_Latest <- renderText({paste("Latest Data (",latest_month,"-",latest_day,"): ", format(round(return_estimate(passengers,ydm(paste("2019-",latest_day,"-",latest_month))),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,ydm(paste("2020-",latest_day,"-",latest_month))),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,ydm(paste("2020-",latest_day,"-",latest_month)))/return_estimate(passengers,ydm(paste("2019-",latest_day,"-",latest_month))))*100,1),"%")})

    # Sea-Tac Aiport Data
    output$chart_sea <- renderPlotly({create_line_chart(seatac,"Daily Passenger Screenings",scales::comma, 0, c('#E2F1CF','#8CC63E'),"year",1,"")})
    
    output$sea_March <- renderText({paste("1st week of March: ", format(round(return_estimate(seatac,"2019-03-04"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-03-04"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-03-04")/return_estimate(seatac,"2019-03-04"))*100,0),"%")})
    output$sea_April <- renderText({paste("1st week of April: ", format(round(return_estimate(seatac,"2019-04-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-04-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-04-01")/return_estimate(seatac,"2019-04-01"))*100,0),"%")})
    output$sea_May <- renderText({paste("1st week of May: ", format(round(return_estimate(seatac,"2019-05-06"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-05-06"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-05-06")/return_estimate(seatac,"2019-05-06"))*100,0),"%")})
    output$sea_June <- renderText({paste("1st week of June: ", format(round(return_estimate(seatac,"2019-06-03"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-06-03"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-06-03")/return_estimate(seatac,"2019-06-03"))*100,0),"%")})
    output$sea_July <- renderText({paste("1st week of July: ", format(round(return_estimate(seatac,"2019-07-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-07-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-07-01")/return_estimate(seatac,"2019-07-01"))*100,0),"%")})
    output$sea_August <- renderText({paste("1st week of August: ", format(round(return_estimate(seatac,"2019-08-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-08-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-08-01")/return_estimate(seatac,"2019-08-01"))*100,0),"%")})
    output$sea_September <- renderText({paste("1st week of September: ", format(round(return_estimate(seatac,"2019-09-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-09-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-09-01")/return_estimate(seatac,"2019-09-01"))*100,0),"%")})
    output$sea_October <- renderText({paste("1st week of October: ", format(round(return_estimate(seatac,"2019-10-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-10-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-10-01")/return_estimate(seatac,"2019-10-01"))*100,0),"%")})
    output$sea_November <- renderText({paste("1st week of November: ", format(round(return_estimate(seatac,"2019-11-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-11-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-11-01")/return_estimate(seatac,"2019-11-01"))*100,0),"%")})
    output$sea_December <- renderText({paste("1st week of December: ", format(round(return_estimate(seatac,"2019-12-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,"2020-12-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,"2020-12-01")/return_estimate(seatac,"2019-12-01"))*100,0),"%")})
    output$sea_Latest <- renderText({paste("Latest Data (",sea_latest_month,"-",sea_latest_day,"): ", format(round(return_estimate(seatac,ydm(paste("2019-",sea_latest_day,"-",sea_latest_month))),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(seatac,ydm(paste("2020-",sea_latest_day,"-",sea_latest_month))),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(seatac,ydm(paste("2020-",sea_latest_day,"-",sea_latest_month)))/return_estimate(seatac,ydm(paste("2019-",sea_latest_day,"-",sea_latest_month))))*100,1),"%")})

    # Transit Data
    output$chart_transit <- renderPlotly({create_line_chart(w_tbl=transit[variable %in% input$TransitOperators], "% of 2019 Daily Boardings", scales::percent, 1, c('#F05A28'), "variable",100,"%")})
    output$map_transit <- renderLeaflet({create_place_map(st_read(ptba_shapefile),input$TransitOperators)})
    
    output$transit_March <- renderText({paste("March 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-03-01")*100,0), nsmall = 0, big.mark = ","),"%")})
    output$transit_April <- renderText({paste("April 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-04-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_May <- renderText({paste("May 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-05-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_June <- renderText({paste("June 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-06-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_July <- renderText({paste("July 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-07-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_August <- renderText({paste("August 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-08-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_September <- renderText({paste("September 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-09-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_October <- renderText({paste("October 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-10-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_November <- renderText({paste("November 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-11-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    output$transit_December <- renderText({paste("December 1st: ", format(round(return_estimate(transit[variable %in% input$TransitOperators],"2020-12-01")*100,0), nsmall = 0, big.mark = ","), "%")})
    
    
    output$ct_Latest <- renderText({paste("Community Transit (",ct_latest_month,"-",ct_latest_day,"): ", format(round(return_single_estimate(transit,ydm(paste("2020-",ct_latest_day,"-",ct_latest_month)),"Community Transit"),1), nsmall = 0, big.mark = ","),"%")})
    output$et_Latest <- renderText({paste("Everett Transit (",et_latest_month,"-",et_latest_day,"): ", format(round(return_single_estimate(transit,ydm(paste("2020-",et_latest_day,"-",et_latest_month)),"Everett Transit"),1), nsmall = 0, big.mark = ","),"%")})
    output$kcm_Latest <- renderText({paste("King County Metro (",kcm_latest_month,"-",kcm_latest_day,"): ", format(round(return_single_estimate(transit,ydm(paste("2020-",kcm_latest_day,"-",kcm_latest_month)),"King County Metro"),1), nsmall = 0, big.mark = ","),"%")})    
    output$kt_Latest <- renderText({paste("Kitsap Transit (",kt_latest_month,"-",kt_latest_day,"): ", format(round(return_single_estimate(transit,ydm(paste("2020-",kt_latest_day,"-",kt_latest_month)),"Kitsap Transit"),1), nsmall = 0, big.mark = ","),"%")})
    output$ktf_Latest <- renderText({paste("Kitsap Fast Ferry (",ktf_latest_month,"-",ktf_latest_day,"): ", format(round(return_single_estimate(transit,ydm(paste("2020-",ktf_latest_day,"-",ktf_latest_month)),"Kitsap Fast Ferry"),1), nsmall = 0, big.mark = ","),"%")})
    output$pt_Latest <- renderText({paste("Pierce Transit (",pt_latest_month,"-",pt_latest_day,"): ", format(round(return_single_estimate(transit,ydm(paste("2020-",pt_latest_day,"-",pt_latest_month)),"Pierce Transit"),1), nsmall = 0, big.mark = ","),"%")})
    output$st_Latest <- renderText({paste("Sound Transit (",st_latest_month,"-",st_latest_day,"): ", format(round(return_single_estimate(transit,ydm(paste("2020-",st_latest_day,"-",st_latest_month)),"Sound Transit"),1), nsmall = 0, big.mark = ","),"%")})

    # Ferry Data
    output$chart_ferry <- renderPlotly({create_line_chart(ferry, "% of 2019 Daily Passengers", scales::percent, 1, c('#91268F','#F05A28',"#8CC63E","#00A7A0","#76787A","#BBBDC0"), "variable",100,"%")})
    output$bi_Latest <- renderText({paste("Bainbridge Island (",ferry_latest_month,"-",ferry_latest_day,"): ", format(round(return_single_estimate(ferry,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),"Seattle - Bainbridge Island"),1), nsmall = 0, big.mark = ","),"%")})
    output$brem_Latest <- renderText({paste("Bremerton (",ferry_latest_month,"-",ferry_latest_day,"): ", format(round(return_single_estimate(ferry,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),"Seattle - Bremerton"),1), nsmall = 0, big.mark = ","),"%")})
    output$faunt_Latest <- renderText({paste("Fauntleroy (",ferry_latest_month,"-",ferry_latest_day,"): ", format(round(return_single_estimate(ferry,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),"Fauntleroy - Vashon - Southworth"),1), nsmall = 0, big.mark = ","),"%")})    
    output$king_Latest <- renderText({paste("Edmonds (",ferry_latest_month,"-",ferry_latest_day,"): ", format(round(return_single_estimate(ferry,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),"Edmonds -  Kingston"),1), nsmall = 0, big.mark = ","),"%")})
    output$muki_Latest <- renderText({paste("Mukilteo (",ferry_latest_month,"-",ferry_latest_day,"): ", format(round(return_single_estimate(ferry,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),"Mukilteo - Clinton"),1), nsmall = 0, big.mark = ","),"%")})
    output$ptdef_Latest <- renderText({paste("Pt Defiance (",ferry_latest_month,"-",ferry_latest_day,"): ", format(round(return_single_estimate(ferry,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),"Point Defiance - Tahlequah"),1), nsmall = 0, big.mark = ","),"%")})
    
    # Rail Data
    output$chart_rail <- renderPlotly({create_line_chart(rail, "Daily Passengers", scales::comma, 1, c('#FBD6C9','#F05A28'), "year",1,"")})
    output$rail_Latest <- renderText({paste("Amtrak Cascades (",rail_latest_month,"-",rail_latest_day,"): ", format(round(return_estimate(rail,ydm(paste("2019-",rail_latest_day,"-",rail_latest_month))),-1), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(rail,ydm(paste("2020-",rail_latest_day,"-",rail_latest_month))),-1), nsmall = 0, big.mark = ","), "/", round((return_estimate(rail,ydm(paste("2020-",rail_latest_day,"-",rail_latest_month)))/return_estimate(rail,ydm(paste("2019-",rail_latest_day,"-",rail_latest_month))))*100,1),"%")})
    
    # Unemployment Data
    output$chart_unemployment <- renderPlotly({create_line_chart(w_tbl=unemployment[variable %in% input$ClaimTypes],input$ClaimTypes,scales::comma, 0, c('#C0E095','#8CC63E'),"year",1,"")})
    
    output$unemployment_March <- renderText({paste("1st week of March: ", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-03-02"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-02-29"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-02-29")/return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-03-02"))*100,0),"%")})
    output$unemployment_April <- renderText({paste("1st week of April: ", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-03-30"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-03-28"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-03-28")/return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-03-30"))*100,0),"%")})
    output$unemployment_May <- renderText({paste("1st week of May: ", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-04-27"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-05-02"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-05-02")/return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-04-27"))*100,0),"%")})
    output$unemployment_June <- renderText({paste("1st week of June: ", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-06-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-05-30"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment[variable %in% c("Initial Claims")],"2020-05-30")/return_estimate(unemployment[variable %in% c("Initial Claims")],"2019-06-01"))*100,0),"%")})
    output$unemployment_Latest <- renderText({paste("Latest Data (",esd_latest_month_current,"-",esd_latest_day_current,"): ", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],ydm(paste("2019-",esd_latest_day_prior,"-",esd_latest_month_prior))),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment[variable %in% c("Initial Claims")],ydm(paste("2020-",esd_latest_day_current,"-",esd_latest_month_current))),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment[variable %in% c("Initial Claims")],ydm(paste("2020-",esd_latest_day_current,"-",esd_latest_month_current)))/return_estimate(unemployment[variable %in% c("Initial Claims")],ydm(paste("2019-",esd_latest_day_prior,"-",esd_latest_month_prior))))*100,0),"%")})

    output$chart_unemployment_industry <- renderPlotly({create_bar_chart(industries,"Industry" ,"Total Continuing Claims", "stack", 0, "yes", scales::comma,1,"")})
    output$chart_unemployment_race_share <- renderPlotly({create_bar_chart(race[race$variable == "Share"],"Race" ,"Unemployed Share of Labor Force", "stack", 1, "no", scales::percent,100,"%")})
    output$chart_unemployment_race_total <- renderPlotly({create_bar_chart(race[race$variable == "Continued Claims"],"Race" ,"Total Continuing Claims", "stack", 0, "no", scales::comma,1,"")})
    
    # Traffic Data
    output$chart_volumes <- renderPlotly({create_line_chart(w_tbl=volumes[Location %in% input$CountLocations],"Daily Traffic",scales::comma, 0, c('#BFE9E7','#00A7A0'),"year",1,"")})
    output$volumes_March <- renderText({paste("March 1st: ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2019))*100,1),"%")})
    output$volumes_April <- renderText({paste("April 1st: ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2019))*100,1),"%")})
    output$volumes_May <- renderText({paste("May 1st: ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2019))*100,1),"%")})    
    output$volumes_Latest <- renderText({paste("Latest Data (",volumes_latest_month,"-",volumes_latest_day,"): ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2019))*100,1),"%")})
    
    # Truck Data
    output$chart_trucks <- renderPlotly({create_line_chart(w_tbl=trucks[Location %in% input$TruckLocations],"% of 2019 Daily Truck Traffic",scales::percent, 1, c('#91268F','#F05A28'),"year",100,"%")})
    output$trucks_March <- renderText({paste("March 1st: ", format(round(return_matching_day(w_tbl=trucks[Location %in% input$TruckLocations],"2020-03-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$trucks_April <- renderText({paste("April 1st: ", format(round(return_matching_day(w_tbl=trucks[Location %in% input$TruckLocations],"2020-04-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$trucks_May <- renderText({paste("May 1st: ", format(round(return_matching_day(w_tbl=trucks[Location %in% input$TruckLocations],"2020-05-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$trucks_June <- renderText({paste("June 1st: ", format(round(return_matching_day(w_tbl=trucks[Location %in% input$TruckLocations],"2020-06-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$trucks_Latest <- renderText({paste("Latest Data (",trucks_latest_month,"-",trucks_latest_day,"): ", format(round(return_matching_day(w_tbl=trucks[Location %in% input$TruckLocations],w_day=ydm(paste("2020-",trucks_latest_day,"-",trucks_latest_month)),2020),2)*100, nsmall = 0, big.mark = ","),"%")})

    # Non-Motorized Data
    
    output$chart_nonmotor <- renderPlotly({ if (input$NonMotorLocations %in% nonmotor_SDOT_trail_list ) {
        create_line_chart(w_tbl=nonmotor[nonmotor$Location %in% input$NonMotorLocations,],"7-Day Average",scales::comma, 0, c('#E3C9E3','#91268F'),"year",1,"","Bicycle Counts, 7-day Average")
    } else { 
        create_line_chart(w_tbl=nonmotor[nonmotor$Location %in% input$NonMotorLocations,],"% of 2019 Daily Non-Motorized Counts",scales::percent, 0, c('#91268F'),"year",100,"%")}
        })
    output$nonmotor_March <- renderText({paste("March 1st: ", format(round(return_matching_day(w_tbl=nonmotor[nonmotor$Location %in% input$NonMotorLocations,],"2020-03-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$nonmotor_April <- renderText({paste("April 1st: ", format(round(return_matching_day(w_tbl=nonmotor[nonmotor$Location %in% input$NonMotorLocations,],"2020-04-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$nonmotor_May <- renderText({paste("May 1st: ", format(round(return_matching_day(w_tbl=nonmotor[nonmotor$Location %in% input$NonMotorLocations,],"2020-05-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$nonmotor_June <- renderText({paste("June 1st: ", format(round(return_matching_day(w_tbl=nonmotor[nonmotor$Location %in% input$NonMotorLocations,],"2020-06-01",2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    output$nonmotor_Latest <- renderText({paste("Latest Data (",nonmotor_latest_month,"-",nonmotor_latest_day,"): ", format(round(return_matching_day(w_tbl=nonmotor[nonmotor$Location %in% input$NonMotorLocations,],w_day=ydm(paste("2020-",nonmotor_latest_day,"-",nonmotor_latest_month)),2020),2)*100, nsmall = 0, big.mark = ","),"%")})
    
    # Summary Statistics for Initial Page
    output$screenings_summary <- renderText({paste("Nationwide Airport Screenings: ", round((return_estimate(passengers,ydm(paste("2020-",latest_day,"-",latest_month)))/return_estimate(passengers,ydm(paste("2019-",latest_day,"-",latest_month))))*100,0)-100,"%")})
    output$unemployment_summary <- renderText({paste("Initial Jobless Claims Statewide: ", round((return_estimate(unemployment[variable %in% c("Initial Claims")],ydm(paste("2020-",esd_latest_day_current,"-",esd_latest_month_current)))/return_estimate(unemployment[variable %in% c("Initial Claims")],ydm(paste("2019-",esd_latest_day_prior,"-",esd_latest_month_prior))))*100,0),"%")})
    output$transit_summary <- renderText({paste("Major Transit Operators Statewide: ", format(round(return_single_estimate(transit_data,ydm(paste("2020-",all_tran_latest_day,"-",all_tran_latest_month)),"Average"),1), nsmall = 0, big.mark = ","),"%")})
    output$rail_summary <- renderText({paste("Amtrak Cascades: ", round((return_estimate(rail,ydm(paste("2020-",rail_latest_day,"-",rail_latest_month)))/return_estimate(rail,ydm(paste("2019-",rail_latest_day,"-",rail_latest_month))))*100,0)-100,"%")})
    output$ferry_summary <- renderText({paste("Central Puget Sound Ferry Routes: ", format(round((round(return_single_estimate(ferry_ridership_2020,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),psrc_ferry),0) - round(return_single_estimate(ferry_ridership_2019,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),psrc_ferry),0))/ round(return_single_estimate(ferry_ridership_2019,ydm(paste("2020-",ferry_latest_day,"-",ferry_latest_month)),psrc_ferry),0)*100,0), nsmall = 0, big.mark = ","),"%")})
    output$volume_summary <- renderText({paste("Central Puget Sound Highways: ", format(round(((round(return_matching_day(w_tbl=volumes[Location %in% count_locations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2020),-2)-round(return_matching_day(w_tbl=volumes[Location %in% count_locations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2019),-2))/round(return_matching_day(w_tbl=volumes[Location %in% count_locations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2019),-2))*100,0), nsmall = 0, big.mark = ","),"%")})
    
})    
