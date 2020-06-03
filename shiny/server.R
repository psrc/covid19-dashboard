# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # headings
    output$airtravel_heading <- renderText({
        paste("Daily Passenger Screenings at our Nation's Airports")
    })
    
    output$TSALink <- renderText({
        paste("Source: https://www.tsa.gov/coronavirus/passenger-throughput")
    })
    
    output$TSASummary <- renderText({
        paste("Daily passenger screenings by the Transportation Security Adminstration have ranged from a maximum of ", format(round(max_tsa,-2), nsmall = 0, big.mark = ",")," to a minimum of ",format(round(min_tsa,-2), nsmall = 0, big.mark = ","), " since March 1st. At it's lowest point on April 14th, daily passenger screenings in 2020 were only ", round((return_estimate(passengers,"2020-04-14")/return_estimate(passengers,"2019-04-14"))*100,1),"% of the total passenger screenings from 2019. Passenger volumes at our nation's airports have been gradually increasing since late April and as of ",latest_month,"-",latest_day," passsenger screenings were ", round((return_estimate(passengers,ydm(paste("2020-",latest_day,"-",latest_month)))/return_estimate(passengers,ydm(paste("2019-",latest_day,"-",latest_month))))*100,1),"% of 2019 screenings.")
    })
    
    output$TSABackground <- renderText({
        paste("TSA understands that the novel coronavirus (COVID-19) continues to weigh heavily on the minds of travelers and the general public. They have established a webpage to provide resources and information to assist passengers who find they must travel during this challenging time. For the latest press releases and statements related to COVID-19, please visit their website at https://www.tsa.gov/coronavirus.")
    })
    
    output$TSABackground2 <- renderText({
        paste("To help provide relevant information about the impact of COVID-19 on air travel, the TSA provides daily updates at 9am EDT of passenger screenings from the previous day with a comparison to to same weekday in 2019.")
    })

    output$esdBackground <- renderText({
        paste("Initial unemployment claims are reported for each week based on the last day of the work week and are generally available early in the next week. Due to the high volume of claims and issues of fraud, over 20,000 calls are going into their phone center each day and wait times are very long, making it difficult for people to reach ESD at this time.")
    })

    output$esdLink <- renderText({
        paste("Source: https://esd.wa.gov/newsroom/unemployment-statistics")
    })
        
    output$TransitLink <- renderText({
        paste("Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/")
    })
    
    output$FerryLink <- renderText({
        paste("Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/")
    })
    
    output$RailLink <- renderText({
        paste("Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/")
    })
    
    output$VolumeLink <- renderText({
        paste("Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/")
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
    
    output$TransitSummary <- renderText({
        paste("Daily weekday transit boardings continue to be less than half of the daily transit boardings from 2019 for all operators in the region. One interesting item to note is that weekend boarding levels have averaged closer to 2019 levels than weekdays, likely reflecting the higher percentage of non-work transit trips that occur on weekends.")
    })
    
        
    # TSA Data
    output$chart_tsa <- renderPlotly({create_line_chart(passengers,"Daily Passenger Screenings",scales::comma, 0, c('#91268F','#F05A28'),"year",1,"")})
    
    output$TSA_March <- renderText({paste("March 1st: ", format(round(return_estimate(passengers,"2019-03-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-03-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-03-01")/return_estimate(passengers,"2019-03-01"))*100,1),"%")})
    output$TSA_April <- renderText({paste("April 1st: ", format(round(return_estimate(passengers,"2019-04-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-04-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-04-01")/return_estimate(passengers,"2019-04-01"))*100,1),"%")})
    output$TSA_May <- renderText({paste("May 1st: ", format(round(return_estimate(passengers,"2019-05-01"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,"2020-05-01"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,"2020-05-01")/return_estimate(passengers,"2019-05-01"))*100,1),"%")})
    output$TSA_Latest <- renderText({paste("Latest Data (",latest_month,"-",latest_day,"): ", format(round(return_estimate(passengers,ydm(paste("2019-",latest_day,"-",latest_month))),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(passengers,ydm(paste("2020-",latest_day,"-",latest_month))),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(passengers,ydm(paste("2020-",latest_day,"-",latest_month)))/return_estimate(passengers,ydm(paste("2019-",latest_day,"-",latest_month))))*100,1),"%")})
    
    # Tranit Data
    output$chart_transit <- renderPlotly({create_line_chart(transit, "% of 2019 Daily Boardings", scales::percent, 1, c('#91268F','#F05A28',"#8CC63E","#00A7A0","#00A7A0","#76787A","#BBBDC0"), "variable",100,"%")})
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
    output$chart_rail <- renderPlotly({create_line_chart(rail, "% of 2019 Daily Passengers", scales::percent, 1, c('#91268F','#F05A28',"#8CC63E","#00A7A0","#76787A","#BBBDC0"), "variable",100,"%")})
    output$rail_Latest <- renderText({paste("Amtrak Cascades (",rail_latest_month,"-",rail_latest_day,"): ", format(round(return_single_estimate(rail,ydm(paste("2020-",rail_latest_day,"-",rail_latest_month)),"Amtrak"),1), nsmall = 0, big.mark = ","),"%")})

    # Unemployment Data
    output$chart_unemployment <- renderPlotly({create_line_chart(unemployment,"Initial Claims",scales::comma, 0, c('#91268F','#F05A28'),"year",1,"")})
    
    output$unemployment_March <- renderText({paste("1st week of March: ", format(round(return_estimate(unemployment,"2019-03-02"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment,"2020-02-29"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment,"2020-02-29")/return_estimate(unemployment,"2019-03-02"))*100,1),"%")})
    output$unemployment_April <- renderText({paste("1st week of April: ", format(round(return_estimate(unemployment,"2019-03-30"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment,"2020-03-28"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment,"2020-03-28")/return_estimate(unemployment,"2019-03-30"))*100,1),"%")})
    output$unemployment_May <- renderText({paste("1st week of May: ", format(round(return_estimate(unemployment,"2019-04-27"),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment,"2020-05-02"),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment,"2020-05-02")/return_estimate(unemployment,"2019-04-27"))*100,1),"%")})
    output$unemployment_Latest <- renderText({paste("Latest Data (",esd_latest_month_current,"-",esd_latest_day_current,"): ", format(round(return_estimate(unemployment,ydm(paste("2019-",esd_latest_day_prior,"-",esd_latest_month_prior))),-2), nsmall = 0, big.mark = ","), "/", format(round(return_estimate(unemployment,ydm(paste("2020-",esd_latest_day_current,"-",esd_latest_month_current))),-2), nsmall = 0, big.mark = ","), "/", round((return_estimate(unemployment,ydm(paste("2020-",esd_latest_day_current,"-",esd_latest_month_current)))/return_estimate(unemployment,ydm(paste("2019-",esd_latest_day_prior,"-",esd_latest_month_prior))))*100,1),"%")})

    # Traffic Data
    output$chart_volumes <- renderPlotly({create_line_chart(w_tbl=volumes[Location %in% input$CountLocations],"Daily Traffic",scales::comma, 0, c('#91268F','#F05A28'),"year",1,"")})
    output$volumes_March <- renderText({paste("March 1st: ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-03-01",w_year=2019))*100,1),"%")})
    output$volumes_April <- renderText({paste("April 1st: ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-04-01",w_year=2019))*100,1),"%")})
    output$volumes_May <- renderText({paste("May 1st: ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day="2020-05-01",w_year=2019))*100,1),"%")})    
    output$volumes_Latest <- renderText({paste("Latest Data (",volumes_latest_month,"-",volumes_latest_day,"): ", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2019),-2), nsmall = 0, big.mark = ","), "/", format(round(return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2020),-2), nsmall = 0, big.mark = ","), "/", round((return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2020)/return_matching_day(w_tbl=volumes[Location %in% input$CountLocations],w_day=ydm(paste("2020-",volumes_latest_day,"-",volumes_latest_month)),w_year=2019))*100,1),"%")})
    
    
            
})    
