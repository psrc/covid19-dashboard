shinyUI(
navbarPage(title=div(img(src="psrc-logo.png", width = "20%", height = "20%", style = "float: right;")),
          windowTitle = "COVID-19 Data Dashboard", 
          theme = "styles.css",

           tabPanel("Overview",
                    sidebarLayout(
                      sidebarPanel(
                        h2("Summary of Impacts:"),
                        "(compared to same time period in 2019)",
                        hr(),
                        tags$div(class="sidebar_headings","Unemployment Claims"),
                        tags$div(class="sidebar_data",textOutput("unemployment_summary")),
                        tags$div(class="sidebar_headings","Airport Usage"),
                        tags$div(class="sidebar_data",textOutput("screenings_summary")),
                        tags$div(class="sidebar_headings","Transit Usage"),
                        tags$div(class="sidebar_data",textOutput("transit_summary")),
                        tags$div(class="sidebar_headings","Ferry Usage"),
                        tags$div(class="sidebar_data",textOutput("ferry_summary")),
                        tags$div(class="sidebar_headings","Passsenger Rail Usage"),
                        tags$div(class="sidebar_data",textOutput("rail_summary")),
                        tags$div(class="sidebar_headings","Highway Usage"),
                        tags$div(class="sidebar_data",textOutput("volume_summary")),
                        br(),
                        width=3),
                      mainPanel(
                        h1("Economic and Travel Impacts of COVID-19"),
                        fluidRow(column(12,div(img(src="covid-mural.jpg", width = "75%", height = "75%", style = "padding-top: 25px")))
                        ),
                        br(),
                        "Travel in Washington state has declined significantly since mid-March as the citizens of Washington have responded to various COVID-19 related initiatives such as closing schools, restricting gatherings to fewer than 50 people and the Stay Home - Stay Healthy Executive Order on March 25. This dashboard compiles data from numerous sources daily to help summarize travel and economic impacts for the Central Puge Sound region.",
                        "Many of the data sources on these pages are updated daily - a direct response from many levels of government to provide timely information on impacts of this pandemic. Data is continually updated by these agencies and as such caution should be used when using the information on these pages as it can change as issues are discovered.",
                        br(),
                        br(),
                        "Many of the travel impacts reported here have been summarized from the amazing data that the Washington State Department of Transportation has gathered and processed from numerous agencies across the state. Each source is documented on the specific dashboard page with a direct link to the data but if you wish to see more detailed information, we recommend that you check out WSDOT's COVID-19 Multimodal Transportation System Performance Dashboard at https://www.wsdot.wa.gov/about/covid-19-transportation-report/",
                        br(),
                      ) # End of Overview Panel
                    ) # End of Sidebar of Overview Panel
           ),# End of Tab Panel of Overview Panel
           
           tabPanel(icon("briefcase"),
                    sidebarLayout(
                      sidebarPanel(
                        h2("Unemployment Claims:"),
                        "(compared to same time period in 2019)",
                        hr(),
                        selectInput("ClaimTypes","Select the Data you would like to see:",claim_types),
                        hr(),
                        strong("Weekly Initial Jobless Claims:"),
                        strong("(2019 / 2020 / Ratio)"),
                        tags$div(class="sidebar_data",textOutput("unemployment_April")),
                        tags$div(class="sidebar_data",textOutput("unemployment_May")),
                        tags$div(class="sidebar_data",textOutput("unemployment_June")),
                        tags$div(class="sidebar_data",textOutput("unemployment_July")),
                        tags$div(class="sidebar_data",textOutput("unemployment_Latest")),
                        hr(),
                        strong("Notes on Initial Jobless Claims:"),
                        br(),
                        tags$div(class="sidebar_notes",textOutput("esdBackground")),
                        hr(),
                        div(img(src="esd.jpg", width = "30%", height = "30%", style = "padding-top: 5px")),
                        width=3),
                      mainPanel(
                        fluidRow(column(4,div(img(src="unemployment_1.jpg", width = "55%", height = "55%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="unemployment_2.jpg", width = "60%", height = "60%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="unemployment_3.jpg", width = "70%", height = "70%", style = "padding-top: 25px")))
                        ),
                        br(),
                        h2("Initial Unemployment Claims"),
                        fluidRow(column(12,plotlyOutput("chart_unemployment"))),
                        fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://esd.wa.gov/newsroom/unemployment-statistics", "Source: https://esd.wa.gov/newsroom/unemployment-statistics"))),
                        br(),
                        h2("Continuing Unemployment Claims by Race"),
                        fluidRow(column(6,plotlyOutput("chart_unemployment_race_total")),
                                 column(6,plotlyOutput("chart_unemployment_race_share"))
                        ),
                        fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://esd.wa.gov/labormarketinfo/unemployment-insurance-data", "https://esd.wa.gov/labormarketinfo/unemployment-insurance-data"))),
                        br(),
                        h2("Continuing Unemployment Claims by Industry"),
                        fluidRow(column(12,plotlyOutput("chart_unemployment_industry"))),
                        fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://esd.wa.gov/labormarketinfo/unemployment-insurance-data", "https://esd.wa.gov/labormarketinfo/unemployment-insurance-data")))
                        
                        
                      ) # End of Main Panel of Unemployment
                    ) # End of Sidebar of Unemployment
           ),# End of Tab Panel of Unemployment

          tabPanel(icon("plane-departure"),
                   sidebarLayout(
                     sidebarPanel(
                       h2("Airport Screenings:"),
                       "(compared to same time period in 2019)",
                       hr(),
                       strong("Sea-Tac Daily Screenings:"),
                       strong("(2019 / 2020 / Ratio)"),
                       tags$div(class="sidebar_data",textOutput("sea_April")),
                       tags$div(class="sidebar_data",textOutput("sea_May")),
                       tags$div(class="sidebar_data",textOutput("sea_June")),
                       tags$div(class="sidebar_data",textOutput("sea_July")),
                       tags$div(class="sidebar_data",textOutput("sea_Latest")),
                       hr(),
                       strong("National Daily Screenings:"),
                       strong("(2019 / 2020 / Ratio)"),
                       tags$div(class="sidebar_data",textOutput("TSA_April")),
                       tags$div(class="sidebar_data",textOutput("TSA_May")),
                       tags$div(class="sidebar_data",textOutput("TSA_June")),
                       tags$div(class="sidebar_data",textOutput("TSA_July")),
                       tags$div(class="sidebar_data",textOutput("TSA_Latest")),
                       hr(),
                       strong("Notes on Sea-Tac Screening Data:"),
                       br(),
                       tags$div(class="sidebar_notes",textOutput("SEABackground")),
                       br(),
                       strong("Notes on National Screening Data:"),
                       br(),
                       tags$div(class="sidebar_notes",textOutput("TSABackground")),
                       br(),
                       tags$div(class="sidebar_notes",textOutput("TSABackground2")),
                       hr(),
                       fluidRow(column(4,div(img(src="logo-port_0.png", width = "75%", height = "75%", style = "padding-top: 5px"))),
                                column(4,div(img(src="tsa_logo.svg", width = "75%", height = "75%", style = "padding-top: 5px")))
                       ),
                       width=3),
                     mainPanel(
                       fluidRow(column(4,div(img(src="seatacterminal.jpg", width = "70%", height = "70%", style = "padding-top: 25px"))),
                                column(4,div(img(src="alaska-paine-field_0.jpg", width = "70%", height = "70%", style = "padding-top: 25px"))),
                                column(4,div(img(src="painefieldterminal_int_big.jpg", width = "70%", height = "70%", style = "padding-top: 25px")))
                       ),
                       br(),
                       h2("Sea-Tac International Airport Average Weekday Passenger Screenings"),
                       textOutput("SEASummary"),
                       fluidRow(column(12,plotlyOutput("chart_sea"))),
                       fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.portseattle.org/page/airport-statistics#:~:text=Operated%20by%20the%20Port%20of,of%20air%20cargo%20in%202019.", "Source: https://www.portseattle.org/page/airport-statistics"))),
                       br(),
                       h2("National Airport Average Weekday Passenger Screenings"),
                       textOutput("TSASummary"),
                       fluidRow(column(12,plotlyOutput("chart_tsa"))),
                       fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.tsa.gov/coronavirus/passenger-throughput", "Source: https://www.tsa.gov/coronavirus/passenger-throughput")))
                       
                     ) # End of Main Panel of Passenger Screening
                   ) # End of Sidebar of Passenger Screening
          ),# End of Tab Panel of Passenger Screening
          
        tabPanel(icon("bus"),
                 sidebarLayout(
                    sidebarPanel(
                      h2("Transit Boardings by Operator:"),
                      "(compared to same time period in 2019)",
                      hr(),
                      selectInput("TransitOperators","Please select a Transit Agency:",psrc_agencies),
                      hr(),
                      strong("Percentage of 2019 Daily Boardings by Operator"),
                      tags$div(class="sidebar_data",textOutput("transit_April")),
                      tags$div(class="sidebar_data",textOutput("transit_May")),
                      tags$div(class="sidebar_data",textOutput("transit_June")),
                      tags$div(class="sidebar_data",textOutput("transit_July")),
                      hr(),
                      strong("Notes on Transit Data:"),
                      tags$div(class="sidebar_notes",textOutput("TransitBackground")),
                      hr(),
                      fluidRow(column(4,div(img(src="everett-transit.png", width = "55%", height = "55%", style = "padding-top: 5px"))),
                               column(4,div(img(src="kitsap-transit.jpg", width = "40%", height = "40%", style = "padding-top: 5px"))),
                               column(4,div(img(src="pierce-transit.jpg", width = "40%", height = "40%", style = "padding-top: 5px")))
                      ),
                      fluidRow(column(4,div(img(src="community-transit.png", width = "80%", height = "80%", style = "padding-top: 25px"))),
                               column(4,div(img(src="kc-metro.png", width = "55%", height = "55%", style = "padding-top: 25px"))),
                               column(4,div(img(src="sound-transit.png", width = "55%", height = "55%", style = "padding-top: 25px")))
                      ),
                      width=3),
                    mainPanel(
                      fluidRow(column(4,div(img(src="bellevuetransitcenter.jpg", width = "80%", height = "80%", style = "padding-top: 5px"))),
                               column(4,div(img(src="ct.jpg", width = "80%", height = "80%", style = "padding-top: 5px"))),
                               column(4,div(img(src="link.jpg", width = "80%", height = "80%", style = "padding-top: 5px")))
                      ),
                      br(),
                      textOutput("TransitSummary"),
                      fluidRow(column(8,plotlyOutput("chart_transit")),
                               column(4,leafletOutput("map_transit"))),
                      fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/transit/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/transit/default.htm")))
                    ) #End of Main Panel
                 ) # End of Sidebar Layout for Transit
        ), # End of Transit Tab Panel

        tabPanel(icon("ship"),
                 sidebarLayout(
                   sidebarPanel(
                     h2("Ferry Boardings by Operator:"),
                     "(compared to same time period in 2019)",
                     hr(),
                     strong("Percentage of 2019 Daily Boardings by Operator"),
                     tags$div(class="sidebar_data",textOutput("bi_Latest")),
                     tags$div(class="sidebar_data",textOutput("brem_Latest")),
                     tags$div(class="sidebar_data",textOutput("faunt_Latest")),
                     tags$div(class="sidebar_data",textOutput("king_Latest")),
                     tags$div(class="sidebar_data",textOutput("muki_Latest")),
                     tags$div(class="sidebar_data",textOutput("ptdef_Latest")),
                     hr(),
                     strong("Notes on Ferry Data:"),
                     tags$div(class="sidebar_notes",textOutput("FerryBackground")),
                     hr(),
                     fluidRow(column(4,div(img(src="wsf.png", width = "45%", height = "45%", style = "padding-top: 5px"))),
                              column(4,div(img(src="kitsap-transit.jpg", width = "40%", height = "40%", style = "padding-top: 5px")))
                     ),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="ferries.jpg", width = "40%", height = "40%", style = "padding-top: 5px"))),
                              column(4,div(img(src="ferry.jpg", width = "55%", height = "55%", style = "padding-top: 5px"))),
                              column(4,div(img(src="fast_ferry_terminal.jpg", width = "55%", height = "55%", style = "padding-top: 5px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_ferry"))),
                     fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/ferries/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/ferries/default.htm")))
                   ) #End of Main Panel
                 ) # End of Sidebar Layout for Ferry
        ), # End of Ferry Tab Panel

        tabPanel(icon("train"),
                 sidebarLayout(
                   sidebarPanel(
                     h2("Passenger Rail Boardings:"),
                     "(compared to same time period in 2019)",
                     hr(),
                     strong("Daily Passenger Volumes:"),
                     strong("(2019 / 2020 / Ratio)"),
                     tags$div(class="sidebar_data",textOutput("rail_April")),
                     tags$div(class="sidebar_data",textOutput("rail_May")),
                     tags$div(class="sidebar_data",textOutput("rail_June")),
                     tags$div(class="sidebar_data",textOutput("rail_July")),
                     tags$div(class="sidebar_data",textOutput("rail_Latest")),
                     hr(),
                     strong("Notes on Rail Data:"),
                     tags$div(class="sidebar_notes",textOutput("RailBackground")),
                     br(),
                     fluidRow(column(12,div(img(src="amtrak_logo.jpg", width = "25%", height = "25%", style = "padding-top: 5px")))
                     ),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="seattleamtrak.jpg", width = "80%", height = "80%", style = "padding-top: 5px"))),
                              column(4,div(img(src="amtrak_2.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="amtrak_3.jpg", width = "55%", height = "55%", style = "padding-top: 5px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_rail"))),
                     fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/rail/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/rail/default.htm")))
                   ) #End of Main Panel
                 ) # End of Sidebar Layout for Rail
        ), # End of Rail Tab Panel        

        tabPanel(icon("car"),
                 sidebarLayout(
                   sidebarPanel(
                     h2("Daily Highway Volumes:"),
                     "(compared to same time period in 2019)",
                     hr(),
                     selectInput("CountLocations","Select the count location:",count_locations),
                     hr(),
                     strong("Daily Traffic Volumes:"),
                     strong("(2019 / 2020 / Ratio)"),
                     tags$div(class="sidebar_data",textOutput("volumes_April")),
                     tags$div(class="sidebar_data",textOutput("volumes_May")),
                     tags$div(class="sidebar_data",textOutput("volumes_June")),
                     tags$div(class="sidebar_data",textOutput("volumes_July")),
                     tags$div(class="sidebar_data",textOutput("volumes_Latest")),
                     hr(),
                     strong("Notes on Daily Traffic Volumes:"),
                     br(),
                     tags$div(class="sidebar_notes",textOutput("VolumeBackground")),
                     br(),
                     div(img(src="wsdot.png", width = "15%", height = "15%", style = "padding-top: 5px")),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="i-90mercerisland_1.jpg", width = "75%", height = "75%", style = "padding-top: 25px"))),
                              column(4,div(img(src="access-ramp_0.jpg", width = "75%", height = "75%", style = "padding-top: 25px"))),
                              column(4,div(img(src="bremertonmanettebridge.jpg", width = "75%", height = "75%", style = "padding-top: 25px")))
                     ),
                     br(),
                     fluidRow(column(8,plotlyOutput("chart_volumes")),
                              column(4,leafletOutput("map_volume_locations"))),
                     fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/highway/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/highway/default.htm")))
                   ) # End of Main Panel of Traffic Volumes
                 ) # End of Sidebar of Traffic Volumes
        ),# End of Tab Panel of Traffic Volumes

        tabPanel(icon("truck"),
                 sidebarLayout(
                   sidebarPanel(
                     h2("Daily Truck Volumes:"),
                     "(compared to same time period in 2019)",
                     hr(),
                     selectInput("TruckLocations","Select the Truck count location:",truck_count_locations),
                     hr(),
                     strong("Percentage of 2019 Daily Truck Volumes"),
                     tags$div(class="sidebar_data",textOutput("trucks_April")),
                     tags$div(class="sidebar_data",textOutput("trucks_May")),
                     tags$div(class="sidebar_data",textOutput("trucks_June")),
                     tags$div(class="sidebar_data",textOutput("trucks_July")),
                     tags$div(class="sidebar_data",textOutput("trucks_Latest")),
                     hr(),
                     strong("Notes on Daily Truck Volumes:"),
                     br(),
                     tags$div(class="sidebar_notes", textOutput("TruckBackground")),
                     br(),
                     div(img(src="wsdot.png", width = "15%", height = "15%", style = "padding-top: 5px")),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="autonomous_truck.jpg", width = "75%", height = "75%", style = "padding-top: 25px"))),
                              column(8,div(img(src="trucks.jpg", width = "50%", height = "50%", style = "padding-top: 25px")))
                     ),
                     br(),
                     fluidRow(column(8,plotlyOutput("chart_trucks")),
                              column(4,leafletOutput("map_truck_locations"))),
                     fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/freight/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/freight/default.htm")))
                   ) # End of Main Panel of Truck Volumes
                 ) # End of Sidebar of Truck Volumes
        ),# End of Tab Panel of Truck Volumes
        
        tabPanel(icon("walking"),
                 sidebarLayout(
                   sidebarPanel(
                     h2("Daily Non-Motirzed Counts:"),
                     "(compared to same time period in 2019)",
                     hr(),
                     selectInput("NonMotorLocations","Select the Non-Motorized count location:",place_choices),
                     hr(),
                     strong("Percentage of 2019 Non-Motorized Counts"),
                     tags$div(class="sidebar_data",textOutput("nonmotor_April")),
                     tags$div(class="sidebar_data",textOutput("nonmotor_May")),
                     tags$div(class="sidebar_data",textOutput("nonmotor_June")),
                     tags$div(class="sidebar_data",textOutput("nonmotor_July")),
                     tags$div(class="sidebar_data",textOutput("nonmotor_Latest")),
                     hr(),
                     strong("Notes on Non-Motorized Counts:"),
                     br(),
                     tags$div(class="sidebar_notes", textOutput("NonMotorBackground")),
                     br(),
                     fluidRow(column(4,div(img(src="wsdot.png", width = "50%", height = "50%", style = "padding-top: 5px"))),
                              column(4,div(img(src="sdot.jpg", width = "50%", height = "50%", style = "padding-top: 5px")))
                     ),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="bike-walk-bus.jpeg", width = "75%", height = "75%", style = "padding-top: 25px"))),
                              column(4,div(img(src="biking.jpg", width = "75%", height = "75%", style = "padding-top: 25px"))),
                              column(4,div(img(src="tokul_trestle.jpg", width = "75%", height = "75%", style = "padding-top: 25px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_nonmotor"))),
                     fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/act/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/act/default.htm")))
                   ) # End of Main Panel of Non-Motorized Volumes
                 ) # End of Sidebar of Non-Motorized Volumes
        )# End of Tab Panel of Non-Motorized Volumes
                
  ) # End of NavBar Page
) # End of Shiny App
