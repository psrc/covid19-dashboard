shinyUI(
navbarPage(title=div(img(src="psrc-logo.png", width = 260, height = 92, style = ("padding-bottom: 30px"))),
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
                        fluidRow(column(6,div(img(src="southlakeunionseattle_0.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                                  column(6,div(img(src="bellevuelakewashington_3.jpg", width = "100%", height = "100%", style = "padding-top: 25px")))
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
                        div(img(src="esd.jpg", width = "50%", height = "50%", style = "padding-top: 5px")),
                        hr(),
                        strong("Weekly Initial Jobless Claims:"),
                        strong("(2019 / 2020 / Ratio)"),
                        tags$div(class="sidebar_data",textOutput("unemployment_March")),
                        tags$div(class="sidebar_data",textOutput("unemployment_April")),
                        tags$div(class="sidebar_data",textOutput("unemployment_May")),
                        tags$div(class="sidebar_data",textOutput("unemployment_Latest")),
                        hr(),
                        strong("Notes on Initial Jobless Claims:"),
                        br(),
                        tags$div(class="sidebar_notes",textOutput("esdBackground")),
                        br(),
                        width=3),
                      mainPanel(
                        fluidRow(column(4,div(img(src="unemployment_1.jpg", width = "75%", height = "75%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="unemployment_2.jpg", width = "85%", height = "85%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="unemployment_3.jpg", width = "100%", height = "100%", style = "padding-top: 25px")))
                        ),
                        br(),
                        fluidRow(column(12,plotlyOutput("chart_unemployment"))),
                        fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://esd.wa.gov/newsroom/unemployment-statistics", "Source: https://esd.wa.gov/newsroom/unemployment-statistics")))
                      ) # End of Main Panel of Unemployment
                    ) # End of Sidebar of Unemployment
           ),# End of Tab Panel of Unemployment

           tabPanel(icon("walking"),
                    sidebarLayout(
                      sidebarPanel(
                        div(img(src="wsdot.png", width = "30%", height = "30%", style = "padding-top: 5px")),
                        hr(),
                        selectInput("NonMotorLocations","Select the Non-Motorized count location:",place_choices),
                        hr(),
                        strong("Percentage of 2019 Non-Motorized Counts"),
                        tags$div(class="sidebar_data",textOutput("nonmotor_March")),
                        tags$div(class="sidebar_data",textOutput("nonmotor_April")),
                        tags$div(class="sidebar_data",textOutput("nonmotor_May")),
                        tags$div(class="sidebar_data",textOutput("nonmotor_June")),
                        tags$div(class="sidebar_data",textOutput("nonmotor_Latest")),
                        hr(),
                        strong("Notes on Non-Motorized Counts:"),
                        br(),
                        tags$div(class="sidebar_notes", textOutput("NonMotorBackground")),
                        br(),
                        width=3),
                      mainPanel(
                        fluidRow(column(4,div(img(src="bike-walk-bus.jpeg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="biking.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="tokul_trestle.jpg", width = "100%", height = "100%", style = "padding-top: 25px")))
                        ),
                        br(),
                        fluidRow(column(12,plotlyOutput("chart_nonmotor"))),
                        fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/act/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/act/default.htm")))
                      ) # End of Main Panel of Non-Motorized Volumes
                    ) # End of Sidebar of Non-Motorized Volumes
           ),# End of Tab Panel of Non-Motorized Volumes
           
            tabPanel(icon("plane-departure"),
            sidebarLayout(
                sidebarPanel(
                    div(img(src="tsa_logo.svg", width = 190, height = 67, style = "padding-top: 5px")),
                    hr(),
                    strong("Daily Passenger Screenings:"),
                    strong("(2019 / 2020 / Ratio)"),
                    tags$div(class="sidebar_data",textOutput("TSA_March")),
                    tags$div(class="sidebar_data",textOutput("TSA_April")),
                    tags$div(class="sidebar_data",textOutput("TSA_May")),
                    tags$div(class="sidebar_data",textOutput("TSA_Latest")),
                    hr(),
                    strong("Notes on Screening Data:"),
                    br(),
                    tags$div(class="sidebar_notes",textOutput("TSABackground")),
                    br(),
                    tags$div(class="sidebar_notes",textOutput("TSABackground2")),
                    br(),
                    width=3),
                mainPanel(
                  fluidRow(column(4,div(img(src="seatacterminal.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                           column(4,div(img(src="alaska-paine-field_0.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                           column(4,div(img(src="painefieldterminal_int_big.jpg", width = "100%", height = "100%", style = "padding-top: 25px")))
                  ),
                  br(),
                  textOutput("TSASummary"),
                  fluidRow(column(12,plotlyOutput("chart_tsa"))),
                  fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.tsa.gov/coronavirus/passenger-throughput", "Source: https://www.tsa.gov/coronavirus/passenger-throughput")))
           ) # End of Main Panel of Passenger Screening
          ) # End of Sidebar of Passenger Screening
        ),# End of Tab Panel of Passenger Screening
        
        tabPanel(icon("bus"),
                 sidebarLayout(
                    sidebarPanel(
                      fluidRow(column(6,div(img(src="community-transit.png", width = "75%", height = "75%", style = "padding-top: 5px"))),
                               column(6,div(img(src="everett-transit.png", width = "50%", height = "50%", style = "padding-top: 5px")))
                      ),
                      fluidRow(column(6,div(img(src="pierce-transit.jpg", width = "50%", height = "50%", style = "padding-top: 25px"))),
                               column(6,div(img(src="kitsap-transit.jpg", width = "50%", height = "50%", style = "padding-top: 25px")))
                      ),
                      fluidRow(column(6,div(img(src="kc-metro.png", width = "50%", height = "50%", style = "padding-top: 25px"))),
                               column(6,div(img(src="sound-transit.png", width = "50%", height = "50%", style = "padding-top: 25px")))
                      ),
                      hr(),
                      strong("Percentage of 2019 Daily Boardings by Operator"),
                      tags$div(class="sidebar_data",textOutput("ct_Latest")),
                      tags$div(class="sidebar_data",textOutput("et_Latest")),
                      tags$div(class="sidebar_data",textOutput("kcm_Latest")),
                      tags$div(class="sidebar_data",textOutput("kt_Latest")),
                      tags$div(class="sidebar_data",textOutput("ktf_Latest")),
                      tags$div(class="sidebar_data",textOutput("pt_Latest")),
                      tags$div(class="sidebar_data",textOutput("st_Latest")),
                      hr(),
                      strong("Notes on Tranist Data:"),
                      tags$div(class="sidebar_notes",textOutput("TransitBackground")),
                      br(),
                      width=3),
                    mainPanel(
                      fluidRow(column(4,div(img(src="bellevuetransitcenter.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                               column(4,div(img(src="ct.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                               column(4,div(img(src="link.jpg", width = "100%", height = "100%", style = "padding-top: 5px")))
                      ),
                      br(),
                      textOutput("TransitSummary"),
                      fluidRow(column(12,plotlyOutput("chart_transit"))),
                      fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/transit/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/transit/default.htm")))
                    ) #End of Main Panel
                 ) # End of Sidebar Layout for Transit
        ), # End of Transit Tab Panel

        tabPanel(icon("ship"),
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(column(6,div(img(src="wsf.png", width = "75%", height = "75%", style = "padding-top: 5px"))),
                              column(6,div(img(src="kitsap-transit.jpg", width = "50%", height = "50%", style = "padding-top: 5px")))
                              ),
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
                     br(),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="rp1.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="ferry_terminal.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="fast_ferry_terminal.jpg", width = "100%", height = "100%", style = "padding-top: 5px")))
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
                     fluidRow(column(12,div(img(src="AmtrakCascadesLogo.png", width = "75%", height = "75%", style = "padding-top: 5px")))
            
                     ),
                     hr(),
                     strong("Daily Passenger Volumes:"),
                     strong("(2019 / 2020 / Ratio)"),
                     tags$div(class="sidebar_data",textOutput("rail_Latest")),
                     hr(),
                     strong("Notes on Rail Data:"),
                     tags$div(class="sidebar_notes",textOutput("RailBackground")),
                     br(),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="seattleamtrak.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="amtrak_2.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="amtrak_3.jpg", width = "75%", height = "75%", style = "padding-top: 5px")))
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
                     div(img(src="wsdot.png", width = "30%", height = "30%", style = "padding-top: 5px")),
                     hr(),
                     selectInput("CountLocations","Select the count location:",count_locations),
                     hr(),
                     strong("Daily Traffic Volumes:"),
                     strong("(2019 / 2020 / Ratio)"),
                     tags$div(class="sidebar_data",textOutput("volumes_March")),
                     tags$div(class="sidebar_data",textOutput("volumes_April")),
                     tags$div(class="sidebar_data",textOutput("volumes_May")),
                     tags$div(class="sidebar_data",textOutput("volumes_Latest")),
                     hr(),
                     strong("Notes on Daily Traffic Volumes:"),
                     br(),
                     tags$div(class="sidebar_notes",textOutput("VolumeBackground")),
                     br(),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="i-90mercerisland_1.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                              column(4,div(img(src="access-ramp_0.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                              column(4,div(img(src="bremertonmanettebridge.jpg", width = "100%", height = "100%", style = "padding-top: 25px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_volumes"))),
                     fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/highway/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/highway/default.htm")))
                   ) # End of Main Panel of Traffic Volumes
                 ) # End of Sidebar of Traffic Volumes
        ),# End of Tab Panel of Traffic Volumes

        tabPanel(icon("truck"),
                 sidebarLayout(
                   sidebarPanel(
                     div(img(src="wsdot.png", width = "30%", height = "30%", style = "padding-top: 5px")),
                     hr(),
                     selectInput("TruckLocations","Select the Truck count location:",truck_count_locations),
                     hr(),
                     strong("Percentage of 2019 Daily Truck Volumes"),
                     tags$div(class="sidebar_data",textOutput("trucks_March")),
                     tags$div(class="sidebar_data",textOutput("trucks_April")),
                     tags$div(class="sidebar_data",textOutput("trucks_May")),
                     tags$div(class="sidebar_data",textOutput("trucks_June")),
                     tags$div(class="sidebar_data",textOutput("trucks_Latest")),
                     hr(),
                     strong("Notes on Daily Truck Volumes:"),
                     br(),
                     tags$div(class="sidebar_notes", textOutput("TruckBackground")),
                     br(),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="autonomous_truck.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                              column(8,div(img(src="trucks.jpg", width = "75%", height = "75%", style = "padding-top: 25px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_trucks"))),
                     fluidRow(br(),column(width = 12, tags$a(class = "source_url", href="https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/freight/default.htm", "Source: https://www.wsdot.wa.gov/about/covid-19-transportation-report/dashboard/freight/default.htm")))
                   ) # End of Main Panel of Truck Volumes
                 ) # End of Sidebar of Truck Volumes
        ),# End of Tab Panel of Truck Volumes
        
        
        tabPanel(icon("info-circle"),
                 h1("Data Sources"),
                 "The data in this portal comes from a few key sources:",
                 hr(),
                 h2("Initial Unemployment Claims "),
                 "from ESD",
                 br(),
                 h2("Airport Passenger Screenings:"),
                 "from TSA",
                 br()
        )# End of Tab Panel of Information        
                
  ) # End of NavBar Page
) # End of Shiny App
