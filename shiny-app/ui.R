shinyUI(
navbarPage("COVID-19 Data Dashboard", theme = "styles.css",
           tabPanel(icon("briefcase"),
                    sidebarLayout(
                      sidebarPanel(
                        div(img(src="esd.jpg", width = 190, height = 67, style = "padding-top: 5px")),
                        hr(),
                        strong("Weekly Initial Jobless Claims:"),
                        strong("(2019 / 2020 / Ratio)"),
                        textOutput("unemployment_March"),
                        textOutput("unemployment_April"),
                        textOutput("unemployment_May"),
                        textOutput("unemployment_Latest"),
                        hr(),
                        strong("Notes on Initial Jobless Claims:"),
                        br(),
                        textOutput("esdBackground"),
                        br(),
                        width=3),
                      mainPanel(
                        fluidRow(column(4,div(img(src="unemployment_1.jpg", width = "75%", height = "75%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="unemployment_2.jpg", width = "85%", height = "85%", style = "padding-top: 25px"))),
                                 column(4,div(img(src="unemployment_3.jpg", width = "100%", height = "100%", style = "padding-top: 25px")))
                        ),
                        br(),
                        fluidRow(column(12,plotlyOutput("chart_unemployment"))),
                        fluidRow(br(),column(width = 12, h4(textOutput("esdLink"))))
                      ) # End of Main Panel of Passenger Screening
                    ) # End of Sidebar of Passenger Screening
           ),# End of Tab Panel of Passenger Screening
           
           
            tabPanel(icon("plane-departure"),
            sidebarLayout(
                sidebarPanel(
                    div(img(src="tsa_logo.svg", width = 190, height = 67, style = "padding-top: 5px")),
                    hr(),
                    strong("Daily Passenger Screenings:"),
                    strong("(2019 / 2020 / Ratio)"),
                    textOutput("TSA_March"),
                    textOutput("TSA_April"),
                    textOutput("TSA_May"),
                    textOutput("TSA_Latest"),
                    hr(),
                    strong("Notes on Screening Data:"),
                    br(),
                    textOutput("TSABackground"),
                    br(),
                    textOutput("TSABackground2"),
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
                  fluidRow(br(),column(width = 12, h4(textOutput("TSALink"))))
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
                      textOutput("ct_Latest"),
                      textOutput("et_Latest"),
                      textOutput("kcm_Latest"),
                      textOutput("kt_Latest"),
                      textOutput("pt_Latest"),
                      textOutput("st_Latest"),
                      hr(),
                      strong("Notes on Tranist Data:"),
                      textOutput("TransitBackground"),
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
                      fluidRow(br(),column(width = 12, h4(textOutput("TransitLink"))))
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
                     textOutput("bi_Latest"),
                     textOutput("brem_Latest"),
                     textOutput("faunt_Latest"),
                     textOutput("king_Latest"),
                     textOutput("muki_Latest"),
                     textOutput("ptdef_Latest"),
                     hr(),
                     strong("Notes on Ferry Data:"),
                     textOutput("FerryBackground"),
                     br(),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="rp1.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="ferry_terminal.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="fast_ferry_terminal.jpg", width = "100%", height = "100%", style = "padding-top: 5px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_ferry"))),
                     fluidRow(br(),column(width = 12, h4(textOutput("FerryLink"))))
                   ) #End of Main Panel
                 ) # End of Sidebar Layout for Ferry
        ), # End of Ferry Tab Panel

        tabPanel(icon("train"),
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(column(12,div(img(src="AmtrakCascadesLogo.png", width = "75%", height = "75%", style = "padding-top: 5px")))
            
                     ),
                     hr(),
                     strong("Percentage of 2019 Daily Boardings by Operator"),
                     textOutput("rail_Latest"),
                     hr(),
                     strong("Notes on Rail Data:"),
                     textOutput("RailBackground"),
                     br(),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="seattleamtrak.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="amtrak_2.jpg", width = "100%", height = "100%", style = "padding-top: 5px"))),
                              column(4,div(img(src="amtrak_3.jpg", width = "75%", height = "75%", style = "padding-top: 5px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_rail"))),
                     fluidRow(br(),column(width = 12, h4(textOutput("RailLink"))))
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
                     textOutput("volumes_March"),
                     textOutput("volumes_April"),
                     textOutput("volumes_May"),
                     textOutput("volumes_Latest"),
                     hr(),
                     strong("Notes on Daily Traffic Volumes:"),
                     br(),
                     textOutput("VolumeBackground"),
                     br(),
                     width=3),
                   mainPanel(
                     fluidRow(column(4,div(img(src="i-90mercerisland_1.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                              column(4,div(img(src="access-ramp_0.jpg", width = "100%", height = "100%", style = "padding-top: 25px"))),
                              column(4,div(img(src="bremertonmanettebridge.jpg", width = "100%", height = "100%", style = "padding-top: 25px")))
                     ),
                     br(),
                     fluidRow(column(12,plotlyOutput("chart_volumes"))),
                     fluidRow(br(),column(width = 12, h4(textOutput("VolumeLink"))))
                   ) # End of Main Panel of Traffic Volumes
                 ) # End of Sidebar of Traffic Volumes
        )# End of Tab Panel of Traffic Volumes
        
  ) # End of NavBar Page
) # End of Shiny App