# Below is the code for Grid2050, an R Shiny app created to investigate preferences around electricity decarbonizaiton in the US
# Grid2050 was created by Dr. Sarah Troise
# The app is released under the MIT License
#


#The below packages are needed to run the app
library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)

# Data is imported from an external excel spreadsheet
data <- read_excel("ToolData.xlsx", sheet = "toolData")

#Creates a dataframe format to be used in the tool
data <- as.data.frame(data)

#Sets row names at tech names
row.names(data) <- data$Technology

########CHANGE IF NEW TECHNOLOGIES ADDED#########
numTechnology <- 8

# These dataframes will be used later in the tool to create time dynamics graphs
timeData_1 <- data.frame(
  Year = c(rep(2025,numTechnology),rep(2030,numTechnology),rep(2035,numTechnology),rep(2040,numTechnology),rep(2045,numTechnology), rep(2050,numTechnology)),
  Technology = rep(data$Technology,6),
  Generation = rep(as.numeric(data$Generation_2020),6),
  LCOE = rep(as.numeric(data$LCOE_2020),6)
)

#Creates reactive versions of tool data needed for R Shiny app
toolData <- reactiveVal(data)
timeData <- reactiveVal(timeData_1)

#Sets up User Interface
ui <- fluidPage(

  dashboardPage(
  
    skin = "green",
    
    dashboardHeader(title="Grid2050"),
    
    dashboardSidebar(
      sidebarMenu(
########MENU ITEMS, CAN ADD MORE#############
        menuItem("Start", tabName = "start"),
        menuItem("2050 Generation Portfolio", tabName = "tool_gen"),
        menuItem("Uncertainity", tabName = "tool_uncert"),
        menuItem("Time Dynamics", tabName = "tool_time"),
        menuItem("Download", tabName = "download"),
        menuItem("Assumptions", tabName = "assume"),
        menuItem("Contact Us", tabName = "contact")
      )
    ), #end dashboardSidebar
    dashboardBody(
      ###HOME PAGE
      tabItems(
      ###STARTING ASSUMPTIONS
      tabItem(tabName = "start",
              titlePanel("Grid2050: Electricity Decarbonization"),
              sidebarLayout(
                sidebarPanel( fluidRow(
                  # Copy the chunk below to make a group of checkboxes
                  checkboxGroupInput("checkGroup", label = "Select Technologies to Include", 
                                     choices = data$Technology,
                                     selected = data$Technology),
                  sliderInput("demand", label = "Annual Growth Electricity in Demand (%)", min = 0, max = 10, step = .25, value = 0),
                  sliderInput("goal", label = "Goal % of Generation to to be Decarbonized", min = 0, max = 100, step = 5, value = 80)
                )),
                mainPanel(
                  includeHTML("start.html")
                )
              )
              
             ),
      ### GENERATION PORTFOLIO
        tabItem(tabName = "tool_gen",
                fluidRow(
                  column(3,wellPanel(
                    uiOutput("tech_control"),
                    sliderInput("maxGW", "2050 Generation (TWhr)", min = 0, max = 4000, 
                                value = 0,
                                step = 10),
                    sliderInput("newCost", "Levelized Cost of Electricity ($/MWhr)", min = 0, max = 200, 
                                value = 0,
                                step = 5),
                    
                    actionButton("submit","Change Value",
                                 style="color: #fff; background-color: #298b3e; border-color: #298b3")
                  )),
                  column(9,
                         includeHTML("step1.html"),
                         br(),
                         infoBoxOutput("demand"),
                         infoBoxOutput("remainGoal"),
                         plotOutput("generation_portfolio", width = 800, height = 600)
                  )
                ),
                ),
      ### TOOL UNCERTAINTY
      tabItem(tabName = "tool_uncert",
              fluidRow(
                column(3, wellPanel(
                  selectizeInput(
                    inputId = 'tech_err', 'Chosen Technology', choices = data$Technology,
                    options = list(
                      placeholder = 'Please select Technology'
                    )),
                  sliderInput("rangeGW", "Range of Generation (TWhr):",
                              min = 0, max = 4000,
                              value = c(145,145),
                              step = 10),
                  sliderInput("rangeCost", "Range of Cost ($/MWhr):",
                              min = 0, max = 200,
                              value = c(30,49),
                              step = 5),
                  actionButton("submitCostUncert","Change Value",
                               style="color: #fff; background-color: #298b3e; border-color: #298b3"),
                  br(),
                  br(),
                  
                ##four extreme scenario buttons
                  actionButton("maxPminC", "Max Generation/Min Cost"),
                  actionButton("minPmaxC", "Min Generation/Max Cost"),
                  actionButton("maxPmaxC", "Max Generation/Max Cost"),
                  actionButton("minPminC", "Min Generation/Min Cost"),
                  br(),
                  br(),
                  actionButton("reset_tech", "Reset", style="color: #fff; background-color: red; border-color: #298b3"),
                  
                )),
                column(6,
                       includeHTML("uncertain.html"),
                       plotOutput("errorPlot", width = 800, height = 600)
                       
                ),
              )),
      ### TOOL TIME DEPENDENCY
      tabItem(tabName = "tool_time",
              fluidRow(
                column(3,wellPanel(
                  selectizeInput(
                    inputId = 'tech_time', 'Chosen Technology', choices = data$Technology,
                    options = list(
                      placeholder = 'Please select Technology'
                    )),
                  sliderInput("adopt", "Adoption Rate (%)", min = 0, max = 200, value = 0,
                              step = 1),
                  sliderInput("retire", "Retirement Rate pre 2040 (%)", min = 0, max = 100, 
                              value = 0,
                              step = 1),
                  sliderInput("retire2", "Retirement Rate post 2040 (%)", min = 0, max = 100, 
                              value = 0,
                              step = 1),
                  sliderInput("learn", "Learning Rate (%)", min = 0, max = 100, 
                              value = 0,
                              step = 1),
                  
                  actionButton("submit_time","Change Value",
                               style="color: #fff; background-color: #298b3e; border-color: #298b3")
                )),
                column(9,
                       includeHTML("learn.html"),
                       br(),
                       plotOutput("timePlot", width = 800, height = 600),
                       br(),
                       plotOutput("learningPlot", width = 800, height = 600)
                )
              ),
      ),
        tabItem(tabName = "assume",
      tabsetPanel(
        tabPanel("Assumptions",
                 column(width = 12, style='padding:20px',
                 br(),
                 includeHTML("assumption.html"))),
        tabPanel("CCS",
                 column(width = 12, style='padding:20px',
                 br(),
                 includeHTML("CCS.html"))),
        tabPanel("Hydro",
                 column(width = 12, style='padding:20px',
                 br(),
                 includeHTML("Hydro.html"))),
        tabPanel("Nuclear",
                 column(width = 12, style='padding:20px',
                 br(),
                 includeHTML("Nuclear.html"))),
        tabPanel("Offshore",
                 column(width = 12, style='padding:20px',
                 br(),
                 includeHTML("Offshore.html"))),
        tabPanel("Onshore",
                 column(width = 12, style='padding:20px',
                 br(),
                 includeHTML("Onshore.html"))),
        tabPanel("Solar",
                 column(width = 12, style='padding:20px',
                 br(),
                 includeHTML("Solar.html")))
        
      )),
      tabItem(tabName = "download",
              p(""),
              textInput("filename", "Enter File Name"),
              downloadButton("downloadData", "Download")
              ),
        tabItem(tabName = "contact",
                includeHTML("contact.html"))
      
      )#end tabItems
    ) #end dashboard Body 
     
  ) #end dashboardPage
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
#Create drop down menu with only selected technologies
 output$tech_control <- renderUI({         
    tech <- input$checkGroup
    selectizeInput(
      inputId = 'tech', 'Chosen Technology', choices = tech)
  })
 
 
  ############## CHANGE SLIDER VALUES ##########################
  #Each of these update given sliders to reflect the current value of generation or cost for each technology
  observe({
    currentTech <- input$tech
    val <- as.numeric(toolData()[currentTech, "LCOE_2050_Mean_User"])
    updateSliderInput(session, "newCost", value = val)
  })
  
  observe({
    currentTech <- input$tech
    val <- as.numeric(toolData()[currentTech, "Generation_2050_Mean_User"])
    updateSliderInput(session, "maxGW", value = val)
    
  })
  
  observe({
    currentTech <- input$tech_err
    minFrac <- as.numeric(toolData()[currentTech, "Generation_2050_Min_User"])
    maxFrac <- as.numeric(toolData()[currentTech, "Generation_2050_Max_User"])
    updateSliderInput(session, "rangeGW", value = c(minFrac,maxFrac))
  })
  
  observe({
    currentTech <- input$tech_err
    minFrac <- as.numeric(toolData()[currentTech, "LCOE_2050_Min_User"])
    maxFrac <- as.numeric(toolData()[currentTech, "LCOE_2050_Max_User"])
    updateSliderInput(session, "rangeCost", value = c(minFrac,maxFrac))
  })
  
  observe({
    currentTech <- input$tech_time
    rateDataPlot <- toolData()
    adopt <- rateDataPlot[rateDataPlot$Technology == input$tech_time, "adoption"]
    updateSliderInput(session, "adopt", value = adopt)
  })
  
  observe({
    currentTech <- input$tech_time
    rateDataPlot <- toolData()
    retire <- rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire"]
    updateSliderInput(session, "retire", value = retire)
  })
  
  observe({
    currentTech <- input$tech_time
    rateDataPlot <- toolData()
    retire <- rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire2"]
    updateSliderInput(session, "retire2", value = retire)
  })
  
  observe({
    currentTech <- input$tech_time
    rateDataPlot <- toolData()
    learn <- rateDataPlot[rateDataPlot$Technology == input$tech_time, "learn"]
    updateSliderInput(session, "learn", value = learn)
  })
  
  
  ################## PLOT FOR GENERATION PORTFOLIO #####################
  #This creates the portfolio plot for Step 1, it is initalized with the current low carbon generation, and then users use sliders to add
  
  output$generation_portfolio <- renderPlot({
    #Initalize reactive dataset
    cleanData <- toolData()
    cleanData <- as.data.frame(cleanData)
    
    #Demand goal as set by user using growth rate
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    
    demandData <- data.frame(demand = demand,
                             Goal = "2050 Goal Demand")
    cleanData <- subset(cleanData, Technology %in% input$checkGroup)


      row.names(cleanData) <- cleanData$Technology
      #Set generation and cost values for select technology from slider input
      cleanData[input$tech,"Generation_2050_Mean_User"] <- as.numeric(input$maxGW)
      cleanData[input$tech, "LCOE_2050_Mean_User"] <- as.numeric(input$newCost)
      
      #values needed to plot each technology as a bar
      cleanData <- cleanData[order(cleanData$LCOE_2050_Mean_User),]
      cleanData$w <- cumsum(cleanData$Generation_2050_Mean_User)
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Mean_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
 
      ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                        ymax = LCOE_2050_Mean_User, fill = Technology)) + 
                                        geom_vline(data = demandData, aes(xintercept = demand, color = Goal), linetype="dashed", size=1.5) +
                          
                                        labs(x = "TWhr of carbon free electricity",y = "Levelize Cost of Energy ($/MWhr)")  + 
                                        theme_bw() + theme(text = element_text(size=20)) 
      
      
    

    })
  
  #############SUBMIT BUTTONS##################
  ##Update the underlying data with the slider values in step 2
  observeEvent(input$submit, {
    outData <- toolData()
    outData[input$tech,"Generation_2050_Mean_User"] <- as.numeric(input$maxGW)
    outData[input$tech, "LCOE_2050_Mean_User"] <- as.numeric(input$newCost)
    inital <- as.numeric(outData[input$tech, "Generation_2020"])
    final <- as.numeric(input$maxGW)
    outData[input$tech, "adoption"] <-  (((final/inital)^(1/25))-1)*100
    toolData(outData)
    
  })
  
  #################### UNCERTAINTY GRAPH ####################
  #The following plots are for the uncertainty plot and scenario plots in step 3
  
  output$errorPlot <- renderPlot({
    #Initalize reactive dataset
      cleanData <- toolData()
    #Demand set by user goals 
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      
      #Set generation and cost ranges for the select technology using the sliders
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
      
      #Values needed to plot each technology as a bar 
      cleanData <- cleanData[order(cleanData$LCOE_2050_Mean_User),]
      cleanData$w <- cumsum(cleanData$Generation_2050_Mean_User)
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Mean_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
      ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                                       max = LCOE_2050_Mean_User, fill = Technology)
      ) + geom_errorbar(data=cleanData[input$tech_err,],aes(x=mid, ymin = cleanData[input$tech_err, "LCOE_2050_Min_User"], ymax= cleanData[input$tech_err, "LCOE_2050_Max_User"]), size =2, width = 10
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[2]) , xmax=mid + (.5 * input$rangeGW[2])), color="red", size=2
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[1]) , xmax=mid + (.5 * input$rangeGW[1])), color="blue", size =2
      ) + labs(x = "TWhr of carbon free electricity", y = "Levelized Cost of Energy ($/MWhr)"
      ) + theme_bw() + theme(
          text = element_text(size=20))  + 
        geom_vline(data = demandData, aes(xintercept = demand, color = Goal), linetype="dashed", size=1.5) 
      
      
     })
  
  ###################UNCERTAINTY SCENARIO PLOT##################
  #This plot is for the Max Generation, Min Cost scenario. This scenario plots what the portfolio would look like at the high end of generation uncertainty and
  #low end of cost. This is the best case scenario. 
  observeEvent(input$maxPminC,{
    output$errorPlot <- renderPlot({
    #Initalize reactive data
    cleanData <- toolData()
    #Demand goal set by user
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    demandData <- data.frame(demand = demand,
                             Goal = "2050 Goal Demand")
    cleanData <- as.data.frame(cleanData)
    row.names(cleanData) <- cleanData$Technology
    cleanData <- subset(cleanData, Technology %in% input$checkGroup)
    #Set generation and cost ranges for the select technology using the sliders
    cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
    cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
    cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
    cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
    #Values needed to plot each technology as a bar
    cleanData <- cleanData[order(cleanData$LCOE_2050_Min_User),]
    cleanData$w <- cumsum(cleanData$Generation_2050_Max_User)
    #Set values to algin with scenario wm=Generation Max
    cleanData$wm <- cleanData$w - cleanData$Generation_2050_Max_User
    cleanData$wt <- with(cleanData, wm + (w- wm)/2)
    cleanData$mid <- (cleanData$w + cleanData$wm)/2
    
    #Set values to align with scenario
    # max = LCOE Min 
    ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                                     max = LCOE_2050_Min_User, fill = Technology)
    ) + geom_errorbar(data=cleanData[input$tech_err,],aes(x=mid, ymin = cleanData[input$tech_err, "LCOE_2050_Min_User"], ymax= cleanData[input$tech_err, "LCOE_2050_Max_User"]), size =2, width = 10
    ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[2]) , xmax=mid + (.5 * input$rangeGW[2])), color="red", size=2
    ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[1]) , xmax=mid + (.5 * input$rangeGW[1])), color="blue", size =2
    ) + labs(x = "TWhr of carbon free electricity", y = "Levelized Cost of Energy ($/MWhr)"
    ) + theme_bw() + theme(
      text = element_text(size=20))  + 
      geom_vline(data = demandData, aes(xintercept = demand, color = Goal), linetype="dashed", size=1.5) 
  })
  })
  
  ###################UNCERTAINTY SCENARIO PLOT##################
  #This plot is for the Max Generation, Max Cost scenario. This scenario plots what the portfolio would look like at the high end of generation uncertainty and
  #high end of cost.  
  observeEvent(input$maxPmaxC,{
    output$errorPlot <- renderPlot({
      #Initialize reactive data
      cleanData <- toolData()
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      #Set generation and cost ranges for the select technology using the sliders
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
      cleanData <- cleanData[order(cleanData$LCOE_2050_Max_User),]
      #Values needed to plot each technology as a bar
      cleanData$w <- cumsum(cleanData$Generation_2050_Max_User)
      #Set values to algin with scenario w=Generation Max
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Max_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
      #Set values to align with scenario
      # max = LCOE Max 
      ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                                       max = LCOE_2050_Max_User, fill = Technology)
      ) + geom_errorbar(data=cleanData[input$tech_err,],aes(x=mid, ymin = cleanData[input$tech_err, "LCOE_2050_Min_User"], ymax= cleanData[input$tech_err, "LCOE_2050_Max_User"]), size =2, width = 10
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[2]) , xmax=mid + (.5 * input$rangeGW[2])), color="red", size=2
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[1]) , xmax=mid + (.5 * input$rangeGW[1])), color="blue", size =2
      ) + labs(x = "TWhr of carbon free electricity", y = "Levelized Cost of Energy ($/MWhr)"
      ) + theme_bw() + theme(
        text = element_text(size=20))  + 
        geom_vline(data = demandData, aes(xintercept = demand, color = Goal), linetype="dashed", size=1.5) 
    })
  })
  
  ###################UNCERTAINTY SCENARIO PLOT##################
  #This plot is for the Min Generation, Max Cost scenario. This scenario plots what the portfolio would look like at the low end of generation uncertainty and
  #high end of cost. This is the worst case scenario
  
  observeEvent(input$minPmaxC,{
    output$errorPlot <- renderPlot({
      #initialize reactive data
      cleanData <- toolData()
      #Demand goal set by user
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      #Set generation and cost ranges for the select technology using the sliders
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
      #Values needed to plot each technology as a bar
      cleanData <- cleanData[order(cleanData$LCOE_2050_Min_User),]
      #Set values to algin with scenario w=Generation Min
      cleanData$w <- cumsum(cleanData$Generation_2050_Min_User)
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Min_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
      #Set values to align with scenario
      # max = LCOE Max 
      ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                                       max = LCOE_2050_Max_User, fill = Technology)
      ) + geom_errorbar(data=cleanData[input$tech_err,],aes(x=mid, ymin = cleanData[input$tech_err, "LCOE_2050_Min_User"], ymax= cleanData[input$tech_err, "LCOE_2050_Max_User"]), size =2, width = 10
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[2]) , xmax=mid + (.5 * input$rangeGW[2])), color="red", size=2
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[1]) , xmax=mid + (.5 * input$rangeGW[1])), color="blue", size =2
      ) + labs(x = "TWhr of carbon free electricity", y = "Levelized Cost of Energy ($/MWhr)"
      ) + theme_bw() + theme(
        text = element_text(size=20))  + 
        geom_vline(data = demandData, aes(xintercept = demand, color = Goal), linetype="dashed", size=1.5) 
    })
  })
  
  ###################UNCERTAINTY SCENARIO PLOT##################
  #This plot is for the Min Generation, Min Cost scenario. This scenario plots what the portfolio would look like at the low end of generation uncertainty and
  #low end of cost. 
  observeEvent(input$minPminC,{
    output$errorPlot <- renderPlot({
      #Initalize reactive data
      cleanData <- toolData()
      #Demand goal set by user
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      #Set generation and cost ranges for the select technology using the sliders
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
      #Values needed to plot each technology as a bar
      cleanData <- cleanData[order(cleanData$LCOE_2050_Min_User),]
      #Set values to algin with scenario w=Generation Min
      cleanData$w <- cumsum(cleanData$Generation_2050_Min_User)
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Min_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
      #Set values to align with scenario
      # max = LCOE Min
      ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                                       max = LCOE_2050_Min_User, fill = Technology)
      ) + geom_errorbar(data=cleanData[input$tech_err,],aes(x=mid, ymin = cleanData[input$tech_err, "LCOE_2050_Min_User"], ymax= cleanData[input$tech_err, "LCOE_2050_Max_User"]), size =2, width = 10
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[2]) , xmax=mid + (.5 * input$rangeGW[2])), color="red", size=2
      ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[1]) , xmax=mid + (.5 * input$rangeGW[1])), color="blue", size =2
      ) + labs(x = "TWhr of carbon free electricity", y = "Levelized Cost of Energy ($/MWhr)"
      ) + theme_bw() + theme(
        text = element_text(size=20))  + 
        geom_vline(data = demandData, aes(xintercept = demand, color = Goal), linetype="dashed", size=1.5) 
    })
  })
  
  ########SUBMIT BUTTON UNCERTAINTY#########
  #submit button and update values for step 3
  observeEvent(input$submitCostUncert, {
    outData <- toolData()
    outData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
    outData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
    outData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
    outData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
    
    toolData(outData)
  })
  
  
  
#############RESET BUTTON################
  #This resets the uncertainty plot in Step 3 to the user values after exploring the scenario buttons
  observeEvent(input$reset_tech, {
  output$errorPlot <- renderPlot({
    #Initalize reactive data
    cleanData <- toolData()
    #User demand goal
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    demandData <- data.frame(demand = demand,
                             Goal = "2050 Goal Demand")
    cleanData <- as.data.frame(cleanData)
    row.names(cleanData) <- cleanData$Technology
    cleanData <- subset(cleanData, Technology %in% input$checkGroup)
    #Set generation and cost ranges for the select technology using the sliders
    cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
    cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
    cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
    cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
    cleanData <- cleanData[order(cleanData$LCOE_2050_Mean_User),]
    #Values needed to plot each technology as a bar
    cleanData$w <- cumsum(cleanData$Generation_2050_Mean_User)
    cleanData$wm <- cleanData$w - cleanData$Generation_2050_Mean_User
    cleanData$wt <- with(cleanData, wm + (w- wm)/2)
    cleanData$mid <- (cleanData$w + cleanData$wm)/2
    
    ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                                     max = LCOE_2050_Mean_User, fill = Technology)
    ) + geom_errorbar(data=cleanData[input$tech_err,],aes(x=mid, ymin = cleanData[input$tech_err, "LCOE_2050_Min_User"], ymax= cleanData[input$tech_err, "LCOE_2050_Max_User"]), size =2, width = 10
    ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[2]) , xmax=mid + (.5 * input$rangeGW[2])), color="red", size=2
    ) + geom_errorbarh(data=cleanData[input$tech_err,],aes(y=LCOE_2050_Mean_User, xmin = mid - (.5 * input$rangeGW[1]) , xmax=mid + (.5 * input$rangeGW[1])), color="blue", size =2
    ) + labs(x = "TWhr of carbon free electricity", y = "Levelized Cost of Energy ($/MWhr)"
    ) + theme_bw() + theme(
      text = element_text(size=20))  + 
      geom_vline(data = demandData, aes(xintercept = demand, color = Goal), linetype="dashed", size=1.5) 
    
    
  })
})
  
  ##################TIME PLOT##################
  ##This plot creates the annual growth required to meet the portfolio goals set in step 2
  output$timePlot <- renderPlot({
    #User demand goal
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    demandData <- data.frame(demand = demand,
                           Goal = "2050 Goal Demand")
    #Initalize reactive data
    timeDataPlot <- timeData()
    rateDataPlot <- toolData()
    
    timeDataPlot <- as.data.frame(timeDataPlot)
    timeDataPlot <- subset(timeDataPlot, Technology %in% input$checkGroup)
    
    rateDataPlot <- as.data.frame(rateDataPlot)
    rateDataPlot <- subset(rateDataPlot, Technology %in% input$checkGroup)
    
    #Capture user inputs for adoption, retirement pre and post
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "adoption"] <- input$adopt
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire"] <- input$retire
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire2"] <- input$retire2

    #Calculate the annual growth pre 2040
    for (row in 1:(numTechnology * 4)) {
      #print(row)
      years <- (timeDataPlot[row,"Year"] - 2020)/5
      adopt <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "adoption"] / 100
      retire <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "retire"] / 100
      inital <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "Generation_2020"]
 
      growth <- (adopt + 1) - retire
      years <- (years-1) * 5
      timeDataPlot[row,"Generation"] <- inital * (growth^(years))
    }
    
    #Calculate the annual growth post 2040
    for (row in (numTechnology * 4 + 1):(numTechnology * 6)) {
      #print(row)
      years <- (timeDataPlot[row,"Year"] - 2020)/5
      adopt <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "adoption"] / 100
      retire <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "retire"] / 100
      retire2 <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "retire2"] / 100
      growth <- (adopt + 1) - retire
      inital <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "Generation_2020"] * (growth^(15))
      
      growth2 <- (adopt + 1) - retire2
      years <- (years-1) * 5
      years <- years - 15
      timeDataPlot[row,"Generation"] <- inital * (growth2^(years))
      print(years)
      print(timeDataPlot)
    }

    # stacked area chart
    ggplot(timeDataPlot, aes(x=Year, y=Generation, fill=Technology)) + 
      geom_area() + geom_hline(data = demandData, aes(yintercept = demand, color = Goal), linetype="dashed", size=1.5) + labs(x = "Year", y = "TWhr of carbon free electricity") + theme_bw() + theme(text = element_text(size=20))
  })

########LEARNING RATE PLOT###########
#This plots the learning rate change in cost based on the adoption of new technology
  output$learningPlot <- renderPlot({
    timeDataPlot <- timeData()
    rateDataPlot <- toolData()
    
    timeDataPlot <- as.data.frame(timeDataPlot)
    timeDataPlot <- subset(timeDataPlot, Technology %in% input$checkGroup)
    
    rateDataPlot <- as.data.frame(rateDataPlot)
    rateDataPlot <- subset(rateDataPlot, Technology %in% input$checkGroup)
    
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "adoption"] <- input$adopt
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire"] <- input$retire
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "learn"] <- input$learn
    
    for (row in 1:nrow(timeDataPlot)) {
      years <- (timeDataPlot[row,"Year"] - 2020)/5
      adopt <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "adoption"] / 100
      inital <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "Generation_2020"]
      inital_cost <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "LCOE_2020"]
      LR <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "learn"] / 100
      growth <- (adopt + 1)
      
      if (years == 0){
 
        pre_gen <- inital * (growth^(years))
        final_gen <- inital * (growth^(years))
        doubling <- log(final_gen/pre_gen) / log(2)
        cost <- inital_cost * (1-LR)^doubling
        timeDataPlot[row,"LCOE"] <- cost
      }
      if (years != 0){

        pre_gen <- inital * (growth^(years - 1))
        final_gen <- inital * (growth^(years))
        doubling <- log(final_gen/pre_gen) / log(2)
        cost <- inital_cost * ((1-LR)^doubling)^years
        timeDataPlot[row,"LCOE"] <- cost
        
      }

    }

    # stacked area chart
    ggplot(timeDataPlot, aes(x=Year, y=LCOE, group=Technology)) + 
      geom_line(aes(color=Technology), size=2) + labs(x = "Year", y = "LCOE ($/MWhr)"
                               ) + theme_bw() + theme(
                                 text = element_text(size=20))
  })
  
 

  #########SUBMIT BUTTON TIME DYNAMICS#############
  #Submit button and update values for step 4
  
  observeEvent(input$submit_time, {
    rateDataPlot <- toolData()
    
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "adoption"] <- input$adopt
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire"] <- input$retire
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "learn"] <- input$learn
    toolData(rateDataPlot)

  })
  

  ###########INFO BOX###########
  output$demand <- renderInfoBox({
    outData <- toolData()
    inital <- as.numeric(outData[input$tech, "Generation_2020"])
    final <- as.numeric(input$maxGW)
    adopt <-  ((final/inital)^(1/25))-1
    adopt <- adopt * 100
    infoBox("Current Adoption Rate", value = paste0(round(adopt, 1), "%"), width = 8, icon = icon("fas fa-bolt"))
  })
  
  output$remainGoal <- renderInfoBox({
    generation <- toolData()
    total <- sum(generation$Generation_2050_Mean_User)
    rate <- generation[generation$Technology == input$tech, "maxGrowth"]
    infoBox("Max Historic Rate", value = rate, width = 4, icon = icon("fas fa-bolt"))
  })
  
  
  #############DOWNLOAD############
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(toolData(), file)
    },
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
