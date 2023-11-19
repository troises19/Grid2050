#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
# Define UI for application that draws a histogram
data <- read_excel("ToolData.xlsx", sheet = "toolData")
data <- as.data.frame(data)
row.names(data) <- data$Technology
timeData_1 <- data.frame(
  Year = c(rep(2025,6),rep(2030,6),rep(2035,6),rep(2040,6),rep(2045,6), rep(2050,6)),
  Technology = rep(data$Technology,6),
  Generation = rep(as.numeric(data$Generation_2020),6),
  LCOE = rep(as.numeric(data$LCOE_2020),6)
)

rates <- data.frame(
  Technology = data$Technology,
  adoption = c(0), 
  retire = c(0),
  learn = c(0),
  inital = data$Generation_2020,
  inital_lcoe = data$LCOE_2020
)
storeData <- reactiveVal(data)
timeData <- reactiveVal(timeData_1)
rateData <- reactiveVal(rates)

ui <- fluidPage(

  dashboardPage(
  
    skin = "green",
    
    dashboardHeader(title="Grid2050"),
    
    dashboardSidebar(
      sidebarMenu(

        menuItem("Start", tabName = "start"),
        menuItem("Tool Generation", tabName = "tool_gen"),
        menuItem("Tool Uncertainity", tabName = "tool_uncert"),
        menuItem("Tool Time", tabName = "tool_time"),
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
      ###TOOL GENERATION
        tabItem(tabName = "tool_gen",
                fluidRow(
                  column(3,wellPanel(
                    uiOutput("tech_control"),
                    sliderInput("maxGW", "2050 Generation (TWhr)", min = 0, max = 1400, 
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
                         plotOutput("plot1", width = 800, height = 600)
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
                      #onInitialize = I('function() { this.setValue(""); }')
                    )),
                  sliderInput("rangeGW", "Range of Generation (TWhr):",
                              min = 0, max = 1000,
                              value = c(145,145),
                              step = 10),
                  #verbatimTextOutput("maxkWError"),
                  #verbatimTextOutput("minkWError"),
                  sliderInput("rangeCost", "Range of Cost ($/MWhr):",
                              min = 0, max = 200,
                              value = c(30,49),
                              step = 5),
                  #verbatimTextOutput("maxCostError"),
                  #verbatimTextOutput("minCostError"),
                  actionButton("submitCostUncert","Change Value",
                               style="color: #fff; background-color: #298b3e; border-color: #298b3"),
                  br(),
                  br(),
                  actionButton("maxPminC", "Max Potential/Min Cost"),
                  actionButton("minPmaxC", "Min Potential/Max Cost"),
                  actionButton("maxPmaxC", "Max Potential/Max Cost"),
                  actionButton("minPminC", "Min Potential/Min Cost"),
                  br(),
                  br(),
                  actionButton("reset_tech", "Reset", style="color: #fff; background-color: red; border-color: #298b3"),
                  
                )),
                column(6,
                       includeHTML("uncertain.html"),
                       #p("Step 3 of the tool allows you to investigate uncertainties associated with each technology. You can see projected ranges 
                      # for generation and cost of each technology. Once you navigate to a given technology horizontal and vertical error bars will 
                       #one the select bar. The black vertical bars are the range of cost values. The blue vertical bar is the lower range of generation,
                       #and the red horizontal bar is the upper range of generation. You can adjust the uncertainties to match your preferences using the 
                      #   sliders on the left. You need to press “Change Value” to lock in your adjustments before moving on to the next technology. "),
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
                      #onInitialize = I('function() { this.setValue(""); }')
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
        tabItem(tabName = "contact")
      
      )#end tabItems
    ) #end dashboard Body 
     
  ) #end dashboardPage
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
 output$tech_control <- renderUI({         
    tech <- input$checkGroup

    selectizeInput(
      inputId = 'tech', 'Chosen Technology', choices = tech)
  })
  ############## CHANGE SLIDER VALUES ##########################
  
  observe({
    currentTech <- input$tech
    val <- as.numeric(storeData()[currentTech, "LCOE_2050_Mean_User"])
    updateSliderInput(session, "newCost", value = val)
  })
  
  observe({
    currentTech <- input$tech
    val <- as.numeric(storeData()[currentTech, "Generation_2050_Mean_User"])
    updateSliderInput(session, "maxGW", value = val)
    
  })
  
  observe({
    currentTech <- input$tech_err
    minFrac <- as.numeric(storeData()[currentTech, "Generation_2050_Min_User"])
    maxFrac <- as.numeric(storeData()[currentTech, "Generation_2050_Max_User"])
    updateSliderInput(session, "rangeGW", value = c(minFrac,maxFrac))
  })
  
  observe({
    currentTech <- input$tech_err
    minFrac <- as.numeric(storeData()[currentTech, "LCOE_2050_Min_User"])
    maxFrac <- as.numeric(storeData()[currentTech, "LCOE_2050_Max_User"])
    updateSliderInput(session, "rangeCost", value = c(minFrac,maxFrac))
  })
  
  observe({
    currentTech <- input$tech_time
    rateDataPlot <- storeData()
    adopt <- rateDataPlot[rateDataPlot$Technology == input$tech_time, "adoption"]
    updateSliderInput(session, "adopt", value = adopt)
  })
  
  observe({
    currentTech <- input$tech_time
    rateDataPlot <- storeData()
    retire <- rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire"]
    updateSliderInput(session, "retire", value = retire)
  })
  
  observe({
    currentTech <- input$tech_time
    rateDataPlot <- storeData()
    learn <- rateDataPlot[rateDataPlot$Technology == input$tech_time, "learn"]
    updateSliderInput(session, "learn", value = learn)
  })
  
  
  ################## PLOT FOR SUPPLY CURVE #####################
  
  output$plot1 <- renderPlot({
    cleanData <- storeData()
    cleanData <- as.data.frame(cleanData)
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    
    demandData <- data.frame(demand = demand,
                             Goal = "2050 Goal Demand")
    cleanData <- subset(cleanData, Technology %in% input$checkGroup)


      row.names(cleanData) <- cleanData$Technology
      cleanData[input$tech,13] <- as.numeric(input$maxGW)
      cleanData[input$tech, 6] <- as.numeric(input$newCost)
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
  
  #################### ERROR GRAPH ####################
  output$errorPlot <- renderPlot({

    #if (input$tech != ''){
      cleanData <- storeData()
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
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
  
  ###################ERROR BUTTON PLOT##################
  observeEvent(input$maxPminC,{
    output$errorPlot <- renderPlot({
    #if (input$tech != ''){
    cleanData <- storeData()
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    demandData <- data.frame(demand = demand,
                             Goal = "2050 Goal Demand")
    cleanData <- as.data.frame(cleanData)
    row.names(cleanData) <- cleanData$Technology
    cleanData <- subset(cleanData, Technology %in% input$checkGroup)
    cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
    cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
    cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
    cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
    cleanData <- cleanData[order(cleanData$LCOE_2050_Min_User),]
    cleanData$w <- cumsum(cleanData$Generation_2050_Max_User)
    cleanData$wm <- cleanData$w - cleanData$Generation_2050_Max_User
    cleanData$wt <- with(cleanData, wm + (w- wm)/2)
    cleanData$mid <- (cleanData$w + cleanData$wm)/2
    
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
  
  ###################ERROR BUTTON PLOT##################
  observeEvent(input$maxPmaxC,{
    output$errorPlot <- renderPlot({
      #if (input$tech != ''){
      cleanData <- storeData()
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
      cleanData <- cleanData[order(cleanData$LCOE_2050_Max_User),]
      cleanData$w <- cumsum(cleanData$Generation_2050_Max_User)
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Max_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
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
  
  observeEvent(input$maxPmaxC,{
    output$testPlot <- renderPlot({
      if (input$tech != ''){
        cleanData <- storeData()
        cleanData[input$tech,5] <- input$num
        cleanData[, 2] <- cleanData[,3]
        cleanData[, 5] <- cleanData[,6]
        cleanData <- cleanData[order(cleanData$cost),]
        cleanData$w <- cumsum(cleanData$abatement)
        cleanData$wm <- cleanData$w - cleanData$abatement
        cleanData$wt <- with(cleanData, wm + (w- wm)/2)
        cleanData$mid <- (cleanData$w + cleanData$wm)/2
        
        ggplot(cleanData, aes(ymin = 0)) + geom_rect(aes(xmin = wm, xmax = w,
                                                         max = cost, fill = techName)
        )  + labs(x = "GW of low carbon electricity", y = "Total Abatement Cost (Billions of Dollars)"
        ) + geom_text(aes(
          x=wt, y=cost/1, label = techName, angle=90), size=6) + theme_bw() + theme(
            text = element_text(size=20)) + theme(legend.position =  "none")
        
        
        
      }
    })
  })
  
  ###################ERROR BUTTON PLOT##################
  observeEvent(input$minPminC,{
    output$errorPlot <- renderPlot({
      #if (input$tech != ''){
      cleanData <- storeData()
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
      cleanData <- cleanData[order(cleanData$LCOE_2050_Min_User),]
      cleanData$w <- cumsum(cleanData$Generation_2050_Min_User)
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Min_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
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
  
  ###################ERROR BUTTON PLOT##################
  observeEvent(input$minPmaxC,{
    output$errorPlot <- renderPlot({
      #if (input$tech != ''){
      cleanData <- storeData()
      demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
      demandData <- data.frame(demand = demand,
                               Goal = "2050 Goal Demand")
      cleanData <- as.data.frame(cleanData)
      row.names(cleanData) <- cleanData$Technology
      cleanData <- subset(cleanData, Technology %in% input$checkGroup)
      cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
      cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
      cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
      cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
      cleanData <- cleanData[order(cleanData$LCOE_2050_Max_User),]
      cleanData$w <- cumsum(cleanData$Generation_2050_Min_User)
      cleanData$wm <- cleanData$w - cleanData$Generation_2050_Min_User
      cleanData$wt <- with(cleanData, wm + (w- wm)/2)
      cleanData$mid <- (cleanData$w + cleanData$wm)/2
      
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
  
#############RESET BUTTON################
  observeEvent(input$reset_tech, {
  output$errorPlot <- renderPlot({
    
    #if (input$tech != ''){
    cleanData <- storeData()
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    demandData <- data.frame(demand = demand,
                             Goal = "2050 Goal Demand")
    cleanData <- as.data.frame(cleanData)
    row.names(cleanData) <- cleanData$Technology
    cleanData <- subset(cleanData, Technology %in% input$checkGroup)
    cleanData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
    cleanData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
    cleanData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
    cleanData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin
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
})
  
  ##################TIME PLOT##################
  output$timePlot <- renderPlot({
    demand <- (4243 * (1 + (input$demand/100))^25) * (input$goal/100)
    demandData <- data.frame(demand = demand,
                             Goal = "2050 Goal Demand")
    timeDataPlot <- timeData()
    rateDataPlot <- storeData()
    
    timeDataPlot <- as.data.frame(timeDataPlot)
    timeDataPlot <- subset(timeDataPlot, Technology %in% input$checkGroup)
    
    rateDataPlot <- as.data.frame(rateDataPlot)
    rateDataPlot <- subset(rateDataPlot, Technology %in% input$checkGroup)
    
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "adoption"] <- input$adopt
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire"] <- input$retire

    for (row in 1:nrow(timeDataPlot)) {
      years <- (timeDataPlot[row,"Year"] - 2020)/5
      adopt <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "adoption"] / 100
      retire <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "retire"] / 100
      inital <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "Generation_2020"]
 
      growth <- (adopt + 1) - retire
      years <- (years-1) * 5
      print(inital * (growth^(years)))
      timeDataPlot[row,"Generation"] <- inital * (growth^(years))
    }

    # stacked area chart
    ggplot(timeDataPlot, aes(x=Year, y=Generation, fill=Technology)) + 
      geom_area() + geom_hline(data = demandData, aes(yintercept = demand, color = Goal), linetype="dashed", size=1.5) + labs(x = "Year", y = "TWhr of carbon free electricity") + theme_bw() + theme(text = element_text(size=20))
  })
  
  output$learningPlot <- renderPlot({
    timeDataPlot <- timeData()
    rateDataPlot <- storeData()
    
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
      retire <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "retire"] / 100
      inital <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "Generation_2020"]
      inital_cost <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "LCOE_2020"]
      LR <- rateDataPlot[rateDataPlot$Technology==timeDataPlot[row,2], "learn"] / 100
      growth <- (adopt + 1) - retire
      
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
  #print(timeDataPlot)
    # stacked area chart
    ggplot(timeDataPlot, aes(x=Year, y=LCOE, group=Technology)) + 
      geom_line(aes(color=Technology), size=2) + labs(x = "Year", y = "LCOE ($/MWhr)"
                               ) + theme_bw() + theme(
                                 text = element_text(size=20))
  })
  
  #############SUBMIT BUTTONS##################
  observeEvent(input$submit, {
    outData <- storeData()
    outData[input$tech,13] <- as.numeric(input$maxGW)
    outData[input$tech, 6] <- as.numeric(input$newCost)
    inital <- as.numeric(outData[input$tech, "Generation_2020"])
    final <- as.numeric(input$maxGW)
    outData[input$tech, "adoption"] <-  (((final/inital)^(1/25))-1)*100
    storeData(outData)
    
  })
  
  observeEvent(input$submitCostUncert, {
    outData <- storeData()
    outData[input$tech_err, "LCOE_2050_Max_User"] <-input$rangeCost[2]  #costMax
    outData[input$tech_err, "LCOE_2050_Min_User"] <- input$rangeCost[1]  #costMin 
    outData[input$tech_err, "Generation_2050_Max_User"] <- input$rangeGW[2]    #abatmentMAx
    outData[input$tech_err, "Generation_2050_Min_User"] <- input$rangeGW[1]    #abatmentMin

    storeData(outData)
  })
  
  observeEvent(input$submit_time, {
    rateDataPlot <- storeData()
    
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "adoption"] <- input$adopt
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "retire"] <- input$retire
    rateDataPlot[rateDataPlot$Technology == input$tech_time, "learn"] <- input$learn
    #print(rateDataPlot)
    storeData(rateDataPlot)

  })
  

  ###########INFO BOX###########
  output$demand <- renderInfoBox({
    outData <- storeData()
    inital <- as.numeric(outData[input$tech, "Generation_2020"])
    final <- as.numeric(input$maxGW)
    adopt <-  ((final/inital)^(1/25))-1
    adopt <- adopt * 100
    infoBox("Current Adoption Rate", value = paste0(round(adopt, 1), "%"), width = 8, icon = icon("fas fa-bolt"))
  })
  
  output$remainGoal <- renderInfoBox({
    generation <- storeData()
    total <- sum(generation$Generation_2050_Mean_User)
    rate <- generation[generation$Technology == input$tech, "maxGrowth"]
    infoBox("Max Historic Rate", value = rate, width = 4, icon = icon("fas fa-bolt"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
