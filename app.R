
#By Alex Braksator


library(shiny)
library(plotly)
library(plyr)
library(dplyr)
library(ggplot2)
library(broom)

library(forecast)
library(shinyjs)

#Read and clean data
vgData <- data_frame(nrow = 16598, ncol = 15)
vgData <- read.csv("vgsales.csv", stringsAsFactors = FALSE, header = TRUE) 
vgData$ignRating = as.numeric(vgData$ignRating)
vgData$Year <- as.numeric(vgData$Year)

other = c("GB","DS","PS2","SNES","GBA","3DS","PS","XB","2600","PSP","GC","WiiU","GEN","DC",
          "PSV","SAT","SCD","WS","NG","TG16","3DO","GG","PCFX")



# Define UI for application that builds scatterplots
ui <- shinyUI(
  fluidPage(
  
  shinyjs::useShinyjs(),
  fluidRow(
    titlePanel("Video Game Sales Dashboard by Alex Braksator", windowTitle = "Video Game Analytics"),
    column(width = 6, 
      selectInput("YAxis", label = h3("Select Y Variable"), 
                choices = list("Global Sales" = 1, "NA_Sales" = 2, "EU_Sales" = 3
                               ,"JP_Sales" = 4, "Other_Sales" = 5), 
                selected = 1)
    ),
    column(width = 6,
      selectInput("XAxis", label = h3("Select X Variable"), 
                choices = list("ignReview" = 1, "Year" = 2), 
                selected = 2)
    )
  ),
  
  fluidRow(
    titlePanel("Filters"),
    column(width = 6,
      sliderInput("Year", label = h3("Year Range"), min = 1980, 
                max = 2016, value = c(2000, 2016))
    ),
    column(width = 6,
    sliderInput("Score", label = h3("IGN Score Range"), min = 0, 
                max = 10, value = c(0, 10))
    ),
    column(width = 6,
           checkboxGroupInput("Platform", label = h3("Filter Platforms"),
                              choices = list("XOne"="XOne","X360"="X360","PS3"="PS3","PS4"="PS4","PS"="PS","PC"="PC","NES"="NES","N64"="N64",
                                             "Other"=0),
                              selected = c("XOne","X360","PS3","PS4","PS","PC","NES","N64",0))
           
    ),
    column(width = 6,
           checkboxGroupInput("Genre", label = h3("Filter Genre"),choiceNames = sort(unique(vgData$Genre)),
                              choiceValues = unique(vgData$Genre), selected = unique(vgData$Genre))
    ),
    column(width = 6,
           selectInput("Publisher", label = h3("Filter Publisher"),choices = c("ALL",unique(vgData$Publisher)))
    )
    
  ),
  


  
  fluidRow(
    column(width = 6,
           radioButtons("Color", label = h3("Color By"),
                        choices = list("EditorsChoice" = 1, "Genre" = 2, "Publisher" = 3, "Platform" = 4), 
                        selected = 3)
    ),
    column(width = 6,
           radioButtons("LinReg", label = h3("Forecast:"),
                        choices = list("None"=0, "ARIMA" = 1, "Exponential Smoothing" = 2), 
                        selected = 2)
    )
  ),
  
  fluidRow(
    column(12, align = "center",
    mainPanel(plotlyOutput("graph",width="1000px", height = "550px")))
  )
))



server <- shinyServer(function(input, output, session) {
  
  observe({
    if (input$XAxis == 1) {
      shinyjs::hide("LinReg")
      shinyjs::show("Color")
    } else {
      shinyjs::show("LinReg")
      shinyjs::hide("Color")
    }
  })
  
 
  
   #Reactive function
   output$graph <- renderPlotly({
     
     
     #Access our input variables from UI.R
     YAxis = input$YAxis
     XAxis = input$XAxis
     minYear = input$Year[1]
     maxYear = input$Year[2]
     minScore = input$Score[1]
     maxScore = input$Score[2]
     genres = input$Genre
     publishers = input$Publisher
     platforms = input$Platform
     
     

     
     if(is.null(genres)){
       print("Please Select at least one Genre")
     }
     if(is.null(publishers)){
       message("Please Select at least one Publisher")
     }
     
     if(publishers == "ALL"){
       publishers = unique(vgData$Publisher)
     }
     if("0" %in% platforms){
       platforms = c(platforms, other) 
     }
     
         
     #Filter by YEAR and IGNRATING using dplyr
     filteredVGs <- vgData %>%
       filter(
         Year >= minYear,
         Year <= maxYear,
         ignRating >= minScore,
         ignRating <= maxScore,
         Genre %in% genres,
         Publisher %in% publishers,
         Platform %in% platforms

       ) %>%
       arrange(Global_Sales)

     
     #Filter by COLOR
     col <- rep("N/A", nrow(filteredVGs))
     if(input$Color == 1){        # Color by Editor's Choice
       col = rep("N/A", nrow(filteredVGs))
       col[which(filteredVGs$EditorsChoice == "Y")] <- "Yes"
       col[which(filteredVGs$EditorsChoice == "N")] <- "No"
     }
     else if(input$Color == 2){   # Color by Genre
       for(i in factor(filteredVGs$Genre)){
         col[which(filteredVGs$Genre == i)] <- i
       }
     }
     if(input$Color == 3){         # Color by Publisher
       col = rep("Other", nrow(filteredVGs))
       col[which(filteredVGs$Publisher == "Nintendo")] <- "Nintendo"
       col[which(filteredVGs$Publisher == "Electronic Arts")] <- "EA"
       col[which(filteredVGs$Publisher == "Sony Computer Entertainment")] <- "Sony"
     }
     if(input$Color == 4){         # Color by Platform
#        col = rep("Other", nrow(filteredVGs))
#        col[which(filteredVGs$Platform == "Wii")] <- "Wii"
       for(i in factor(filteredVGs$Platform)){
         col[which(filteredVGs$Platform == i)] <- i
       }
     }
     
     avgMoney <- c()
     #Determine our AXIS 
     if(input$XAxis==1){
       xAx = filteredVGs$ignRating
       xlab<- list(
         title = "IGN Rating",
         showticklabels = TRUE,
         tickangle = 0,
         exponentformat = "E"
       )
     }
     else{
       xAx = filteredVGs$Year
       xlab<- list(
         title = "Year",
         showticklabels = TRUE,
         tickangle = 0,
         exponentformat = "E"
       )
     }
     if(input$YAxis==1){
       yAx = filteredVGs$Global_Sales
       ylab<- list(
         title = "Global Sales ($ million)",
         showticklabels = TRUE,
         tickangle = 0,
         exponentformat = "E"
       )
     }
     else if(input$YAxis==2){
       yAx = filteredVGs$NA_Sales
       ylab<- list(
         title = "North American Sales ($ million)",
         showticklabels = TRUE,
         tickangle = 0,
         exponentformat = "E"
       )
     }
     else if(input$YAxis==3){
       yAx = filteredVGs$EU_Sales
       ylab<- list(
         title = "European Sales ($ million)",
         showticklabels = TRUE,
         tickangle = 0,
         exponentformat = "E"
       )
     }
     else if(input$YAxis==4){
       yAx = filteredVGs$JP_Sales
       ylab<- list(
         title = "Japanese Sales ($ million)",
         showticklabels = TRUE,
         tickangle = 0,
         exponentformat = "E"
       )
     }
     else if(input$YAxis==5){
       yAx = filteredVGs$Other_Sales
       ylab<- list(
         title = "Other Sales ($ million)",
         showticklabels = TRUE,
         tickangle = 0,
         exponentformat = "E"
       )
     }
  #PLOTTING   
  if(input$XAxis==1) #Plot with IGNRATING on XAXIS
     if(input$LinReg == 1) { #Plot with regression
        xAx = filteredVGs$ignRating
        m <- loess(yAx ~ xAx)  
        plot_ly(filteredVGs, y = yAx, x = xAx,
             text = ~paste("Game: ", Name, "<br>Platform:", Platform,"<br>Year:", Year, "<br>Publisher:", Publisher, "<br>Genre:", Genre), 
             color = ~col,
             type = "scatter")%>%
        add_lines(y = fitted(m),
             line = list(color = 'BLACK'),
             name = "Loess Smoother", showlegend = FALSE)%>%
          layout(xaxis = xlab, yaxis = ylab)
     }
     
     else{ #Plot without regression
       plot_ly(filteredVGs, y = yAx, x = xAx,
               text = ~paste("Game: ", Name, "<br>Platform:", Platform,"<br>Year:", Year, "<br>Publisher:", Publisher, "<br>Genre:", Genre), 
               color = ~col,
               type = "scatter")
     }
     
  else if(input$XAxis==2){ #Plot with YEAR on XAXIS
      newDF <- data.frame(xAx, yAx)
      newDF[, 1] <- round(newDF[, 1])
      sumMoney <- ddply(newDF, .(xAx), summarize, xm=sum(yAx))[2]
      sumMoneyF <- sumMoney[, 1]
      year <- c(1980:2016, 2017, 2020)
      num1 <- maxYear-1980+1
      num2 <- minYear-1980+1
      year <- year[num2:num1]
      newerDF <- data.frame(year, sumMoneyF)
      
      vect <- newerDF[,2]
      qxts <- structure(list(y = vect, date = as.Date(paste(seq(minYear, maxYear), 1, 1, sep = "-"))))
      fit <- auto.arima(qxts$y, approximation = FALSE, D=1)
      ets_fit <- ets(qxts$y)
      
      print(qxts$date)
      
      if(input$LinReg == 0){
        plot_ly() %>%
          add_lines(x = as.POSIXct(qxts$date, origin='1970-01-01'), y = qxts$y,
                    color = I("black"), 
                    name = "observed", 
                    marker=list(mode='lines'))
      }
      else{
      forecast_length <- 5
      if(input$LinReg == 1){
        fore.xts <- forecast(fit, h = forecast_length, level = c(80, 95))
      }
      else{
        fore.xts <- forecast(ets_fit, h = forecast_length, level = c(80, 95))
      }
      fore.dates <- seq(as.POSIXct(qxts$date[length(qxts$date)], origin='1970-01-01'), by=qxts$date[length(qxts$date)] - qxts$date[length(qxts$date)-1], len=forecast_length)
      
      
      plot_ly() %>%
        add_lines(x = as.POSIXct(qxts$date, origin='1970-01-01'), y = qxts$y,
        color = I("black"), 
        name = "observed", 
        marker=list(mode='lines')) %>% 
        add_lines(x = fore.dates, y = fore.xts$mean, color = I("blue"), name = "prediction") %>%
        add_ribbons(x = fore.dates, 
        ymin = fore.xts$lower[, 2], 
        ymax = fore.xts$upper[, 2],
        color = I("gray95"), 
        name = "95% confidence") %>%
        add_ribbons(p, 
        x = fore.dates, 
        ymin = fore.xts$lower[, 1], 
        ymax = fore.xts$upper[, 1],
        color = I("gray80"), name = "80% confidence")%>%
        layout(xaxis = xlab, yaxis = ylab)
      }
    }
   })
})


# Run the application 
shinyApp(ui = ui, server = server)
