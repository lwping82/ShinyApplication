# Loading required libraries
library(lubridate)
library(ggplot2)

# Declaring variables
label1Vote <- 0
label2Vote <- 0
label3Vote <- 0
label4Vote <- 0
label5Vote <- 0
label6Vote <- 0
label7Vote <- 0
label8Vote <- 0
label9Vote <- 0
label10Vote <- 0
label11Vote <- 0
label12Vote <- 0

dataSet1 <- NULL
dataSet2 <- NULL
dataSet3 <- NULL
dataSet4 <- NULL
dataSet5 <- NULL
dataSet6 <- NULL
dataSet7 <- NULL
dataSet8 <- NULL
dataSet9 <- NULL
dataSet10 <- NULL
dataSet11 <- NULL
dataSet12 <- NULL

duration <- 1
year <- year(Sys.Date())

shinyServer(function(input, output) {
  # Loadind the data file
  dat <- NULL
  
  tryCatch ({
    dat <- read.csv("data.csv", header = TRUE)

    # Process the data file in subsetting the dataset based on each character then calculate the number of vote available for each of the characters
    dataSet1 <- subset(dat, dat$CHARACTER=='Mickey Mouse')
    dataSet2 <- subset(dat, dat$CHARACTER=='Snow White')
    dataSet3 <- subset(dat, dat$CHARACTER=='Lighting McQueen')
    dataSet4 <- subset(dat, dat$CHARACTER=='Sheriff Woody')
    dataSet5 <- subset(dat, dat$CHARACTER=='Tigger')
    dataSet6 <- subset(dat, dat$CHARACTER=='Aladdin')
    dataSet7 <- subset(dat, dat$CHARACTER=='Nemo')
    dataSet8 <- subset(dat, dat$CHARACTER=='Elsa')
    dataSet9 <- subset(dat, dat$CHARACTER=='Stitch')
    dataSet10 <- subset(dat, dat$CHARACTER=='Tinker Bell')
    dataSet11 <- subset(dat, dat$CHARACTER=='Blade Ranger')
    dataSet12 <- subset(dat, dat$CHARACTER=='Tarzan')    
    
    label1Vote <<- nrow(dataSet1)
    label2Vote <<- nrow(dataSet2)
    label3Vote <<- nrow(dataSet3)
    label4Vote <<- nrow(dataSet4)
    label5Vote <<- nrow(dataSet5)
    label6Vote <<- nrow(dataSet6)
    label7Vote <<- nrow(dataSet7)
    label8Vote <<- nrow(dataSet8)
    label9Vote <<- nrow(dataSet9)
    label10Vote <<- nrow(dataSet10)
    label11Vote <<- nrow(dataSet11)
    label12Vote <<- nrow(dataSet12)  
    
  }, warning = function(war) {
    print(paste("WARNING:  ",war))
  },  error = function(err) {
    print(paste("ERRORyearList <- list(c(uniqueYear)):  ",err))
  }, finally = {
    print(paste("FINALLY:  "))
  })
  
  # Display the calculated vote to each of the voting result label for each character
  output$label1 <- renderText({ label1Vote })
  output$label2 <- renderText({ label2Vote })
  output$label3 <- renderText({ label3Vote })
  output$label4 <- renderText({ label4Vote })
  output$label5 <- renderText({ label5Vote })
  output$label6 <- renderText({ label6Vote })
  output$label7 <- renderText({ label7Vote })
  output$label8 <- renderText({ label8Vote })
  output$label9 <- renderText({ label9Vote })
  output$label10 <- renderText({ label10Vote })
  output$label11 <- renderText({ label11Vote })
  output$label12 <- renderText({ label12Vote })
  
  # Set the instruction in statistics tab
  output$labelStatus <- renderText({ "Make your selections from the left then click on \"Submit\" button to generate the statistics" })
  
  # Observing the created HTML objects
  observeEvent(input$but1, {
    castVote('Mickey Mouse', 'M.Mouse', 'N')
    label1Vote <<- label1Vote + 1
    output$label1 <- renderText({ label1Vote })    
  })  
  
  observeEvent(input$but2, {
    castVote('Snow White', 'S.White', 'Y')
    label2Vote <<- label2Vote + 1
    output$label2 <- renderText({ label2Vote })     
  }) 
  
  observeEvent(input$but3, {
    castVote('Lighting McQueen', 'L.McQueen', 'N')
    label3Vote <<- label3Vote + 1
    output$label3 <- renderText({ label3Vote })     
  }) 
  
  observeEvent(input$but4, {
    castVote('Sheriff Woody', 'S.Woody', 'Y')
    label4Vote <<- label4Vote + 1
    output$label4 <- renderText({ label4Vote })     
  })  
  
  observeEvent(input$but5, {
    castVote('Tigger', 'Tigger', 'N')
    label5Vote <<- label5Vote + 1
    output$label5 <- renderText({ label5Vote })     
  }) 
  
  observeEvent(input$but6, {
    castVote('Aladdin', 'Aladdin', 'Y')
    label6Vote <<- label6Vote + 1
    output$label6 <- renderText({ label6Vote })     
  }) 
  
  observeEvent(input$but7, {
    castVote('Nemo', 'Nemo', 'N')
    label7Vote <<- label7Vote + 1
    output$label7 <- renderText({ label7Vote })     
  })  
  
  observeEvent(input$but8, {
    castVote('Elsa', 'Elsa', 'Y')
    label8Vote <<- label8Vote + 1
    output$label8 <- renderText({ label8Vote })     
  }) 
  
  observeEvent(input$but9, {
    castVote('Stitch', 'Stitch', 'N')
    label9Vote <<- label9Vote + 1
    output$label9 <- renderText({ label9Vote })     
  }) 
  
  observeEvent(input$but10, {
    castVote('Tinker Bell', 'T.Bell','Y')
    label10Vote <<- label10Vote + 1
    output$label10 <- renderText({ label10Vote })     
  })  
  
  observeEvent(input$but11, {
    castVote('Blade Ranger', 'B.Ranger', 'N')
    label11Vote <<- label11Vote + 1
    output$label11 <- renderText({ label11Vote })     
  }) 
  
  observeEvent(input$but12, {
    castVote('Tarzan', 'Tarzan', 'Y')
    label12Vote <<- label12Vote + 1
    output$label12 <- renderText({ label12Vote })     
  })  
  
  observeEvent(input$monthDropdown, {
  })    
  
  observeEvent(input$yearDropdown, {
  })  
  
  observeEvent(input$but13, {
    generateChart()
  })   
  
  # Method to generate chart
  generateChart <- function() {

    duration <- input$monthDropdown
    year <- input$yearDropdown 
       
    output$labelStatus <- renderText({ "" })
    
    dat <- read.csv("data.csv", header = TRUE)
    
    filteredDataSet <- dat
    if(duration=="1") {
      print(paste("processing group 1 duration"))
      filteredDataSet <- dat[dat$YEAR==year&dat$MONTH %in% c(1,2,3,4,5,6), ]
    } else {
      print(paste("processing group 2 duration"))
      filteredDataSet <- dat[dat$YEAR==year&dat$MONTH %in% c(7,8,9,10,11,12), ]
    }
    
    if(nrow(filteredDataSet)>0) {
      output$distPlot <- renderPlot({   
        plotChart <- ggplot(filteredDataSet, aes(MONTH, VOTE, fill=SHORT_NAME, stat="identity")) + geom_bar(stat="identity") + scale_x_continuous(breaks=c(unique(dat$MONTH))) + facet_grid(MONTH~SHORT_NAME, scale="fixed", space="free")#facet_grid(CHARACTER, scales="free", space="free")
        plotChart <- plotChart + guides(fill = guide_legend(title = "Character", title.theme = element_text(size=15, face="italic", colour = "red", angle = 0)))
        plotChart <- plotChart + theme(legend.position="bottom")
        
        print(plotChart)
      })    
    } else {
      output$labelStatus <- renderText({ "No matching record found. Please make a difference selections then try again." })
    }
  }
  
  # Method to write casted vote to the file
  castVote <- function(charName, charShortName, isHumanFigure) {
    tryCatch ({
      dat <- read.csv("data.csv", header = TRUE)
      
      df2<-data.frame(no=c(nrow(dat)+1), voteDate=c(Sys.Date()), day(Sys.Date()), month(Sys.Date()), year(Sys.Date()), timeZone=c(Sys.timezone()), character=c(charName), humanFigure=c(isHumanFigure), paste(1), charShortName)
      write.table(df2,"data.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
    }, warning = function(war) {
      print(paste("Error encountered when generating requested chart. Please contact the system administrator."))
    },  error = function(err) {
      print(paste("Data not found when system was attempting to generate the requested chart."))
    }, finally = {
    })    
  }  
})

