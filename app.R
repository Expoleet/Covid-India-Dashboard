library(shiny)
library(shinydashboard)
library(ggplot2)
library(tseries)
library(forecast)
library(magrittr)
library(timeDate)
library(dplyr)
library(zoo)
library(shinythemes)
library(plotrix)
library(RColorBrewer)
library(reshape)
library(tidyverse)
library(scales)


header <- dashboardHeader(title = "Basic Dashboard")
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        
        selectInput("graphs", "Select a graph:",
                    choices = c('Select','Current Status','','Age Wise','Gender Wise','Recovered vs Cases','TimeSeries'),
                    selected = 'Select'),
        #selectizeInput(,choices=statevector,selected = "Select"),
        selectizeInput("state", "Select State", choices = statevector),
        numericInput("tst", "Estimated No of test ", value="")
        
    ))
agedataset <- read.csv('AgeGroupDetails.csv',stringsAsFactors = F,header=T)
covidind <- read.csv('covid_19_india.csv',stringsAsFactors = F,header = T)
hosbed <- read.csv('HospitalBedsIndia.csv',stringsAsFactors = F,header = T)
individual <- read.csv('IndividualDetails.csv',stringsAsFactors = F,header = T)
testing <- read.csv('icmrr.csv',stringsAsFactors = F,header = T)
hosptialbeddata <- read.csv('Beds Available and Patients.csv',stringsAsFactors = F,header = T)
dailydata <- read.csv('dailydataa.csv',stringsAsFactors = F,header = T)


uii <- fluidPage(theme = shinytheme("slate"),
                 frow1 <- fluidRow(
                     valueBoxOutput("value1"),
                     valueBoxOutput('value2'),
                     valueBoxOutput('value3')
                     
                 ),
                 frow2 <- fluidRow( 
                     box(
                         width = '100%',
                         title = "Cases State Wise"
                         ,status = "primary"
                         ,solidHeader = TRUE 
                         ,collapsible = TRUE 
                         ,div(style = 'height: 50vh;overflow-x: auto;overflow-y: auto',plotOutput("revenuebyPrd", height = "1000px",width = "1500px"))
                     ),
                     box(
                         title = "Graphs"
                         ,status = "primary"
                         ,solidHeader = TRUE 
                         ,collapsible = TRUE,
                         width = '100%',
                         tabBox(
                             id = "",
                             side = "right",
                             title = "Graphs",
                             width = '100%',
                             div(
                                 tabPanel(plotOutput("graphs"),title="Graph"))
                         )),
                   
                     box(
                       title = "Graphs"
                       ,status = "primary"
                       ,solidHeader = TRUE 
                       ,collapsible = TRUE,
                       tabBox(
                         id = "",
                         side = "right",
                         title = "Graphs",
                         width = '100%',
                         div(
                           tabPanel(plotOutput("state"),title="Graph"))
                       )),
                     box(
                       title = "Predicited  Number of Positive Cases Over Samples"
                       ,status = "primary"
                       ,solidHeader = TRUE 
                       ,collapsible = TRUE 
                       ,verbatimTextOutput('tp')
                     )
                     

                 )
)
body <- dashboardBody(uii)
ui <- dashboardPage(title = 'Covid Dashboard', header, sidebar,body) 
                
server <- function(input, output) {
    latest <- subset.data.frame(covidind,covidind$Date=='12/04/20')
    confirmed = sum(latest$Confirmed)
    recoverd = sum(latest$Cured)
    deaths= sum(latest$Deaths)
    topregion <- latest %>% group_by(State.UnionTerritory) %>% summarise(value = sum(Confirmed)) %>% filter(value==max(latest$Confirmed))
    totaltestedpeople <- c(as.integer(testing$TotalSamplesTested)) 
    totalnotp <- testing$TotalSamplesTested[31]

  
    
    
   
  
    
    
    
     output$value1 <- renderValueBox({
        valueBox(
            formatC(topregion$value, format="d", big.mark=',',width = 1)
            ,paste('Top Region :',topregion$State.UnionTerritory)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")  
    })
    
    output$value2 <- renderValueBox({
        valueBox(
            formatC(confirmed, format="d", big.mark=',',width = 1)
            ,paste('Confirmed Cases India')
            ,icon = icon("stats",lib='glyphicon')
            ,color = "green")  
    })
    output$value3 <- renderValueBox({
        valueBox(
            formatC(totalnotp, format="d", big.mark=',',width = 1)
            ,paste('TotalSamplesTested')
            ,icon = icon("stats",lib='glyphicon')
            ,color = "yellow")  
    })
    output$revenuebyPrd <- renderPlot({
        ggplot(data = latest, 
               aes(x=State.UnionTerritory, y=Confirmed, fill=factor(State.UnionTerritory))) + 
                geom_bar(position = "dodge", stat = "identity") + ylab("No of Person ") + 
                xlab("STATES") + theme(legend.position="bottom" 
                ,plot.title = element_text(size=15, face="bold")) + 
                labs(fill = "Region") +
        theme(
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.ticks  = element_blank(),
          panel.grid = element_blank()
          
        )
   
    })
    output$graphs <- renderPlot({
        if(input$graphs == 'Current Status'){
            
            slices <- c(confirmed,recoverd,deaths)
            lbls <- c('Active \n Cases   ','\n\n\nCured    \n   ','              \t  Death     \n       ')
            lbls <- paste(lbls, slices)
            pie3D(slices,radius=0.9,labels=lbls,explode=0.3,
                main="Pie Chart of Current Status ")
            
        }
        else if(input$graphs == 'Age Wise'){
            ggplot(data =agedataset,aes(x=AgeGroup, y=TotalCases,group = 1) ) +
                geom_line(linetype = "dashed",color = "orange") +
                geom_point() +
                ggtitle("Age Wise Effects")  +
           theme(
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks  = element_blank(),
                  panel.grid = element_blank()
                  
                ) +
            annotate("text",
                     x=5,
                     y= 150,
                     label = "Age Wise "
            )
        } 
        else if(input$graphs == 'Gender Wise'){
          plot(agedataset$Male,type = "o",col = "red", xlab = "Age Group", ylab = "No of Male And Female", 
                main = "Gender  chart")
          axis(1, at=1:10, labels=c(agedataset$AgeGroup))
          lines(agedataset$Female, type = "o", col = "blue")
        }
      else if(input$graphs == 'Recovered vs Cases'){
        datee=seq(as.Date('2020-01-30'),as.Date('2020-04-13'),by = 1)
        dailyconfirmed=c(dailydata$Daily.Confirmed)
        dailyrecovered=c(dailydata$Daily.Recovered)
        test_data_long <- melt(datee)
       # df11 <- data.frame("Date "= test_data_long,"confirm"=dailyconfirmed,"recovered"=dailyrecovered)
        ggplot(df11, aes(x=value,group = 1)) +
          geom_line(aes(y = confirm), color = "darkred", size = 1.5) +
          geom_line(aes(y = recovered), color = "steelblue", size = 1.5) +
          scale_x_date(labels = date_format("%m-%d"),breaks = date_breaks(width = "7 day")) +
          theme(
            panel.background = element_blank(),
            panel.border = element_blank(),
              axis.title = element_blank(),
              axis.ticks  = element_blank(),
            panel.grid = element_blank()
                
                ) +
          annotate("text",
                   x =as.Date("04/16/2020", "%m/%d/%Y"),
                   y=c(700,80),
                   label = c("Total confirm","Total Recovered")
                   )
          
        
          
      
       
        
      }
        else if(input$graphs == 'TimeSeries'){
          ggplot(data=dailydata,aes(x=df11$value,y=Total.Confirmed)) +
            geom_line(color = "orange") +
            geom_point()+
            ggtitle("Time Series Positive Cases")  +
            scale_x_date(labels = date_format("%m-%d"),breaks = date_breaks(width = "1 month")) +
            theme(
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.title = element_blank(),
              axis.ticks  = element_blank(),
              panel.grid = element_blank()
              
            ) +
            annotate("text",
                     x =as.Date("04/16/2020", "%m/%d/%Y"),
                     y=9000,
                     label = "Positive Case"
            )
        }
      
      
      
      
      
    })
    output$state <- renderPlot({
      strrr=as.character(input$state)
      z=which(statevector==strrr)
      df = data.frame("beds"=hosptialbeddata$Beds[z],"Cases"=hosptialbeddata$Cases[z])
   df2 = data.matrix(df)
   #df3 = table("beds"=hosptialbeddata$Beds[z],"Cases"=hosptialbeddata$Cases[z])
   barplot(df2, main="Beds VS Cases",
           xlab=strrr, col=c("darkblue","red"),
           legend = rownames(strrr), beside=TRUE)
    })
    
    
    
    
    output$tp <- function(){
      tst=c(as.integer(testing$TotalSamplesTested))
      tpc=c(testing$TotalPositiveCases)
      val=lm(tpc~tst)
      intr=as.integer(input$tst)
      a <- data.frame(tst=intr)
      res=as.integer(predict.lm(val,a,data=testing))
      return(res)
      
    }
   
    

}


shinyApp(ui = ui, server = server)
