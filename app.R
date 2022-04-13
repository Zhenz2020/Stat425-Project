#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(ggplot2)
library(openxlsx)
library(readr)
library(mice)
library(Hmisc)
library(DT)

#replace temp with name of dataset
#days: how many days forward you want to predict
#conflow,confhigh: confidence interval for prediction
library(forecast)
#temp=read_csv("https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/New_GeniusPageView.csv")
predict_ts<-function(temp, songname, days, conflow, confhigh){
    n=ncol(temp)-which(names(temp)=="Day.1")
    m=((2*n+n)/n)+1
    if (days>m){
        warning("result may be inaccurate due to too many days ahead")
    }
    exp=na.omit(t(as.data.frame(temp[which(temp$Song==songname),])[which(names(temp)=="Day.1"):ncol(temp)]))
    mts<-ts(exp)
    plot(mts)
    fit <- auto.arima(mts)
    plot(forecast(fit, days, level=c(conflow,confhigh)))
}

#predict_ts(temp,"Kraazy",3,50,60)

PMM_function <- function(x){
    # PMM_function replaces missing values using pmm method
    newdata <- x
    data <- mice(newdata, m = 5, method = "pmm", maxit = 100, seed = 1)
    completedata <- complete(data)
    return(completedata)
} 


Mean_function <- function(x){
    # Mean_function replaces missing values using means.
    mean_list <- c()
    for (j in 1:dim(x)[2]){
        mean_list[j] <- mean(x[, j])
        for (i in 1:dim(x)[1]){
            if (x[i, j] == 0){
                # replace "0" by "NA"
                x[i, j] = mean_list[j]
            }
        }
    }
    return(x)
}

Median_function <- function(x){
    # Median_function replaces missing values using medians.
    median_list <- c()
    for (j in 1:dim(x)[2]){
        median_list[j] <- median(x[, j])
        for (i in 1:dim(x)[1]){
            if (x[i, j] == 0){
                # replace "0" by "NA"
                x[i, j] = median_list[j]
            }
        }
    }
    return(x)
}

Max_function <- function(x){
    # Max_function replaces missing values using maxs.
    max_list <- c()
    for (j in 1:dim(x)[2]){
        max_list[j] <- max(x[, j])
        for (i in 1:dim(x)[1]){
            if (x[i, j] == 0){
                # replace "0" by "NA"
                x[i, j] = max_list[j]
            }
        }
    }
    return(x)
}

Min_function <- function(x){
    # Min_function replaces missing values using mins.
    min_list <- c()
    for (j in 1:dim(x)[2]){
        min_list[j] <- min(x[, j])
        for (i in 1:dim(x)[1]){
            if (x[i, j] == 0){
                # replace "0" by "NA"
                x[i, j] = min_list[j]
            }
        }
    }
    return(x)
}

# Define UI for application that draws a histogram
ui <- navbarPage(title = "Final Project of Stat 427",

    # Application title
    tabPanel(title = "Data cleaning",
             fluidPage(
                    
                 h1("Data Cleaning & Visualization", style = "font-family:Times"),
    
    sidebarLayout(
        sidebarPanel(
            selectizeInput("url",
                           label = "Enter your URL or choose from one of the below datasets",
                           selected = NULL,
                           options = list(create = TRUE),
                           choices = c(
                                       "Genius Chart Placement" = "https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Genius%20Chart%20Placement.csv",
                                       "Genius Pageviews"="https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Genius%20Pageviews.csv",
                                       "Shazam Count" = "https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Shazam%20Count.csv",
                                       "Shazam US Top 200 Placement"="https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Shazam%20US%20Top%20200%20Placement.csv",
                                       "SoundCloud Play Counts" = "https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20SoundCloud%20Play%20Counts.csv",
                                       "SoundCloud US New & Hot"="https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20SoundCloud%20US%20New%20%26%20Hot.csv",
                                       "Spotify Global Viral 50 Placement" = "https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Spotify%20Global%20Viral%2050%20Placement.csv",
                                       "Spotify Popularity Metric"="https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Spotify%20Popularity%20Metric.csv",
                                       "Spotify Total Streams" = "https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Spotify%20Total%20Streams.csv",
                                       "Spotify US Viral 50 Placement"="https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20Spotify%20US%20Viral%2050%20Placement.csv",
                                       "TikTok Video Counts" = "https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20TikTok%20Video%20Counts.csv",
                                       "YouTube Video View Count"="https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/DATA%20FOR%20MODELING%20-%20YouTube%20Video%20View%20Count.csv"
                                       
                           )),
            sliderInput("bins_prop",
                        "Proportion of NA observations:",
                        min = 0,
                        max = 1,
                        value = 0.4),
            
            selectizeInput("methods",
                           label = "Which methods you want to choose to replace missing value",
                           selected = NULL,
                           options = list(create = TRUE),
                           choices = c(
                              "Max","Min","Mean","Median","PMM"
                           )),
        
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            downloadButton("downloadData", "Download")
            ),
        
        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("table"),
            plotOutput("distPlot")
        )
    ))),
    tabPanel(title = "Time Series Model",
             fluidPage(
                 h1("Prediction based on time series model ", style = "font-family:Times"),
                 sidebarLayout(
                     sidebarPanel(
                         # Enter URL or choose from existing datasets
                         
                         sliderInput("days", "how many days forward you want to predict",
                                     min = 1, max = 15, value = 5
                         ),
                         textInput("songname", "Which song you want to predict? Please correctly spell the song name:"),
                         
                         h4("The song name can be Kraazy, Rat, Adderall (Corvette Corvette), R60LLXN N CONTROLLXN, 	YKWIM, etc.", style = "font-family:Times"),
                         actionButton("Submit" , label = "Submit", value="Krazzy")
                     ),
                     mainPanel(
                         plotOutput("timeseries")
                     )
                 )
                 
             )
    ),
    # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    datasetInput <- reactive({
        data <- read_csv(input$url,na="0")
        label_song=data$Song
        x=as.data.frame(t(data[,-c(1,2,3)]))
        colnames(x)=label_song
        rownames(x)=colnames(data)[-c(1,2,3)]
        x=x[, colSums(is.na(x)) < input$bins_prop*nrow(x)]
        colnames(x)=gsub(" ", "_", colnames(x))
        colnames(x)=gsub("-", "_", colnames(x))
        colnames(x)=gsub('\\(', "", colnames(x))
        colnames(x)=gsub("\\)", "", colnames(x))
        colnames(x)=gsub("!", "", colnames(x))
        colnames(x)=gsub("?", "", colnames(x))
        colnames(x)=gsub("$", "", colnames(x))
        colnames(x)=gsub("'", "", colnames(x))
        colnames(x)=gsub("<", "", colnames(x))
        colnames(x)=gsub(">", "", colnames(x))
        #colnames(x)=Encoding(colnames(x))
        if (input$methods=="PMM"){
            colnames(x)=c(1:length(colnames(x)))
            PMM_function(x)
        } else if(input$methods=="Min"){
            x[is.na(x)]=0
            Min_function(x)
        }else if(input$methods=="Max"){
            x[is.na(x)]=0
            Max_function(x)
        }else if(input$methods=="Mean"){
            x[is.na(x)]=0
            Mean_function(x)
        }else if(input$methods=="Median"){
            x[is.na(x)]=0
            Median_function(x)
        }
        
    })

    output$table <- renderDataTable({
        # generate bins based on input$bins from ui.R
        DT::datatable(datasetInput())

    })
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("temp", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        temp_data=datasetInput()
        temp_data$date=c(1:31)
        temp_d=melt(temp_data,id="date")
        colnames(temp_d) = c("Days","Song","value")
        ggplot(data = temp_d,aes(x=Days,y=value,group = Song,color=Song,shape=Song))+
            geom_point()+
            geom_line()+
            xlab("Days")+
            ylab("value")
            #theme_bw()+
            #theme(panel.grid.major=element_line(colour=NA),
                  #panel.background = element_rect(fill = "transparent",colour = NA),
                  #plot.background = element_rect(fill = "transparent",colour = NA),
                  #panel.grid.minor = element_blank(),
                  #)
        #bins <- seq(min(temp_data), max(temp_data), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        #hist(temp_data, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$timeseries <- renderPlot({
        
        temp=eventReactive(input$Submit, {
            read_csv("https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/New_GeniusPageView.csv")
        })
        predict_ts(temp(),input$songname,input$days,50,60)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
