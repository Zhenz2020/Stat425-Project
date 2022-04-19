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
library(forecast)
library(splines)
library(tidyverse)
library(dbplyr)
library(xgboost)
library(shinydashboard)



cluster_plot <- function(ts,myK){
    set.seed(123)
    length.out = ncol(ts)
    x = seq(0, 1, length.out )
    F=ns(1:length.out,df = 9, intercept = FALSE)
    F=t(t(F) - colMeans(F))
    B=solve(t(F)%*%F)%*%t(F)%*%t(ts)
    B=t(B)
    mykm1=kmeans(B,myK)
    par(mfrow=c(ceiling(myK/3),3))
    ts_temp=ts
    ts_temp=cbind(ts_temp,as.matrix(rep(0,dim(ts_temp)[1]),dim(ts_temp)[1],1))
    mycenters1=matrix(rep(0,myK*ncol(ts)),ncol=ncol(ts))
    for(k in 1:myK){
        id=which(mykm1$cluster==k)
        plot(NA, xlim = c(1, ncol(ts)), ylim = range(ts), xlab = "days", ylab = "values")
        B_i= B[id,]
        temp_mycenters1= colMeans(B_i)
        mycenters1[k,]=F %*% t(t(temp_mycenters1))
        for(i in 1:length(id)){
            lines(1:ncol(ts), ts[id[i],] , col="gray")
        }
        lines(1:ncol(ts), mycenters1[k,], col="red")
    }
}

cluster_tstemp <- function(ts,myK){
    set.seed(123)
    length.out = ncol(ts)
    x = seq(0, 1, length.out )
    F=ns(1:length.out,df = 9, intercept = FALSE)
    F=t(t(F) - colMeans(F))
    B=solve(t(F)%*%F)%*%t(F)%*%t(ts)
    B=t(B)
    mykm1=kmeans(B,myK)
    par(mfrow=c(ceiling(myK/3),3))
    ts_temp=ts
    ts_temp=cbind(ts_temp,as.matrix(rep(0,dim(ts_temp)[1]),dim(ts_temp)[1],1))
    mycenters1=matrix(rep(0,myK*ncol(ts)),ncol=ncol(ts))
    for(k in 1:myK){
        id=which(mykm1$cluster==k)
        B_i= B[id,]
        temp_mycenters1= colMeans(B_i)
        mycenters1[k,]=F %*% t(t(temp_mycenters1))
        ts_temp[id,length.out+1]=k
    }
    return(ts_temp)
}





xgboost_pre <- function(new_data,ts_temp,days){
    set.seed(123)
    train_x=as.matrix(ts_temp[,1:days])
    train_y=as.matrix(ts_temp[,32])
    new_data=matrix(new_data[1:days,],nrow = 1)
    xgb.model <- xgboost(data = train_x, 
                         label = train_y, max_depth = 8,
                         eta = 0.04, nrounds = 1000,
                         subsample = 0.5,
                         verbose = FALSE)
    xgb.pred = predict(xgb.model, newdata = as.matrix(new_data))
    xgb_pred=round(xgb.pred)
    if (xgb_pred==1){
        remark="This data set belongs to the first group. During this month, it will grow slowly and tend to converge in the later stages"
    }else if(xgb_pred==2){
        remark="This data set belongs to the second group. During this month, the number of listens to this song will rise evenly"
    }
    else if(xgb_pred==3){
        remark="This data set belongs to the third group. In the later stages, listening drops"
    }
    else if(xgb_pred==4){
        remark="This data set belongs to the fourth group. In the later stages, listening is unstable, but generally increases at a higher rate"
    }
    else{
        remark="This data set belongs to the fifth group. It is expected to peak at around 25 days, but then decline"
    }
    return(remark)
}
#replace temp with name of dataset
#days: how many days forward you want to predict
#conflow,confhigh: confidence interval for prediction
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
                         
                         sliderInput("days", "How many days forward you want to predict",
                                     min = 1, max = 15, value = 5
                         ),
                         textInput("songname", "Which song you want to predict? Please correctly spell the song name:"),
                         
                         h5("The song name can be Kraazy, Rat, Adderall (Corvette Corvette), R60LLXN N CONTROLLXN, 	YKWIM, etc.", style = "font-family:Times"),
                         actionButton("Submit" , label = "Submit", value="Krazzy")
                     ),
              
                     mainPanel(
                         plotOutput("timeseries")
                     )
                 )
                 
             )
    ),
    tabPanel(title = "Cluster and XGBoost",
             fluidPage(
                 h1("NCS-based clustering and XGBoost-based prediction", style = "font-family:Times"),
                 sidebarLayout(
                     sidebarPanel(
                         tags$head(tags$style("#result_xgboost{color: grey;
                         font-size: 30px;
                         font-style: italic;
                         }"
                         )
                         ),
                         
                         textInput("url_xgboost", "Please input the url for the song you want to predict:",value = "https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/test_data.csv"),
                         
                         selectizeInput("num_cluster",
                                        label = "How many clusters you want to set",
                                        selected = 5,
                                        choices = c(
                                            1,2,3,4,5,6,7,8
                                        )),
                         h5("For this data set, the best number of clusters is 5", style = "font-family:Times"),
                         
                         sliderInput("days_xgboost", "How many days you want to keep",
                                     min = 1, max = 30, value = 15
                         ),
                         
                         
                         actionButton("Submit_cluster" , label = "Submit", value="Krazzy")
                     ),
                     mainPanel(
                         plotOutput("cluster"),
                         plotOutput("xgboost"),
                         textOutput("result_xgboost")
                         
                         
                         
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
    
    
    glob <- reactive({
        temp=read.csv("https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/train_data.csv")
        cluster_tstemp(temp,as.integer(input$num_cluster))
    })
    
    output$cluster <- renderPlot({
        
        temp=read.csv("https://raw.githubusercontent.com/Zhenz2020/Stat427-Project/main/train_data.csv")
        cluster_plot(temp,as.integer(input$num_cluster))
    })
    
    
    output$xgboost <- renderPlot({
        
        temp1=eventReactive(input$Submit_cluster, {
            read.csv(input$url_xgboost)
        })
        #ts_temp_t=glob()
        #text_xgb=xgboost_pre(as.matrix(temp1()),ts_temp_t,as.integer(input$days_xgboost))
        plot(as.matrix(temp1(),nrow = 1)[1:as.integer(input$days_xgboost)],col=1,type = "l",lty=1,main="Scatter plot of the input data set",ylab="Values",xlim=c(1,31),ylim=c(-3,3))
        abline(v=as.integer(input$days_xgboost),col="red",lty=2,lwd=3)
        lines(x=c(as.integer(input$days_xgboost):31),y=as.matrix(temp1(),nrow = 1)[(as.integer(input$days_xgboost)):31],col=2,lty=1)
        legend("topleft", legend=c("Observed Data","Unobserved real variables"),col=c(1,2),lty=c(1,1), cex=0.8)

    })
    
    output$result_xgboost <- renderText({ 
        temp2=eventReactive(input$Submit_cluster, {
            read.csv(input$url_xgboost)
        })
        ts_temp_t=glob()
        xgboost_pre(temp2(),ts_temp_t,as.integer(input$days_xgboost))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
