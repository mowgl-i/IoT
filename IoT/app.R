library(readr)
library(shiny)
library(ggplot2)
library(tidyverse)
library(reactable)
#install.packages("googlesheets")
library(googlesheets4)
library(lubridate)
library(ggrepel)



getData <- function () {
    
    sheets<-googlesheets4::gs4_find('IFTTT')
    Data <<- c()
    
    for(id in sheets[,2]){
    links <- paste("https://docs.google.com/spreadsheets/d/",id,"/edit#gid=0",sep = "")
    for(link in links){
    data <- googlesheets4::read_sheet(link,col_names = c('Date','Variable','Value'))
    data<-data %>% mutate('Day' = substring(Date, 1,14))
    data<-data %>% mutate('Time' = substring(Date, 19,25))
    
    data <- data %>% mutate('Day'= parse_date_time(Day,orders = '%b%d%Y'))
    data<- data %>% mutate('Time' = parse_date_time2(Time, orders = '%I:%M%p',tz = 'America/New_York'))
    data <- data %>% mutate('Time' = substring(Time, 12,19))
    data<-data %>% mutate('Daytime' = paste(Day,Time, sep = ' '))
    data<-data %>% mutate('Daytime' = ymd_hms(Daytime))
    data <- data %>% mutate('Outcome' = case_when(
        Value >= 870 ~ "At Desk",
        Value <= 869 ~ "Not at Desk"
    ))
    Data <<- rbind(Data,data)
    }}
    
}

getData()
sleep_data <- read_csv("C:/Users/macia/Documents/MSIA-21/IoT/Amazon Health Data/Sleep/Sleep_Sessions_ddf09bef-af8c-4a5b-bbd9-6bed8faff4b3.csv")
sleep_dates <- lubridate::date(unique(sleep_data$`Date Of Sleep`))
dates<-lubridate::date(unique(Data$Day))
skin_temp <- read_csv("C:/Users/macia/Documents/MSIA-21/IoT/Amazon Health Data/Sleep/Skin_Temperature_13f1f3d5-f722-4ea5-89ea-78929a307892.csv")

# Define UI for application that draws a histogram
# ui <- fluidPage(
#     
#     # Application title
#     titlePanel("Mowgli at his desk"),
#     
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             radioButtons('Day_select','Which Day would you like to view?',dates),
#             
# 
#         ),
#         
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput('Time_plot')
#         )
#     )
# )


ui <- fluidPage(
    navlistPanel(
        id = "tabset",
        "IoT Reactive: Measure of Screen Time",
        tabPanel("Time at desk",
                 mainPanel(
                 radioButtons('Day_select','Which Day would you like to view?',dates),
                 plotOutput('Time_plot')
                 )
                 ),
        "IoT Reactive: Sleep Quality",
         tabPanel("Time Asleep",mainPanel(
            radioButtons('Sleep_select','Which Day would you like to view?',sleep_dates),
            plotOutput('Sleep_Breakdown')
             
             
             
         )),
        
        tabPanel("Overall Skin Temperature",
                 mainPanel(
                     plotOutput('Skin_temp')
                 ))
    )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filter_data <- reactive({Data %>% mutate(Day = as.factor(Day)) %>% filter(Day == input$Day_select)})
    
    filter_sleep <- reactive({
        sleep_data %>% filter(`Date Of Sleep` == input$Sleep_select) %>% select(`Rem Sleep Percentage`,`Light Sleep Percentage`,`Deep Sleep Precentage`) %>% 
            gather(key = 'Sleep_Type',value = 'Value')
    })
    
    
    output$Time_plot <- renderPlot({
        ggplot(filter_data(),aes(x = as_datetime(Daytime), y = as.numeric(Value), color = Outcome))+
            geom_point()+
            labs(title = 'Light at Desk')+
            xlab('Light Res')})
    
    
    output$Sleep_Breakdown <- renderPlot({
        
        ggplot(filter_sleep(),aes(x = Sleep_Type,y=Value,fill=Sleep_Type,label = paste(round(Value),'%')))+
            geom_bar(stat='identity',show.legend = F)+
            coord_polar()+
            theme_minimal()+
            geom_label_repel()+
            xlab(NULL)+
            ylab(NULL)+
            theme(axis.text.y = element_blank(),
                  axis.text.x = element_blank(),
                  legend.title = element_blank(),
                  title = element_text(face ="bold"))+ 
            labs(title= 'Sleep Breakdown')})
    
    output$Skin_temp <- renderPlot({
        ggplot(skin_temp,aes(x = `time(utc)`,y = `Skin Temperature (Celsius)`))+geom_line()+geom_smooth()+labs(title = 'Skin temp through the day')+theme_minimal()
        
    })
    
    
}




# Run the application
shinyApp(ui = ui, server = server)




#sleep_data %>% filter(`Date Of Sleep` == sleep_dates[4])


