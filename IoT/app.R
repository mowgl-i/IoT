library(readr)
library(shiny)
library(ggplot2)
library(tidyverse)
library(reactable)
#install.packages("googlesheets")
library(googlesheets4)
library(lubridate)



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


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Mowgli at his desk"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons('Day_select','Which Day would you like to view?',c('2021-03-29',
                                                                     '2021-03-28',
                                                                     '2021-03-27',
                                                                     '2021-03-26',
                                                                     '2021-03-25',
                                                                     '2021-03-24',
                                                                     '2021-03-23')),
            

        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput('Time_plot')
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filter_data <- reactive({Data %>% mutate(Day = as.factor(Day)) %>% filter(Day == input$Day_select)})
    
    output$Time_plot <- renderPlot({
        ggplot(filter_data(),aes(x = as_datetime(Daytime), y = as.numeric(Value), color = Outcome))+
            geom_point()+
            labs(title = 'Light at Desk')+
            xlab('Light Res')})
    
    
}

# Run the application
shinyApp(ui = ui, server = server)


