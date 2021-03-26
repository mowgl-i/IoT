library(readr)
library(shiny)
library(ggplot2)
library(tidyverse)
library(reactable)
#install.packages("googlesheets")
library(googlesheets4)
library(lubridate)




getRaw <- function () {
    data <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1fXlE794sSQntw8fd1bPKKja5l25ujphwb0QjAJaFazQ/edit#gid=0')
    
    data<-data %>% mutate('Day' = substring(Date, 1,14))
    data<-data %>% mutate('Time' = substring(Date, 19,25))
    
    data <- data %>% mutate('Day'= parse_date_time(Day,orders = '%b%d%Y'))
    data<- data %>% mutate('Time' = parse_date_time2(Time, orders = '%I:%M%p',tz = 'America/New_York'))
    data <- data %>% mutate('Time' = substring(Time, 12,19))
    data<-data %>% mutate('Daytime' = paste(Day,Time, sep = ' '))
    
    data<-data %>% mutate('Daytime' = ymd_hms(Daytime))
    
 
    }

data <- getRaw()


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Mowgli at his desk"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            # plotOutput("distPlot_titanic"),
            # radioButtons('class','Which passenger class would you like to view?',c('1','2','3')),
            # plotOutput('Plot_by_class')
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput('Time_plot')
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Time_plot <- renderPlot({
        ggplot(data,aes(x = Daytime, y = Value))+
            geom_point()+
            labs(title = 'Light at Desk')+
            xlab('Light Res')})
    
    
}

# Run the application
shinyApp(ui = ui, server = server)



