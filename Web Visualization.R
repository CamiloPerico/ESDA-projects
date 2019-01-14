library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(tidyverse)


time <- read.csv("timechange.csv")

df <- read.csv("generation_price_unit_2017.csv")
df <- select(df, -c(1))
df <- select(df, SettlementDate, SettlementPeriod, everything())
df <- filter(df, SettlementPeriod <=48)
names(df)[4] = "Code"
df <- subset(df, select = c(1,2,3,4,6,7))

df_spread <- spread(df, key = "Code", value = "EnergySupply")
colSums(is.na(df_spread))
df_spread[is.na(df_spread)] <- 0


time$Hour <- as.factor(time$Hour)
names(time)[1] <- "SettlementPeriod"

df_spread$SettlementPeriod <- as.factor(df_spread$SettlementPeriod)
df_spread <- left_join(df_spread,time, by="SettlementPeriod")
df_spread$Date <- paste0(df_spread$SettlementDate," ",df_spread$Hour2)
df_spread <- select(df_spread, SettlementDate, SettlementPeriod, Date, everything())
df_spread$Date <- ymd_hm(df_spread$Date)
df_spread$Date <-  as.POSIXct(df_spread$Date, format = "%Y-%m-%d %H:%M:%S")

one_plant <- df_spread[,1:6] 

g <- ggplot(one_plant, aes(x = Date, y = `48W0000000ABTH7Y`)) +
  geom_line(alpha = 0.4) 

ggplotly(g)

df_spread2 <- select(df_spread,c(3,6:244))


df_daily <- df %>%
  mutate(Date = format(SettlementDate), Code = as.factor(Code)) %>%
  group_by(Date, Code) %>%
  summarise(Generation = sum(EnergySupply))
  
df_daily <- data.frame(df_daily)
df_daily_spread <- spread(df_daily, key = "Code", value = "Generation")
df_daily_spread[is.na(df_daily_spread)] <- 0


# WEB1 ----
widgets_ui  <- fluidPage(
  sidebarLayout(
    sidebarPanel(dateRangeInput("date_range", label=h3("Date Range"),start="2017-01-01", end="2017-12-31")
    ),
    mainPanel(
      plotOutput("qPlot")
    )
  )
)
# Define server logic ----
widgets_server <- function(input, output) {

  output$qPlot <- renderPlot({ 

      p <-ggplot(one_plant, aes(x = as.Date(Date), y = `48W0000000ABTH7Y`)) +
      geom_line(alpha = 0.4) +xlim(as.Date(input$date_range[1]),as.Date(input$date_range[2]))
      p
  }) 
  
}

shinyApp(ui = widgets_ui, server = widgets_server)

# WEB2 --------------------------------------------------------------------
ui2 <- fluidPage(
  
  title = "UK Generation Units",
  
  plotlyOutput('plot'),
  
  hr(),
  
  fluidRow(
           h4("Diamonds Explorer"),
           sidebarPanel(dateRangeInput("date_range", label=h3("Date Range"),start="2017-01-01", end="2017-12-31"))
  )
)

server <- function(input, output) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    p <- ggplot(one_plant, aes(x = as.Date(Date), y = `48W0000000ABTH7Y`)) +
      geom_line(alpha = 0.4)+
      xlim(input$date_range[1],input$date_range[2]) +
      labs(y = "Energy (MWh)",
          x = "Date")
    ggplotly(p)
    
  })
}

shinyApp(ui2, server)


?fluidRow
# WEB3 --------------------------------------------------------------------
ui3 <- fluidPage(
  
  title = "UK Generation Units",
  
  plotOutput('plot'),
  
  hr(),
  
  fluidRow(column(12,align = "center",
    mainPanel(dateRangeInput("date_range", label=h3("Date Range"),start="2017-01-01", end="2017-12-31"))
  )
  ),
  fluidRow(column(6,
    selectInput(inputId = "plant", label = "Choose a Plant", choices = names(df_daily_spread) [c(2:10)]),
    column(6,
           selectInput(inputId = "plant2", label = "Choose a Plant2", choices = names(df_daily_spread) [c(2:10)]))
  )
  )
  
)

server3 <- function(input, output) {
  dfInput <- reactive({
    ##subsetting is a bit tricky here to id the column on which to subset        
    select(df_daily_spread,c(1,input$plant,input$plant))
  })
  
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlot({
    df1 <- dfInput()
    p <- ggplot(df1) +
      geom_line(aes(x = as.Date(Date), y = df1[,2]), colour = "Blue")+
      xlim(input$date_range[1],input$date_range[2]) +
      labs(y = "Energy (MWh)",
           x = "Date")
    p
    
  })
}

shinyApp(ui3, server3)



# WEB 4 --------------------------------------------------------------------
ui4 <- fluidPage(
  
  titlePanel("UK Generation Units"),
  plotlyOutput('plot'),
  hr(),
  
  fluidRow(column(12,align = "center",
                  sidebarPanel(dateRangeInput("date_range", label=h2("Date Range"),start="2017-01-01", end="2017-12-31"))
  )
  ),
  fluidPage(
  fluidRow(column(12,
                  selectInput(inputId = "plant", label = "Select Plant 1", choices = names(df_daily_spread) [c(2:236)]),
            column(12,
                  selectInput(inputId = "plant2", label = "Select Plant 2", choices = names(df_daily_spread) [c(2:236)]))
  )
  )
  )
)
?sidebarPanel
server4 <- function(input, output) {
  dfInput <- reactive({
    ##subsetting is a bit tricky here to id the column on which to subset        
    select(df_daily_spread,c(1,input$plant,input$plant))
  })
  daInput <- reactive({
    ##subsetting is a bit tricky here to id the column on which to subset        
    select(df_daily_spread,c(1,input$plant2))
  })
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    df2 <- daInput()
    df1 <- dfInput()
    p <- ggplot() +
      geom_line(data= df1, aes(x = as.Date(Date), y = df1[,2], colour = "Plant 1"))+
      geom_line(data= df2, aes(x = as.Date(Date), y = df2[,2], colour = "Plant 2"))+
      xlim(input$date_range[1],input$date_range[2]) +
      labs(y = "Energy (MWh)",
           x = "Date")
    ggplotly(p)
    
  })
}
shinyApp(ui4, server4)

p <- ggplot(one_plant, aes(x = Date, y = `48W0000000ABTH7Y`)) +
  geom_line(alpha = 0.4) +xlim(input$date_range[1],input$date_range[2])

?dateRangeInput

?lubridate
str(df_daily_spread)
str(info_plants)
