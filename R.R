install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinydashboardPlus")
install.packages("tidyr")
install.packages("ggplot2")


library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)


dataset <- read.csv("C:\\Users\\Juliana\\Downloads\\DailyDelhiClimateTrain.csv")
head(dataset)

ui <- dashboardPage(
  dashboardHeader(title = icon("cloud", " CLIMATEMPO")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("MEDIAS", tabName = "MEDIAS"),
      menuItem("GRAFICO EM LINHA", tabName = "GRAFICOEMLINHA"), #É importante que o tabName não tenha espaços
      menuItem("HISTOGRAMA", tabName = "HISTOGRAMA"),
      menuItem("BOXPLOT", tabName = "BOXPLOT")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "MEDIAS",
              fluidRow(
                box(width = 7 ,title = "Medias",
                    tableOutput('medias')
                ),
                
                box(width = 5 ,title = "Seletor de Dados",
                    dateRangeInput("date_range_meantable", "Escolha o intervalo de tempo:", start = min(dataset$date), end = max(dataset$date)),
                    selectInput("column_select_meantable", "Selecione um parâmetro:", choices = c("Mean Temperature" = "meantemp", "Humidity" = "humidity", "Wind Speed" = "wind_speed", "Mean Pressure" = "meanpressure"))
                )
              )
      ),
      tabItem(tabName = "GRAFICOEMLINHA",
              fluidRow(
                box(width = 4 ,title = "Seletor de Dados",
                    dateRangeInput("date_range_linechart", "Escolha o intervalo de tempo:", start = min(dataset$date), end = max(dataset$date)),
                    selectInput("column_select_linechart", "Selecione um parâmetro:", choices = c("Mean Temperature" = "meantemp", "Humidity" = "humidity", "Wind Speed" = "wind_speed", "Mean Pressure" = "meanpressure"))
                ),
                box(width = 8 ,title = "Gráfico em Linha",
                    plotOutput("linechart")
                )
              )
      ),
      tabItem(tabName = "HISTOGRAMA",
              fluidRow(
                
                box(width = 4 ,title = "Seletor de Dados",
                    dateRangeInput("date_range_histogram", "Escolha o intervalo de tempo:", start = min(dataset$date), end = max(dataset$date)),
                    selectInput("column_select_histogram", "Selecione um parâmetro:", choices = c("Mean Temperature" = "meantemp", "Humidity" = "humidity", "Wind Speed" = "wind_speed", "Mean Pressure" = "meanpressure"))),
                box(width = 8, title = "Histograma", plotOutput("histogram")),
      
              )
      ),
      tabItem(tabName = "BOXPLOT",
              fluidRow(
                box(width = 8, title = "Boxplot",(plotOutput("boxplot", height = 500))),
                
                box(width = 4 ,title = "Seletor de Dados",
                    dateRangeInput("date_range_boxplot", "Escolha o intervalo de tempo:", start = min(dataset$date), end = max(dataset$date)),
                    selectInput("column_select_boxplot", "Selecione um parâmetro:", choices = c("Mean Temperature" = "meantemp", "Humidity" = "humidity", "Wind Speed" = "wind_speed", "Mean Pressure" = "meanpressure"))
                )
              )
      )
    )
  )
)

find_mode <- function(x) {
  # Remove empty values (NA)
  x <- x[!is.na(x)]
  
  # Use table() to get the frequency of each value
  freq <- table(x)
  
  # Find the maximum frequency
  max_freq <- max(freq)
  
  # Return the values with the maximum frequency (mode)
  return(head(names(freq)[freq == max_freq], 1))
}

server <- function(input, output) {
  
  #Tabela de Medias
  output$medias <- renderTable({
    if (input$column_select_meantable == "meantemp") {
      filtered_data = dataset$meantemp[(dataset$date >= input$date_range_meantable[1]) & 
                                         (dataset$date <= input$date_range_meantable[2])]
    } else if (input$column_select_meantable == "humidity") {
      filtered_data = dataset$humidity[(dataset$date >= input$date_range_meantable[1]) & 
                                         (dataset$date <= input$date_range_meantable[2])]
    } else if (input$column_select_meantable == "wind_speed") {
      filtered_data = dataset$wind_speed[(dataset$date >= input$date_range_meantable[1]) & 
                                           (dataset$date <= input$date_range_meantable[2])]
    } else {
      filtered_data = dataset$meanpressure[(dataset$date >= input$date_range_meantable[1]) & 
                                             (dataset$date <= input$date_range_meantable[2])]
    }
    
    if (input$date_range_meantable[2] >= input$date_range_meantable[1]) {
      data.frame(
        Metrica = c("Media", "Mediana", "Moda", "Desvio Padrão"),
        Valor = c(mean(filtered_data), median(filtered_data), find_mode(filtered_data), sd(filtered_data))
      )
    } else {
      data.frame(
        Metrica = c("Media", "Mediana", "Moda", "Desvio Padrão"),
        Valor = c("ERRO", "ERRO", "ERRO", "ERRO")
      )
    }
  })
  
  output$linechart <- renderPlot({
    if(is.null(input$date_range_linechart)) return()
    
    if (input$column_select_linechart == "meantemp") {
      selectedColumnlinechart <- "Mean Temperature"
    } else if (input$column_select_linechart == "humidity") {
      selectedColumnlinechart <- "Humidity"
    } else if (input$column_select_linechart == "wind_speed") {
      selectedColumnlinechart <- "Wind Speed"
    } else if (input$column_select_linechart == "meanpressure") {
      selectedColumnlinechart <- "Mean Pressure"
    }
    
    filtered_data <- dataset[dataset$date >= input$date_range_linechart[1] & dataset$date <= input$date_range_linechart[2], ]
    
    selected_variables <- input$column_select_linechart
    
    data_long <- pivot_longer(filtered_data, 
                              cols = selected_variables, # Use selected variables
                              names_to = "Variable", 
                              values_to = "Value")
    
    
    ggplot(data_long, aes(x = as.Date(date), y = Value, color = Variable)) +
      geom_line() +
      labs(x = "Date", y = selectedColumnlinechart) +
      theme_minimal() +
      scale_color_manual(values = c(meantemp = "blue", humidity = "red", wind_speed = "green", meanpressure = "purple")) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  })
  
  
  
  #Plot do Histograma
  output$histogram <- renderPlot({
    if(is.null(input$date_range_histogram)) return()
    
    if (input$column_select_histogram == "meantemp") {
      selectedColumnHistogram <- "Mean Temperature"
    } else if (input$column_select_histogram == "humidity") {
      selectedColumnHistogram <- "Humidity"
    } else if (input$column_select_histogram == "wind_speed") {
      selectedColumnHistogram <- "Wind Speed"
    } else if (input$column_select_histogram == "meanpressure") {
      selectedColumnHistogram <- "Mean Pressure"
    }
    
    filtered_data <- dataset[dataset$date >= input$date_range_histogram[1] & dataset$date <= input$date_range_histogram[2], ]
    hist(filtered_data[[input$column_select_histogram]], main = paste(selectedColumnHistogram), xlab = input$column_select_histogram)
  })
  
  #Plot do Boxplot
  output$boxplot <- renderPlot({
    if(is.null(input$date_range_boxplot)) return()
    
    if (input$column_select_boxplot == "meantemp") {
      selectedColumnBoxplot <- "Mean Temperature"
    } else if (input$column_select_boxplot == "humidity") {
      selectedColumnBoxplot <- "Humidity"
    } else if (input$column_select_boxplot == "wind_speed") {
      selectedColumnBoxplot <- "Wind Speed"
    } else if (input$column_select_boxplot == "meanpressure") {
      selectedColumnBoxplot <- "Mean Pressure"
    }
    
    filtered_data <- dataset[dataset$date >= input$date_range_boxplot[1] & dataset$date <= input$date_range_boxplot[2], ]
    boxplot(filtered_data[[input$column_select_boxplot]], main = paste(selectedColumnBoxplot), ylab = input$column_select_boxplot, col = "pink")
  })
}

shinyApp(ui, server)