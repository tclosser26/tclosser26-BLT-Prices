library(tidyverse)
library(readxl)
library(shiny)
library(plotly)

# ------------------Preamble---------------- #
fooddata <- read_excel("fooddata.xlsx", 
                       col_types = c("text", "numeric", "text", 
                                     "numeric", "text", "text"))
BLT <- fooddata |> 
  filter(item_name %in% c("Bacon, sliced, per lb. (453.6 gm)", "Lettuce, iceberg, per lb. (453.6 gm)", "Tomatoes, field grown, per lb. (453.6 gm)","Bread, white, pan, per lb. (453.6 gm)"))

BLT <- BLT |>
  group_by(item_name, year) |>
  summarise(yearly_avg = mean(value, na.rm = TRUE), .groups = "drop")

#-------------------------------------------#

bacon_data <- filter(BLT, item_name == "Bacon, sliced, per lb. (453.6 gm)")

lettuce_data <- filter(BLT, item_name == "Lettuce, iceberg, per lb. (453.6 gm)")

tomato_data <- filter(BLT, item_name == "Tomatoes, field grown, per lb. (453.6 gm)")

bread_data <- filter(BLT, item_name == "Bread, white, pan, per lb. (453.6 gm)")

# ------------------Preamble---------------- #
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  
  # Application title
  titlePanel("Predicted prices of BLT sandwhich ingredients over the course of 100 years"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h4("The Bacon, Lettuce, Tomato Sandwich often referred to as a BLT is a timeless 
         American classic, providing comfort to millions over the centuries. However, 
         in a world of high inflation the cost of this beloved icon is vulnerable to 
         price shifts as time goes on. To explore these potential shifts the slider 
         below can be used to predict the price of the BLT's ingredients at a specified 
         year in the future. While the buttons can be used to quantify these prices in 
         terms of three items of constant value in the U.S, Costco Hot Dogs, Arizona Iced 
         Tea, and quarters. "),
      sliderInput("sliderYEAR", label = h3("Predicted Year"), min = 2025, max = 2125,
                  value = 2025),
      radioButtons("radio_1", label = h3("Conversion to:"), choices = list(
        "Costco Footlong Hotdog" = "Costo Footlong Hotdog", 
        "Arizona Iced Tea" = "Arizona Iced Tea", 
        "USD Quarter" = "USD Quarter")),
      checkboxGroupInput("checkGroup", label = h4("Displayed Ingredients"), 
                         choices = list("Tomato" = "Tomato", 
                                        "Bacon" = "Bacon", 
                                        "Lettuce" = "Lettuce",
                                        "Bread" = "Bread"),
                         selected = c("Tomato", "Bacon", "Lettuce", "Bread"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("predplot"),
      h4("Below is the price per pound of each respected BLT ingredient:"),
      textOutput("graphtext1"),
      h4("Important conversions:"),
      textOutput("graphtext2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  regREACTIVE <- reactive({
    
    year <- seq(1995,input$sliderYEAR,1)
    
    bread <- -58.32860 + 0.02965*year
    bacon <- -330.9420 + 0.1669*year
    tomato <- -50.4107 + 0.0259*year
    lettuce <- -34.96515 + 0.01786*year
    
    bacon <- data.frame(year,bacon)
    lettuce <- data.frame(year,lettuce)
    tomato <- data.frame(year,tomato)
    bread <- data.frame(year,bread)
    
    return(list(bacon = bacon, lettuce = lettuce, tomato = tomato, bread = bread))
  })
  
  output$predplot <- renderPlotly({
    test <- regREACTIVE()
    
    selected <- input$checkGroup
    
    alphaTOMATO <- ifelse("Tomato" %in% selected, 1, 0)
    alphaBACON <- ifelse("Bacon" %in% selected, 1, 0)
    alphaLETTUCE <- ifelse("Lettuce" %in% selected, 1, 0)
    alphaBREAD <- ifelse("Bread" %in% selected, 1, 0)
    
    p <- ggplot() +
      geom_line(data = test$tomato, aes(x = year, y = tomato, color = "Tomato"), 
                linewidth = 1, alpha = alphaTOMATO) +
      geom_line(data = test$bacon, aes(x = year, y = bacon, color = "Bacon"), 
                linewidth = 1, alpha = alphaBACON) +
      geom_line(data = test$lettuce, aes(x = year, y = lettuce, color = "Lettuce"), 
                linewidth = 1, alpha = alphaLETTUCE) +
      geom_line(data = test$bread, aes(x = year, y = bread, color = "Bread"), 
                linewidth = 1, alpha = alphaBREAD) +
      labs(title = "Predicted Price per Pound of BLT Ingredients", 
           x = "Year", 
           y = "Price per Pound in USD",
           color = "Ingredients") + 
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_manual(
        values = c("Tomato" = "red", "Bacon" = "brown", "Lettuce" = "darkgreen", "Bread" = "gold")
      )
    
    ggplotly(p, tooltip = c("x","y"))
    
  })
  
  output$graphtext1 <- renderText({
    
    test_2 <- regREACTIVE()
    bread2 <- -58.32860 + 0.02965*input$sliderYEAR
    bacon2 <- -330.9420 + 0.1669*input$sliderYEAR
    tomato2 <- -50.4107 + 0.0259*input$sliderYEAR
    lettuce2 <- -34.96515 + 0.01786*input$sliderYEAR
    
    paste("In the year ", sep = "", input$sliderYEAR, 
          ", the price of bread will be $", round(bread2, 2), " per pound, ",
          "the price of bacon will be $", round(bacon2, 2), " per pound, ",
          "the price of tomatoes will be $", round(tomato2, 2), " per pound, ",
          "and the price of lettuce will be $", round(lettuce2, 2), " per pound.")
    
  })
  
  output$graphtext2 <- renderText({
    
    test_2 <- regREACTIVE()
    bread2 <- -58.32860 + 0.02965*input$sliderYEAR
    bacon2 <- -330.9420 + 0.1669*input$sliderYEAR
    tomato2 <- -50.4107 + 0.0259*input$sliderYEAR
    lettuce2 <- -34.96515 + 0.01786*input$sliderYEAR
    
    if(input$radio_1 == "Costo Footlong Hotdog") {
      conversion_factor <- 1.50
      converted_bread <- bread2 / conversion_factor
      converted_bacon <- bacon2 / conversion_factor
      converted_tomato <- tomato2 / conversion_factor
      converted_lettuce <- lettuce2 / conversion_factor
    } 
    else if(input$radio_1 == "Arizona Iced Tea") {
      conversion_factor <- 0.99  # Example value
      converted_bread <- bread2 / conversion_factor
      converted_bacon <- bacon2 / conversion_factor
      converted_tomato <- tomato2 / conversion_factor
      converted_lettuce <- lettuce2 / conversion_factor
    }
    else if(input$radio_1 == "USD Quarter") {
      conversion_factor <- 0.25  # Example value
      converted_bread <- bread2 / conversion_factor
      converted_bacon <- bacon2 / conversion_factor
      converted_tomato <- tomato2 / conversion_factor
      converted_lettuce <- lettuce2 / conversion_factor
    }
    
    paste("If we convert our price per pound of each BLT ingredient in USD to ", input$radio_1, "'s", sep = "", 
          ", then this will be the set value of each of our BLT ingredients: ", 
          "Bread will be valued at ", round(converted_bread, 2), " ", input$radio_1, "'s,",
          " bacon will be valued at ", round(converted_bacon, 2), " ", input$radio_1, "'s,", 
          " tomatoes will be valued at ", round(converted_tomato, 2), " ", input$radio_1, "'s,",
          " and lettuce will be valued at ", round(converted_lettuce, 2), " ", input$radio_1, "'s,")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
