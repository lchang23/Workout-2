# Workout 02: Shiny App
# The purpose of this assignment is to create a shiny app that allows us to visualize in an
# interactive way the saving-investing modalities.


library(shiny)

# Define UI for application that draws a graph and creates a table
ui <- fluidPage(
  
  # Application title
  titlePanel("Saving-investing modalities"),
  
  # Panel with sliders 
  fluidRow(
    column(4,
           sliderInput("initial",
                       "Initial Amount",
                       min = 1,
                       max = 100000,
                       value = 1000,
                       step = 500,
                       pre = "$", sep = ","),
           
           sliderInput("ann",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500,
                       pre = "$", sep = ","
           )
    ),
    
    column(4,
           sliderInput("return",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5),
           
           sliderInput("growth",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2
                       
           )
    ),
    
    column(4,
           sliderInput("year",
                       "Years",
                       min = 0,
                       max = 50,
                       value = 10),
           
           selectInput("facet", "Facet?",
                       choices = c("No", "Yes"))
    ),
    
    # Graph & Table
    
      h3("Timelines"),
      plotOutput("Plot"),
      h3("Balances"),
      verbatimTextOutput("balances")
      
    
  )
)

library(ggplot2)
library(reshape2)

server <- function(input, output) {
  
  
  output$Plot <- renderPlot({
    
    init <- input$initial # Initial amount
    con <- input$ann # Annual contribution
    r <- (input$return) / 100 # Return rate
    g <- (input$growth) / 100 # Growth rate 
    years <- input$year # Year
    
    # Regular future value
    
    fv1 <- c()
    for (i in 0:years) {
      fv1[i+1] <- init * (1 + r)^i
      
    }
    
    
    # Future value with annuity
    
    fv2 <- c()
    for (i in 0:years) {
      fv2[i+1] <- init * (1 + r)^i + con * ((1 + r)^i - 1) / r
    }
    
    
    # Future value with growing annuity
    
    fv3 <- c()
    for (i in 0:years) {
      fv3[i+1] <- init * (1 + r)^i + con * ((1 + r)^i - (1 + g)^i)/(r - g)
    }

    # Creating data frame
    
    modalities <- data.frame(year = 0:years, no_contrib = fv1, fixed_contrib = fv2, growing_contrib = fv3) 

    
    # Plot graph
    
    if (input$facet == "No") {
      ggplot(modalities, aes(x = year, colour = variable)) +
        geom_line(aes(y = no_contrib, colour = "no_contrib")) +
        geom_point(aes(y = no_contrib, colour = "no_contrib")) +
        geom_line(aes(y = fixed_contrib, colour = "fixed_contrib")) +
        geom_point(aes(y = fixed_contrib, colour = "fixed_contrib")) +    
        geom_line(aes(y = growing_contrib, colour = "growing_contrib")) +
        geom_point(aes(y = growing_contrib, colour = "growing_contrib")) + 
        ggtitle("Three modes of investing") + 
        scale_x_continuous(breaks = seq(0,years,1)) +
        xlab("Year") +
        ylab("Value") +
        theme_bw()
    
    } else {
      modalities2 <- melt(modalities, id = "year")
      colnames(modalities2)[2] <- 'variable'
      colnames(modalities2)[3] <- 'value'
      
      ggplot(modalities2, aes(x = year, y = value)) +
        geom_line(aes(color = variable)) +
        geom_area(aes(fill = variable), alpha = 0.5) +
        geom_point(aes(color = variable)) +
        facet_wrap(~ variable) +
        labs(title = "Three modes of investing")
      
    }
    
  })
  
  # Table 
  
  output$balances <- renderPrint({
    
    init <- input$initial # Initial amount
    con <- input$ann # Annual contribution
    r <- input$return / 100 # Return rate
    g <- input$growth / 100 # Growth rate 
    years <- input$year # Year
    
    # Regular future value
    
    fv1 <- c()
    for (i in 0:years) {
      fv1[i+1] <- init * (1 + r)^i
      
    }
    
    
    # Future value with annuity
    
    fv2 <- c()
    for (i in 0:years) {
      fv2[i+1] <- init * (1 + r)^i + con * ((1 + r)^i - 1) / r
    }
    
    
    # Future value with growing annuity
    
    fv3 <- c()
    for (i in 0:years) {
      fv3[i+1] <- init * (1 + r)^i + con * ((1 + r)^i - (1 + g)^i)/(r - g)
    }
    
    # Creating data frame
    
    modalities <- data.frame(year = 0:years, no_contrib = fv1, fixed_contrib = fv2, growing_contrib = fv3)
    
    modalities
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

