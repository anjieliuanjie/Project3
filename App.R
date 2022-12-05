# import libraries
library(shiny)

# I exported the tidied tibble from Project2.Rmd using write.csv()

# import CSV file as tibble
Project2 <- read.csv("Project2.csv")

# UI
ui <- fluidPage(
  titlePanel("Project 3"),
  sidebarLayout
  (
    sidebarPanel
    (
      helpText("Select a variable to see its distribution"),
      selectInput
      (
        "selectvar", 
        label = h3("Choose a variable"),
        choices = list("Sales" = 1, "DollarVolume" = 2, "AveragePrice" = 3, "MedianPrice" = 4, "Area" = 5),
        selected = 1
      ),
      helpText("Select a color of the graph."),
      selectInput
      (
        "selectColor", 
        label = h3("Choose a color"),
        choices = list("Pink" = 1, "Dark olive green" = 2, "Beige" = 3, "Khaki" = 4, "Sky blue" = 5),
        selected = 1
      ),
      helpText("Drag to change the number of bins of the graph."),
      sliderInput
      (
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),
      helpText("Drag to see how the graph of a variable changes as the number of sales changes"),
      sliderInput
      (
        "sales",
        "Number of sales:",
        min = min(Project2[,3]),
        max = max(Project2[,3]),
        value = max(Project2[,3])
      )
    ),
    mainPanel
    (
      helpText("This Shiny App displays the univariable distributions of variables regarding real estate data collected from Houston and Austin area. It will also display the statistics of the variable selected."),
      img(src = "710 S Fry Road Katy 77450.jpg", height = 140, width = 240),
      
      plotOutput("distPlot"),
      fluidRow(column(6, verbatimTextOutput("stats")))
      
    )
  )
)


server <- function(input, output)
{
  createHistogram <- function(variable, name, unit, color)
  {
    title = paste("Distribution of", name)
    hist(variable, 
         breaks = input$bins, 
         main = title, 
         xlab = unit, 
         col = color, 
         border = "white")
  }
  
  createBarplot <- function(variable, name, color)
  {
    title = paste("Distribution of", name)
    barplot(table(variable), 
            main = title, 
            xlab = name, 
            col = color)
  }
  
  col = "white"
  
  colorChoice <- function(choice)
  {
    if(choice == 1)
    {
      col = "pink"
    }
    else if(choice == 2)
    {
      col = "darkolivegreen2"
    }
    else if(choice == 3)
    {
      col = "beige"
    }
    else if(choice == 4)
    {
      col = "khaki1"
    }
    else
    {
      col = "skyblue"
    }
  }
  output$distPlot <- renderPlot(
    {
      col = colorChoice(input$selectColor)
      Project2 <- Project2[Project2$Sales <= input$sales,]
      if(input$selectvar == 1)
      {
        createHistogram(Project2$Sales,
                        "Sale",
                        "Number of Sales", 
                        col)
      }
      else if(input$selectvar == 2)
      {
        createHistogram(Project2$DollarVolume,
                        "Total Dollars of Sales",
                        "Total Dollars of Sales ($)",
                        col)
      }
      else if(input$selectvar == 3)
      {
        createHistogram(Project2$AveragePrice,
                        "Average Price",
                        "Average Price ($)",
                        col)
      }
      else if(input$selectvar == 4)
      {
        createHistogram(Project2$MedianPrice, 
                        "Median Price",
                        "Median Price ($)",
                        col)
      }
      else
      {
        createBarplot(Project2$Area,
                        "Areas",
                        col)
      }
    }
  )
  
  output$stats <- renderPrint(
    {
      if(input$selectvar != 5)
      {
        index = as.numeric(input$selectvar) + 2
        numeric = Project2[,index]
        writeLines(paste("Median:", round(median(numeric), 2)))
        writeLines(paste("First Quartile:", round(quantile(numeric)[2], 2)))
        writeLines(paste("Third Quartile:", round(quantile(numeric)[4], 2)))
      }
      else
      {
        table(Project2$Area)
      }
    }
  )
}

shinyApp(ui = ui, server = server)
