
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("rErgocheck k() beta"),
  # Target point to optimize

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      selectInput("target_point", "Plot range for",
                  choices = c("--","A", "B", "C")),
      sliderInput("Xa",
                  "Xa=",
                  min = 0,
                  max = 5000,
                  value = 1100),
      sliderInput("Ya",
                  "Ya=",
                  min=0,
                  max=5000,
                  value=700),
      sliderInput("Xb",
                  "Xb=",
                  min = 0,
                  max = 5000,
                  value = 2000),
      sliderInput("Yb",
                  "Yb=",
                  min=0,
                  max=5000,
                  value=1700),
      sliderInput("Xc",
                  "Xc=",
                  min = 0,
                  max = 5000,
                  value = 3500),
      sliderInput("Yc",
                  "Yc=",
                  min=0,
                  max=5000,
                  value=1000),
      # Input: Choose dataset ----
      selectInput("dataset", "Contents",
                  choices = c("Basic Geom", "Rating","All")),
      textInput("concept_name",label = "Concept Name",value = "New Concept"),
      # Button
      downloadButton("downloadData", "Save"),
      width = 4
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("IntPlot"),
      tableOutput("KitchenInfo"),
      tableOutput("KitchenInfo2")
    )
  )
))
