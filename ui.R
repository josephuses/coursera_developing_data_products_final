
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Simulation App for the Central Limit Theorem"),
  # Sidebar with a slider input for type of distributionT and slider input.
  # FOR number of bins
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(8,
                      radioButtons("dist", "Distribution type:",
                                   c("Normal" = "Normal",
                                     "Uniform" = "Uniform",
                                     "Lognormal" = "Lognormal",
                                     "Exponential" = "Exponential")))),
      fluidRow(column(8,
                      sliderInput("nobs",
                                  "Number of samples:",
                                  min = 10,
                                  max = 50000,
                                  value = 500))),
      fluidRow(column(8,
                      numericInput("sampsize",
                                   "Sample size:",
                                   min = 2,
                                   max = 5000,
                                   value = 2))),
      fluidRow(column(8,
                      submitButton(text = "Run Simulation", icon = NULL, width = NULL)))),
    # Show a plot of the generated distribution
    
    mainPanel(h4("Use the panel on the left to:"),
              p("1. choose the distribution type"),
              p("2. indicate the number of independent samples from the distribution"),
              p("3. set the sample size of each random sample from the distribution"),
              h4("Notes"),
              p("1. Hit the", span(strong("Run Simulation")), "button to apply the changes and run the simulation."),
              p("2. Changing the sample size for the same number of samples will only change the plot of the sample means."),
              h4("Guide Questions"),
              p("1. What does the histogram of the sample means look like as we increase the size of the samples?"),
              p("2. What does the histogram of the sample means look like as we increase the size the number of random samples?"),
              p("2. How many samples are required for the histogram of the sample means to look like a bell curve as we increase the sample size?"),
              
             plotOutput("distPlot")
    )
  )	 
)
)
