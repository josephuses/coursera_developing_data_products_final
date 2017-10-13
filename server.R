
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    
    # capture inputs
    nobs <- input$nobs
    sampsize <- input$sampsize
    disttype <- input$dist
    dist <- switch(disttype,
                   Normal = rnorm,
                   Uniform = runif,
                   Lognormal = rlnorm,
                   Exponential = rexp,
                   rnorm)
    
    set.seed(nobs)
    # generate nobs random numbers from the chosen distribution
    rnos <- dist(nobs)
    
    # create vector of sample means from the nobs random numbers
    sampleMeans <- c(rep(0, times=nobs))
    for (i in 1:nobs){
      sampleMeans[i] <- mean(sample(rnos, sampsize), replace = TRUE)
    }
    
    # create panel for two side-by-side plots
    # of the nobs observations from the distribution
    # and the nobs sample means of sampsize size
    
    df <- data.frame(
      rnos ,
      sampleMeans) 

    df %>%
      gather(samp, value, rnos:sampleMeans) %>%
      mutate(
        samp = factor(samp, 
                      labels = c(
                        paste(nobs, "Random Numbers from\n", disttype, "Distribution"), 
                        paste(nobs, "Sample Means of Sample Size", sampsize, "\nfrom", disttype, "Distribution")))) %>%
      ggplot(aes(value, color = samp)) + 
      geom_histogram(fill = "white", bins = min(30, nobs / sampsize)) + 
      facet_wrap(~samp, scale = "free") +
      theme_bw() + 
      xlab("") + 
      ylab("") + 
      theme(legend.title=element_blank())
      ##dist size, parameter from slider
  })
}
)
