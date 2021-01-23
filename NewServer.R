library(stringr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(readr)
library(circlize)
library(reshape2)
library(textdata)
library(shiny)

# Define UI for miles per gallon application

ui <- fluidPage(
  headerPanel("Review Netizen tentang Makanan"),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot"),
  )
)

server <- function(input, output){
  myDataset <- read.csv("data_testing_full.csv")
  myDataset$length <- str_count(myDataset$content)
  test <- reactive({
   {
      test <- "sentimen Negatif & Positif";
    }
    return (test);
  })
  test1 <- reactive({
    {
      myDataset$content <- as.character(myDataset$content)
      
      tidy_texts <- myDataset %>%
        unnest_tokens(word,content)
      bng <- get_sentiments("bing")
      
      set.seed(1234)
      
      test1 <- tidy_texts %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = 'n', fill = 0) %>%
        comparison.cloud(colors = c('#F8766D', '#00BFC4'),
                         max.words = 250)
    }
    return (test1);
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    test()
  })
  
  output$plot <- renderPlot({
    test1()
  })
}

shinyApp(ui = ui, server = server)