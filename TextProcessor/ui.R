library(shiny)
library(wordcloud2)

# Define UI
shinyUI(navbarPage("Artem Tries to Process Natural Language",
    tabPanel("Introduction",
            fluidPage(
                titlePanel("Welcome!"),
                htmlOutput("welcome")
            )
    ),
    tabPanel("Plain Text",
        sidebarLayout(
            sidebarPanel(
                selectInput("source", "Select Text Source", choices = c("All" = "all",
                                                                        "Twitter" = "twitter", 
                                                                        "News" = "news", 
                                                                        "Blogs" = "blogs"), selected = "Twitter"),
                numericInput("lines", "Number of Lines", value = 100, max = 10000, min = 10, step = 1),
                sliderInput("lineSelect", "Which line to display", value = c(10, 100), max = 100, min = 10, step = 1)
            ),
            mainPanel(
                htmlOutput("debug")
            )
        )
    ),
    tabPanel("Search Results", 
        sidebarLayout(
            sidebarPanel(
                textInput("searchWord", "Search for lines with a specific word")
            ),
            mainPanel(
                htmlOutput("found")
            )
        ) 
    ),
    tabPanel("Pre-processed Text",
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput("mapping", "Select processing to apply",
                                   choiceNames = list("Stem", "Stop Words", "Whitespace", "Profanity", "Numbers", "Punctuation"),
                                   choiceValues = list("stem", "stopwords", "whitespace", "profanity", "numeric", "punct")),
                sliderInput("filterLineSelect", "Which line to display", value = c(10, 100), max = 100, min = 10, step = 1)
            ), mainPanel(
                htmlOutput("cleaned")
            )
        )
    ),
    tabPanel("Graphs",
        sidebarLayout(
            sidebarPanel(
                selectInput("graph", "Select Visualization Type", 
                            choices = c("Wordcloud" = "wordcloud", 
                                        "Histogram" = "hist", 
                                        "N-grams" = "ngram"),
                            selected = "Wordcloud"),
                conditionalPanel(
                    condition = "input.graph == 'wordcloud' || input.graph == 'hist'",
                    sliderInput("topSize", "Select how many of the most frequent occurences to show", value = 15, max = 50, min = 5, step = 1),
                    sliderInput("wordSize", "Select word legths to scope", value = c(4,20), max = 20, min = 4, step = 1)
                ),
                conditionalPanel(
                    condition = "input.graph == 'ngram'",
                    sliderInput("gramSize", "Select n-gram size", value = 2, max = 10, min = 2, step = 1)
                )
            ), mainPanel(
                conditionalPanel(
                    condition = "input.graph == 'wordcloud'",
                    wordcloud2Output("graph")
                ),
                conditionalPanel(
                    condition = "input.graph == 'hist'",
                    plotOutput("hist")
                ),
                conditionalPanel(
                    condition = "input.graph == 'ngram'",
                    dataTableOutput("ngrams")
                )
            )
        )
    ),
    tabPanel("Prediction",
        sidebarLayout(
            sidebarPanel(
                textInput("inputString", "Enter a sentence or phrase"),
                actionButton("predictAction", "Click to predict the next word")
            ), mainPanel(
                dataTableOutput("nextWordProbs")
            )
        )
    )
))
