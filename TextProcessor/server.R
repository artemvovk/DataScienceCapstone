#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Data processing libraries
library(tm)
library(magrittr)
library(dplyr)
library(slam)
library(data.table)

# NLP libraries
library(NLP)
library(openNLP)
library(openNLPmodels.en)

# Text mining
library(filehash)
library(quanteda)

# Shiny and visualization
library(ggplot2)
library(wordcloud2)
library(shiny)

# Local modules:
source("data.R")
source("filter.R")
source("tokenizers.R")
source("model.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    docs <- reactive({
        suppressWarnings(getCorpus("en_US", as.character(input$source), as.numeric(input$lines)))
    })
    
    # Data Preprocessing/Cleanup Functions
    removeToken <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x, perl = TRUE))})
    
    cleanWords <- reactive({
        data <- tm_map(docs() ,content_transformer(tolower))
        list("stopWords", "whitespace", "profanity", "numeric", "punct")
        if("punct" %in% input$mapping){
            data <- tm_map(data, removePunctuation)
            data <- tm_map(data, removeToken, "’")
            data <- tm_map(data, removeToken, "‘")
            data <- tm_map(data, removeToken, "-")
        }
        if("stopwords" %in% input$mapping){
            myStopwords <- c(stopwords("english"), "via")
            # remove 'r' and 'big' from stopwords
            myStopwords <- setdiff(myStopwords, c("r", "big"))
            data <- tm_map(data, removeWords, myStopwords)
        }
        
        if("profanity" %in% input$mapping){
            path <- paste0(c("~/Dropbox/Coding/Coursera/Capstone/profanity_", "en_US", ".txt"), collapse = "")
            prof_con <- file(path, encoding = "ISO 8859-1")
            badWords <- trimws(readLines(prof_con))
            data <- tm_map(data, removeWords, badWords)
        }
        if("numeric" %in% input$mapping){
            data <- tm_map(data, removeNumbers)
        }
        if("whitespace" %in% input$mapping){
            data <- tm_map(data, removeToken, "(?<=[\\s])\\s*|^\\s+|\\s+$")
        }
        if("stem" %in% input$mapping){
            data <- tm_map(data, stemDocument)
        }
        return(data)
    })
    
    # Calculate Frequencies of word occurences
    wordFreqs <- reactive({
        return(freqDF(docs = filterLinesReturn(),wordSize = input$wordSize, head = input$topSize))
    })

    # Responsive UI functions:
    # Scope down the number of read lines
    observe({
        updateSliderInput(session, "lineSelect", max = as.numeric(input$lines))
    })
    linesReturn <- reactive({
        linesReturn <- list()
        for(ind in seq(1, as.numeric(input$lineSelect[2])-as.numeric(input$lineSelect[1]))){
            linesReturn <- c(linesReturn, docs()[[ind+as.numeric(input$lineSelect[1])]]$content)
        }
        return(linesReturn)
    })
    
    # Scope down the number of filtered lines
    observe({
        updateSliderInput(session, "filterLineSelect", max = as.numeric(length(cleanWords())))
    })
    filterLinesReturn <- reactive({
        linesReturn <- list()
        for(ind in seq(1, as.numeric(input$filterLineSelect[2])-as.numeric(input$filterLineSelect[1]))){
            linesReturn <- c(linesReturn, cleanWords()[[ind+as.numeric(input$filterLineSelect[1])]]$content)
        }
        return(linesReturn)
    })
    
    # Output processing:
    output$debug <- renderUI({
        ol <- paste(c("<ol start=\"", input$lineSelect[1], "\">"), collapse = "")
        htmlOut <- c(HTML(ol))
        for(line in linesReturn()){
            htmlOut <- c(htmlOut, HTML("<li>"), line, HTML("</li>"))
        }
        htmlOut <- c(htmlOut, HTML("</ol>"))
        return(HTML(htmlOut))
    })
    output$found <- renderUI({
        htmlOut <- c(HTML("<ul>"))
        if(input$searchWord != ""){
            searchResult <- get_lines_with_word(docs, input$searchWord)
            if(length(searchResult[,1]) == 1){
                return(HTML("<br>"))
            }
            for(idx in seq(2, nrow(searchResult))){
                htmlOut <- c(htmlOut, 
                             HTML("<li>"),
                             HTML("<b>"),
                             as.character(searchResult[idx, ][1]),
                             ". ",
                             HTML("</b>"),
                             as.character(searchResult[idx, ][2]),
                             HTML("</li>"))
            }
        }
        htmlOut <- c(htmlOut, HTML("</ul>"))
        return(HTML(htmlOut))
    })
    
    output$cleaned <- renderUI({
        data <- filterLinesReturn()
        ol <- paste(c("<ol start=\"", input$filterLineSelect[1], "\">"), collapse = "")
        htmlOut <- c(HTML(ol))
        for(idx in seq(1, length(data))){
            htmlOut <- c(htmlOut, HTML("<li>"), data[[idx]], HTML("</li>"))
        }
        htmlOut <- c(htmlOut, HTML("</ol>"))
        return(HTML(htmlOut))
    })
    output$graph <- renderWordcloud2({
        lines <- wordFreqs()
        if(is.null(lines)){
            return(NULL)
        }
        wordcloud2(data = lines,
                   fontFamily = "Iosevka Light",
                   shape = "pentagon",
                   color = "random-dark")
    })
    output$hist <- renderPlot({
        lines <- wordFreqs()
        if(is.null(lines)){
            return(NULL)
        }
        ggplot(lines, aes(x = reorder(word, -frequency), y = frequency)) + 
            geom_bar(stat = "identity") + labs(x = "Word", y = "Count") + 
            theme(axis.text=element_text(size=18), axis.title=element_text(size=32,face="bold"))
    })
    output$ngrams <- renderDataTable({
        tokenized <- nTokenize(x = filterLinesReturn(), n = as.numeric(input$gramSize))
        return(tokensDF(tokenized, gramSize = input$gramSize))
    })
    predictTable <- eventReactive(input$predictAction, {
        string <- as.character(input$inputString)
        return(nextWordProb(string, filterLinesReturn()))
    })
    output$nextWordProbs <- renderDataTable({
        return(predictTable())
    })
})
