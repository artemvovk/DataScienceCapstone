
# Data processing libraries
library(parallel)
library(iterators)
library(foreach)
library(doParallel)
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

# Define server logic
shinyServer(function(input, output, session) {
    docs <- reactive({
        withProgress(message = "Downloading dataset", value = 0, {
            download_dataset()
        })
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
    output$welcome <- renderUI({
        return(HTML('
    <ul>
    <li><a href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"><strong>The Data</strong></a></li>
    </ul>
    <p>This exercise uses the files named <code>LOCALE.blogs.txt</code> where <code>LOCALE</code> 
       is the each of the four locales <code>en_US</code>, <code>de_DE</code>, <code>ru_RU</code> 
       and <code>fi_FI</code>. The data is from a corpus called HC Corpora. See the About the 
       Corpora reading for more details. The files have been language filtered but may still 
       contain some foreign text.[^language]<br>
       Each entry is tagged with it’s date of publication. Where user comments are included 
       they will be tagged with the date of the main entry.</p>
    <p>Each entry is tagged with the type of entry, based on the type of website it is 
       collected from (e.g. newspaper or personal blog) If possible, each entry is tagged 
       with one or more subjects based on the title or keywords of the entry (e.g. if the 
       entry comes from the sports section of a newspaper it will be tagged with “sports” 
       subject).In many cases it’s not feasible to tag the entries (for example, it’s not 
       really practical to tag each individual Twitter entry, though I’ve got some ideas 
       which might be implemented in the future) or no subject is found by the automated 
       process, in which case the entry is tagged with a ‘0’.</p>
    <h1><a id="The_Tabs_0"></a>The Tabs</h1>
        <p>At the top of the application you can see a set of available tabs. These appear 
        in the order of a standard data analysis process:</p>
    <h3><a id="Plain_Text_4"></a>Plain Text</h3>
        <p>Here is where you can just “look at the data” and its subsets. The options 
        available to you are in the side panel on the left. You can subset the data by 
        language, source, amount of lines, and offset. The idea here is to perform some 
        basic exploratory analysis.</p>
    <h3><a id="Search_Results_8"></a>Search Results</h3>
        <p>Another way of performing preliminary analysis of the data is to query the 
        dataset. So that’s what you can do here. Based on the subset chosen in the 
        <strong>Plain Text</strong> tab, you can enter a word or a phrase and the application 
        will search the chosen subset for that string.</p>
    <h3><a id="Preprocessed_Text_12"></a>Pre-processed Text</h3>
        <p>Here’s where the fun begins. Using some magical R packages you can clean and 
        pre-process the data chose in the <strong>Plain Text</strong> tab. It is possible 
        subset it further since the pre-processing is an intensive process, given the hardware. 
        This is not a glorious step, but very much necessary for any sensible data analysis.</p>
    <h3><a id="Graphs_16"></a>Graphs</h3>
        <p>Now the fun begins. Using the processed subset it is now possible to analyze the 
        dataset for potential patterns. Here we meet the limitations of current natural language 
        processing. It basically boils down to proper tokenization using large dictionaries. It is 
        possible to get mildly accurate counts with better stemming, but any <em>meaning</em> 
        from the data must be done by a human for now. Therefore, the tools given here are mainly 
        for visualization. Any inference is up to the user.</p>
    <h3><a id="Prediction_20"></a>Prediction</h3>
        <p>This is, by far, the most computationally intensive part of the application. Here is 
        where the pre-processing, tokenization, and some statistics come in to pretend to be a 
        human being. Given a phrase, the application will query the processed subset of the data 
        and attempt to find the next word by matching continuously smaller n-grams based on the 
        input string.</p>
        <p>So, given a 5-word phrase, the algorithm will go through the dataset and see if 
        any 5-grams match, then 4-grams, then 3-grams, down to bigrams. More weight is given 
        to longer grams based on the assumption that longer word patterns tend to rely on each 
        other more: e.g <code>the quick brown</code> is more likely to be followed by 
        <code>fox</code> than <code>bag</code> even though <code>brown bag</code> is a valid 
        phrase as well.</p>
    </body></html>
        '))
    })
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
            theme(axis.text=element_text(size=18), 
                  axis.title=element_text(size=32, face="bold"),
                  axis.text.x = element_text(angle = 45, hjust = 1))
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
