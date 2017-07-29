# Data processing libraries
library(magrittr)
library(dplyr)
library(slam)

# Visualization
library(ggplot2)
library(wordcloud2)

# NLP libraries
library(NLP)
library(openNLP)
library(openNLPmodels.en)

# Text mining
library(tm)
library(RWeka)
library(ngram)
library(tau)

load_file_con <- function(source = NULL, lang = NULL){
    if(is.null(source) || is.null(lang)){
        print("Language options are: ")
        print(paste(dir("~/Downloads/SwiftKeyData/"), sep = " "))
        print("Source options are: blogs, twitter, news")
        return("")
    }
    path <- paste("~/Downloads/SwiftKeyData/", lang, "/", lang, ".", source, ".txt", sep = "")
    con <- file(path, "r")
    return(con)
}

get_longest_line <- function(con){
    max <- 0
    for(line in readLines(con)){
        if(nchar(line) > max){
            max <- nchar(line)
        }
    }
    return(max)
}

count_lines_with_word <- function(con, word){
    count <- 0
    for(line in readLines(con)){
        if(grepl(word, line)){
            count <- count+1
        }
    }
    return(count)
}

get_lines_with_word <- function(docs, word){
    lines <- list()
    if((class(docs) == "SimpleCorpus")[1] || (class(docs) != "Corpus")[2]){
        for(i in seq(length(docs))){
            if(grepl(word, UStwitter[[i]]$content)){
                lines <- c(lines, docs[[i]]$content)
            }
        }
    }else {
        for(line in readLines(docs)){
            if(grepl(word, line)){
                lines <- c(lines, line)
            }
        }
    }
    
    return(lines)
}

annotate_entities <- function(doc, annotation_pipeline) {
    annotations <- annotate(doc, annotation_pipeline)
    AnnotatedPlainTextDocument(doc, annotations)
}

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
    s <- doc$content
    a <- annotations(doc)[[1]]
    if(hasArg(kind)) {
        k <- sapply(a$features, `[[`, "kind")
        s[a[k == kind]]
    } else {
        s[a[a$type == "entity"]]
    }
}

# Tokenization Pipeline
# lines <- readLines(load_file_con("twitter", "en_US"), 50)
# word_ann <- Maxent_Word_Token_Annotator()
# sent_ann <- Maxent_Sent_Token_Annotator()
# annotated <- annotate(lines, list(sent_ann, word_ann))
# lines_doc <- AnnotatedPlainTextDocument(lines, annotated)
# sents(lines_doc) %>% head(5)
# words(lines_doc) %>% head(10)

# Text Mining Pipeline
getCorpus <- function(lang, sources = NULL, samplesize = NULL){
    if (is.null(samplesize)) {
        path <- paste("~/Downloads/SwiftKeyData/", lang, "/", sep = "")
        files <- DirSource(path, encoding = "UTF-8")
        return(Corpus(files))
    } else {
        if(is.null(sources)){
            sources <- c("blogs", "twitter", "news")
        }
        sample <- list()
        for(source in sources){
            con <- load_file_con(source, lang)
            sample <- c(sample, iconv(readLines(con, samplesize, skipNul = TRUE, encoding = "UTF-8"), 'UTF-8', 'ASCII'))
        }
        return(Corpus(VectorSource(sample)))
    }
}

# Make lowercase, drop stop words, remove weird spaces
removeToken <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x, perl = TRUE))})
cleanWords <- function(docs){
    docs <- tm_map(docs,content_transformer(tolower))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeToken, "’")
    docs <- tm_map(docs, removeToken, "‘")
    docs <- tm_map(docs, removeToken, "-")
    myStopwords <- c(stopwords("english"), "via")
    # remove 'r' and 'big' from stopwords
    myStopwords <- setdiff(myStopwords, c("r", "big"))
    docs <- tm_map(docs, removeWords, myStopwords)
    docs <- tm_map(docs, removeToken, "(?<=[\\s])\\s*|^\\s+|\\s+$")
    return(docs)
}

cleanProfanities <- function(docs, lang = "en_US"){
    path <- paste0(c("~/Dropbox/Coding/Coursera/Capstone/profanity_", lang, ".txt"), collapse = "")
    prof_con <- file(path, encoding = "ISO 8859-1")
    badWords <- trimws(readLines(prof_con))
    docs <- tm_map(docs,content_transformer(tolower))
    docs<- tm_map(docs, removeWords, badWords)
    return(docs)
}

getDocMatrix <- function(docs, wordL = c(4,20)) {
    return(DocumentTermMatrix(docs, control=list(wordLengths = wordL, bounds = list(global = c(3, (length(docs)/1.5))))))
}

getNGram <- function(docs, ngL = 2, ngH = 2){
    #options(mc.cores=1)
    tokenizer <- NGramTokenizer(docs, Weka_control(min = ngL, max = ngH))
    df <- data.frame(table(token=tokenizer))
    return(df[order(df$Freq, decreasing = TRUE),])
} 

getFreqWords <- function(docs, wordL = c(4,20), top = 10){
    if((class(docs) != "TermDocumentMatrix")[1] & (class(docs) != "DocumentTermMatrix")[1]){
        docs <- DocumentTermMatrix(docs, control=list(wordLengths=wordL,
                                                      bounds = list(global = c(3, (length(docs)/1.5)) )))   
    }
    
    freq <- colSums(as.matrix(docs))
    ord <- order(freq, decreasing = TRUE)
    return(freq[head(ord, top)])
}
