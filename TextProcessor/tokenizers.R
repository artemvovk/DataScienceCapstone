nTokenize <- function(x, n = 2){
    output <- quanteda::tokenize(unlist(x), ngrams=n, simplify = TRUE)
} 
tokensDF <- function(x, gramSize = 2){
    featMatrix <- dfm(x)
    df <- data.table(first_terms = featnames(featMatrix), count = colSums(featMatrix), key = "first_terms")
    df <- df[like(first_terms, "([^ ]|[^_])([a-zA-Z0-9]+)*_+([a-zA-Z0-9]+_)*")]
    df <- addLastTerm(df)
    df$first_terms <- subGram(df)
    return(setcolorder(df[order(count, decreasing = TRUE)], c("first_terms", "last_word", "count")))
}

subGram <- function(freqDF){
    ngrams <- freqDF$first_terms
    for(idx in 1:length(ngrams)){
        words <- unlist(strsplit(ngrams[idx], "_"))
        ngrams[idx] <- paste0(words[-length(words)], collapse = "_")
    }
    return(ngrams)
}

addLastTerm <- function(freqDF){
    last <- list()
    for(ngram in freqDF$first_terms){
        lastw <- unlist(strsplit(ngram, "_"))
        if(length(tail(lastw, 1)) == 0){
            last <- c(last, "")
        }
        
        else {
            last <- c(last, tail(lastw, 1))
        }
    }
    freqDF$last_word <- unlist(last)
    return(freqDF)
}

freqDF <- function(docs, wordSize = c(4,20), head = 15){
    wordSize <- c(as.numeric(wordSize[1]),as.numeric(wordSize[2]))
    freq <- getFreqWords(docs, wordL = wordSize, top = as.numeric(head))
    if(length(freq) == 0){
        return(NULL)
    }
    freq <- data.frame(word = names(freq), frequency = freq)
    return(freq)
}