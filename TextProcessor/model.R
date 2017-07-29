library(dplyr)

calcProbs <- function(tokenDF){
    tokenDF$discount <- rep(1, nrow(tokenDF))
    if(nrow(tokenDF) < 1){
        return(tokenDF)
    }
    for(i in 1:max(tokenDF$count)){
        r <- i
        r_plus <- r+1
        N <- nrow(tokenDF[count == r])
        N_plus <- nrow(tokenDF[count == r_plus])
        disc <- (r_plus/r) * (N_plus/N)
        tokenDF <- tokenDF[count == N, discount := disc]
    }
    return(tokenDF)
}

leftOverProbs <- function(tokenDF){
    tokenDFLeftOver <- tokenDF[, .(left_prob = calcLeftOverProb(last_word, count, discount)), by=last_word]
    return(tokenDFLeftOver)
}

calcLeftOverProb <- function(last_word, count, discount){
    all_freq <- sum(count)
    return(1-sum((discount*count)/all_freq))
}


removeToken <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x, perl = TRUE))})

cleanInput <- function(input){
    
    words <- Corpus(VectorSource(input))
    
    words <- tm_map(words ,content_transformer(tolower))
    words <- tm_map(words, removePunctuation)
    words <- tm_map(words, removeToken, "’")
    words <- tm_map(words, removeToken, "‘")
    words <- tm_map(words, removeToken, "-")
    words <- tm_map(words, removeNumbers)
    words <- tm_map(words, removeToken, "(?<=[\\s])\\s*|^\\s+|\\s+$")
    words <- tm_map(words, stemDocument)
    
    
    return(words)
}

tokenizeInput <- function(input){
    words <- unlist(strsplit(input[[1]]$content, " "))
    grams <- data.table(
        remainder=character(),
        last_gram=character(),
        stringsAsFactors = FALSE
    )
    for(n in 1:length(words)){
        end_gram <- paste0(tail(words, n), collapse = "_")
        leftover <- paste0(head(words, length(words)-n), collapse = "_")
        grams <- rbind(grams, data.frame(remainder=leftover, last_gram=end_gram, stringsAsFactors = FALSE))
    }
    
    return(grams)
    
}

nextWordProb <- function(input, corpus = NULL){
    words <- cleanInput(input)
    words <- tokenizeInput(words)
    maxGram <- nrow(words)
    if(maxGram < 2){
        return(NULL)
    }
    
    tokenized <- nTokenize(x = corpus, n = maxGram+1)
    grams <- tokensDF(tokenized, gramSize = maxGram+1)[first_terms == words[maxGram,]$last_gram]
    grams <- calcProbs(grams)
    
    for(scope in (maxGram):2){
        
        tokenized <- nTokenize(x = corpus, n = scope)
        next_grams <- tokensDF(tokenized, gramSize = scope)[first_terms == words[scope-1,]$last_gram]
        next_grams <- calcProbs(next_grams)
        grams <- rbind(grams, next_grams)
    }
    freq_sum <- sum(grams$count)
    all_freq <- grams[, sum(count*discount)/freq_sum, by= last_word]
    colnames(all_freq) <- c("word", "prob")
    return(all_freq[prob > 0.01])
}