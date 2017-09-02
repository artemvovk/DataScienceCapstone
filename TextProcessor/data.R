# Loading Data Functions
download_dataset <- function(){
    if (!file.exists("swiftkey.zip")){
        url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(url, destfile = "swiftkey.zip", method = "curl")
    } else if (!file.exists("data")){
        unzip("swiftkey.zip", exdir = "data")
    }
}

load_file_con <- function(source = NULL, lang = NULL){
    path <- "data/final/"
    if(is.null(source) || is.null(lang)){
        print("Language options are: ")
        print(paste(dir(path), sep = " "))
        print("Source options are: blogs, twitter, news")
        return("")
    }
    path <- paste(path, lang, "/", lang, ".", source, ".txt", sep = "")
    con <- file(path, "r")
    return(con)
}

scramble <- function(vec,n) {
    res <- tapply(vec,(seq_along(vec)+n-1)%/%n,
                  FUN=function(x) x[sample.int(length(x), size=length(x))])
    unname(unlist(res))
}

getCorpus <- function(lang, sources = c("all"), samplesize = 1000){
    if(sources == c("all")){
        sources <- c("blogs", "twitter", "news")
    }
    sample <- list()
    for(source in sources){
        con <- load_file_con(source, lang)
        sample <- c(sample, readLines(con, samplesize, skipNul = TRUE, encoding = "ISO 8859-1"))
        sample <- scramble(sample, length(sample)/2)
    }
    close.connection(con)
    return(Corpus(VectorSource(sample)))
}

getFreqWords <- function(docs, wordL = c(4,20), top = 10){
    docs <- Corpus(VectorSource(unlist(docs)))
    if((class(docs) != "TermDocumentMatrix")[1] & (class(docs) != "DocumentTermMatrix")[1]){
        docs <- DocumentTermMatrix(docs, control=list(wordLengths=wordL,
                                                      bounds = list(global = c(3, (length(docs)/1.5)) )))
    }
    freq <- colSums(as.matrix(docs))
    ord <- order(freq, decreasing = TRUE)
    freq <- freq[head(ord, top)]
    return(freq)
}