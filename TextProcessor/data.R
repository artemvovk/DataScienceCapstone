# Loading Data Functions
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

getCorpus <- function(lang, sources = NULL, samplesize = NULL){
    if (is.null(samplesize)) {
        path <- paste("~/Downloads/SwiftKeyData/", lang, "/", sep = "")
        files <- DirSource(path, encoding = "ISO 8859-1")
        # If you wanna save changes to DB
        # PCorpus(files, dbControl=list(useDB=TRUE,dbName="./textDB",dbType="DB1"))
        return(Corpus(files))
    } else {
        if(is.null(sources)){
            sources <- c("blogs", "twitter", "news")
        }
        sample <- list()
        for(source in sources){
            con <- load_file_con(source, lang)
            sample <- c(sample, readLines(con, samplesize, skipNul = TRUE, encoding = "ISO 8859-1"))
        }
        close.connection(con)
        return(Corpus(VectorSource(sample)))
    }
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