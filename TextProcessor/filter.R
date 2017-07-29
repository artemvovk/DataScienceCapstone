# Filtering and Search Functions
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
    lines <- data.frame(index=0, line="line", stringsAsFactors = FALSE)
    data <- docs()
    if((class(data) == "SimpleCorpus") || (class(data) == "Corpus")){
        for(idx in seq(length(data))){
            if(grepl(word, data[[idx]]$content, ignore.case = TRUE)){
                print(c(idx, data[[idx]]$content))
                lines[nrow(lines)+1, ] <- c(idx, data[[idx]]$content)
            }
        }
    }else if(class(data) == "connection") {
        for(idx in seq(1, length(readLines(data)))){
            if(grepl(word, line, ignore.case = TRUE)){
                print(c(idx, line))
                lines[nrow(lines)+1, ] <- c(idx, line)
            }
        }
    }else {
        for(idx in seq(1, length(data))){
            if(grepl(word, data[idx], ignore.case = TRUE)){
                print(c(idx, line))
                lines[nrow(lines)+1, ] <- c(idx, line)
            }
        }
    }
    return(lines)
}