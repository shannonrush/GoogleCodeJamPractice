AlienLanguage <- function(input, output.name) {
    lines <- readLines(input)
    l <- as.integer(unlist(strsplit(lines[1]," ")))
    lines <- lines[-1]
    L <- l[1]
    D <- l[2]
    dictionary <- as.data.frame(matrix(unlist(strsplit(lines[1:D],"")), nrow=D, ncol=L, byrow=T))
    lines <- lines[-(1:D)]
    output <- c()
    case <- 1
    while (length(lines) > 0) {
        letters <- SplitWord(lines[1])
        lines <- lines[-1]
        output <- c(output, paste0("Case #", case, ": ", NumMatching(letters, dictionary)))
        case <- case + 1
    }
    writeLines(output, output.name)
}

NumMatching <- function(letters, d) {
    for (i in 1:length(letters)) {
        chars <- unlist(strsplit(letters[i],""))
        matching <- d[,i]%in%chars
        if (!any(matching)) {
            return(0)
        } else {
            d <- d[matching ,]
        }
    }
    nrow(d)
}

SplitWord <- function(s) {
    chars <- unlist(strsplit(s,""))
    letters <- c()
    while (length(chars) > 0) {
        if (chars[1]=="(") {
            close <- which(chars==")")[1]
            letter <- paste(chars[2:(close-1)], collapse="")
            chars <- chars[-c(1:close)]
        } else {
            letter <- chars[1]
            chars <- chars[-1]
        }
        letters <- c(letters, letter)
    }
    letters
}

# test
# SplitWord("abc")==c("a","b","c")
# SplitWord("(ab)(bc)(ca)")==c("ab","bc","ca")
# SplitWord("(ab)b(ca)")==c("ab","b","ca")
# SplitWord("a(bc)a")==c("a","bc","a")

# Unit: milliseconds
# expr        min         lq       mean     median         uq       max neval
# AlienLanguage("A-small-practice.in", "alien_small.out")   33.77942   35.42873   42.51513   36.49809   38.39273  258.4873   100
# AlienLanguage("A-large-practice.in", "alien_large.out") 8047.88677 8097.90152 8133.54276 8123.32962 8148.22015 8441.6166   100

