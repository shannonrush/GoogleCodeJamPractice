T9Spelling <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    for (l in 1:length(lines)) {
        chars <- unlist(strsplit(lines[l],""))
        con <- unlist(lapply(chars, ConvertChar))
        # add spaces between like chars
        t9spelling <- lapply(1:length(con), 
                             function(i) ifelse(grepl(substr(con[i],1,1), con[i+1]), paste0(con[i]," "), con[i]))
        out.spelling <- paste(unlist(t9spelling),collapse="")
        output <- c(output, paste0("Case #", l, ": ", out.spelling))
    }
    writeLines(output, output.name)
}

ConvertChar <- function(char) {
    if (char %in% letters) {
        keys <- c("*","*","*",letters[-19])
        i <- which(keys==char)
        key <- ifelse(char %in% c("s","z"), ifelse(char=="s", 7, 9), ceiling(i/3))
        pos <- ifelse(char %in% c("s","z"), 4, ifelse(i%%3==0,3,i%%3))
        paste0(rep(key, pos), collapse="")
    } else {
        "0"
    }
}