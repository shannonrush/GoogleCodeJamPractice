Reverse <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    for (l in 1:length(lines)) {
        words <- unlist(strsplit(lines[l], " "))
        reversed <- unlist(lapply(length(words):1, function(w) words[w]))
        output <- c(output, (paste0("Case #", l, ": ", paste(reversed, collapse=" "))))
    }
    writeLines(output, output.name)
}

