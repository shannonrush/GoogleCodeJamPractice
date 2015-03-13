Reverse <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    for (l in 1:length(lines)) {
        words <- unlist(strsplit(lines[l], " "))
        output <- c(output, (paste0("Case #", l, ": ", paste(rev(unlist(strsplit(lines[l], " "))), collapse=" "))))
    }
    writeLines(output, output.name)
}

