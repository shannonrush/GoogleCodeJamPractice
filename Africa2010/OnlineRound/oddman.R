OddMan <- function(input, output.name) {
    lines <- readLines(input)[-1]
    guests <- lines[seq(2, length(lines), 2)]
    output <- c()
    for (i in 1:length(guests)) {
        codes <- unlist(strsplit(guests[i], " "))
        dups <- codes[duplicated(codes) | duplicated(codes, fromLast=TRUE)]
        output <- c(output, paste0("Case #", i, ": ", codes[which(!(codes %in% dups))]))
    }
    writeLines(output, output.name)
}