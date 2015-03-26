NextNumber <- function(input, output.name="") {
    lines <- as.integer(readLines(input))[-1]
    answers <- sapply(lines, GetNext)
    output <- c()
    for (i in 1:length(answers)) {
        output <- c(output, paste0("Case #", i, ": ", answers[i]))
    }
    writeLines(output, output.name)
}

GetNext <- function(n) {
    print(n)
    n.strings <- Strings(n)
    if (length(n.strings)==1) return(n*10)
    n.unique <- sort(unique(n.strings))
    n.digits <- CountDigits(n.strings)
    i.digits <- NA
    i <- n
    while (!identical(i.digits, n.digits)) {
        i <- i+1
        i.strings <- Strings(i)
        if (identical(sort(unique(i.strings)),n.unique)) {
            i.digits <- CountDigits(i.strings)
        }
    }
    i
}

CountDigits <- function(strings) {
    as.data.frame(table(strings))
}

Strings <- function(n) {
    n.s <- unlist(strsplit(toString(n),""))
    n.s[n.s!=0]
}