library(gmp)

NextLarge <- function(input, output.name="") {
    lines <- as.bigz(readLines(input))[-1]
    answers <- sapply(lines, GetNext)
    output <- c()
    for (i in 1:length(answers)) {
        output <- c(output, paste0("Case #", i, ": ", answers[i]))
    }
    writeLines(output, output.name)
}

GetNext <- function(n) {
    n.s <- as.integer(c("0",unlist(strsplit(toString(n),""))))
    end.i <- EndIndex(n.s)
    a <- n.s[1:(end.i-1)]
    b <- n.s[end.i:length(n.s)]
    d <- tail(a,1)
    d.prime <- min(b[b>d])
    b[which(b==d.prime)[1]] <- d
    pre <- c(head(a,length(a)-1), d.prime)
    if (pre[1]=="0") pre <- pre[-1]
    suf <- sort(b)
    paste(c(pre,suf),collapse="")
}

EndIndex <- function(n.strings) {
    i <- length(n.strings)
    while (n.strings[i]<=n.strings[i-1]) {
        if (i==1) return(1)
        i <- i-1
    }
    i
}
