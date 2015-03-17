SaveUniverse <- function(input, output.name) {
    lines <- readLines(input)[-1]
    case <- 1
    output <- c()
    while (length(lines) > 0) {
        num.engines <- as.integer(lines[1])
        lines <- lines[-1]
        engines <- head(lines, num.engines)
        lines <- tail(lines, (length(lines)-num.engines))
        num.queries <- as.integer(lines[1])
        lines <- lines[-1]
        queries <- head(lines, num.queries)
        lines <- tail(lines, (length(lines)-num.queries))
        num.switch <- 0
        while (length(queries) > 0) {
            if (all(engines %in% unique(queries))) {
                m<-sapply(engines, function(e) min(which(queries==e)))
                num.switch <- num.switch + 1
                queries <- queries[max(m):length(queries)]
            } else {
                queries <- c()
            }
        }
        output <- c(output, paste0("Case #", case, ": ", num.switch))
        case <- case + 1
    }
    writeLines(output, output.name)
}

# O(n)
# small: 21ms
# large: 200ms
