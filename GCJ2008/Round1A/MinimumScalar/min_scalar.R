MinScalar <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines) > 0) {
        x <- OrderedVector(lines[2])
        y <- OrderedVector(lines[3])
        lines <- tail(lines, length(lines)-3)
        output <- c(output, paste0("Case #", case, ": ", sum(x*rev(y))))
        case <- case + 1
    }
    writeLines(output, output.name)
}

OrderedVector <- function(s) {
    v <- as.numeric(unlist(strsplit(s," ")))
    v[order(v)]   
}

# small 182ms
# large 30ms

# O(n^2 log(n))