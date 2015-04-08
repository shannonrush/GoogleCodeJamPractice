CandySplitting <- function(input, output.name="") {
    lines <- readLines(input)[-1]
    lines <- lines[seq(2, length(lines), by=2)]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        candy <- as.integer(unlist(strsplit(lines[1]," ")))
        lines <- lines[-1]
        output <- c(output, paste0("Case #", case, ": ", SeanValue(candy)))
        case <- case + 1
    }
    writeLines(output, output.name)
}

SeanValue <- function(candy) {
    if (Reduce(bitwXor, candy)!=0) {
        return("NO")
    } else {
        return(sum(candy)-min(candy))   
    }
}

# Unit: milliseconds
# expr      min        lq      mean    median        uq
# CandySplitting("C-small-practice.in", "candy_small.out")  5.82197  6.081849  6.466606  6.176006  6.397699
# CandySplitting("C-large-practice.in", "candy_large.out") 36.74955 38.312079 39.970958 39.219753 40.473031
# max neval
# 11.15757   100
# 61.11901   100
