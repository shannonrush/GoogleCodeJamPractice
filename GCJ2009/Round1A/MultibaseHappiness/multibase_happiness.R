
MultibaseHappiness <- function(input, output.name="") {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        bases <- as.integer(unlist(strsplit(lines[1]," ")))
        lines <- lines[-1]
        output <- c(output, paste0("Case #", case, ": ", FindHappy(bases)))
        case <- case + 1
    }
    writeLines(output, output.name)
}

FindHappy <- function(bases) {
    n <- 1
    happy.bases <- c()
    while (length(happy.bases)<length(bases)) {
        n <- n+1
        happy.bases <- c()
        for (b in bases) {
            if (IsHappy(ToBase(n, b), b, c())) happy.bases <- c(happy.bases, b)
        }
    }
    n
}

IsHappy <- function(n, b, seen) {
    if (n==1||b==2) {
        return(TRUE)
    } else if (n %in% seen) {
        return(FALSE)
    } else {
        seen <- c(seen, n)
        digits <- as.integer(unlist(strsplit(toString(n),"")))
        n <- sum(sapply(digits, function(x) x^2))
        IsHappy(ToBase(n, b), b, seen)
    }
}

ToBase <- function(n, base) {
    symbols <- c(as.character(0:9), LETTERS)
    max.len <- trunc(log(max(n, 1)) / log(base)) + 1
    power <- rep(1, length(n)) * base^((max.len-1):0)
    n <- n * rep(1, max.len)
    digits <- floor((n %% (base*power)) / power)
    paste(symbols[digits+1], collapse="")
}

# test

IsHappy(82,10,c())==TRUE
FindHappy(c(2,3))==3

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# MultibaseHappiness("A-small-practice.in", "") 193.2747 193.2747 193.2747 193.2747 193.2747 193.2747     1
