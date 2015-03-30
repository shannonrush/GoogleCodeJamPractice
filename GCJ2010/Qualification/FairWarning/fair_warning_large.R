library(gmp)

FairWarningLarge <- function(input, output.name="") {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        t <- as.bigz(unlist(strsplit(lines[1]," "))[-1])  
        lines <- lines[-1]
        output <- c(output, paste0("Case #", case, ": ", OptimumAnniversary(t)))
        case <- case + 1
    }
    writeLines(output, output.name)
}

OptimumAnniversary <- function(t) {
    T <- GetT(t)
    y<-min(t)
    if (y%%T==0) 0 else T-(y%%T)
}

GetT <- function(t) {
    n <- length(t)
    d <- c()
    for (i in 1:n) {
        for (j in 1:n) {
            if (i!=j) d <- unique(c(d, as.character(abs(t[i]-t[j]),b=10)))
        }
    }
    Reduce(gcd,(as.bigz(d)))
}

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# FairWarningLarge("B-small-practice.in", "") 60.88203 62.36211 63.61189 62.96874 63.55107 97.83044   100