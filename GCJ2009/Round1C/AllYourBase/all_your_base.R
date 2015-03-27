library(gmp)

AllYourBase <- function(input, output.name) {
    lines <- readLines(input)[-1]   
    answers <- sapply(lines, SecondsToWar)
    output <- c()
    case <- 1
    for (a in answers) {
        output <- c(output, paste0("Case #", case, ": ", a))
        case <- case + 1
    }
    writeLines(output, output.name)
}

SecondsToWar <- function(s) {
    alien.s <- unlist(strsplit(s,""))
    base.s <- AlienToBase(alien.s)
    BaseToSeconds(base.s)
}

BaseToSeconds <- function(base.s) {
    base <- max(length(unique(base.s)),2)
    rev.b <- rev(base.s)
    p <- 0
    sum <- 0
    for (int in rev.b) {
        sum <- sum+(base^p * int)
        p <- p+1
    }
    sum
}

AlienToBase <- function(alien.s) {
    symbols <- 0:(length(unique(alien.s))-1)
    dict <- AlienDict(symbols, alien.s)
    as.integer(unname(unlist(sapply(alien.s, function(a) dict[a]))))
}

AlienDict <- function(symbols, alien.s) {
    dict <- list()
    if (length(unique(alien.s))==1) {
        dict[[alien.s[1]]]<-1
    } else {
        dict[[alien.s[1]]]<-symbols[2]
        dict[[alien.s[min(which(alien.s!=alien.s[1]))]]]<-symbols[1]
        s <- 3
        for (char in alien.s) {
            if (!(char %in% names(dict))) {
                dict[[char]] <- symbols[s]
                s <- s+1
            }
        }
    }
    dict
}

# Unit: milliseconds
# expr      min       lq     mean   median      uq      max neval
# AllYourBase("A-small-practice.in", "base_small.out") 27.66091 28.21299 30.71991 28.80314 31.6324 112.1654   100
