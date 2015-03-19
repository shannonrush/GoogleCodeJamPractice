NumberSets <- function(input, output.name) {
    lines <- readLines(input)[-1]
    case <- 1
    output <- c()
    pf.main <<- list()
    while (length(lines) > 0) {
        print(case)
        l <- as.integer(unlist(strsplit(lines[1]," ")))
        lines <- lines[-1]
        start <- l[1]
        end <- l[2]
        P <- l[3]
        primes <- PrimesTo(end)
        subset.primes <- primes[primes >= P]
        prime.factors <- lapply(start:end, PrimeFactors, P, primes)
        int.sets <- lapply(start:end, c)
        for (prime in subset.primes) {
            sets <- which(sapply(prime.factors, function(x) prime %in% x))
            if (length(sets) > 1) {
                target <- sets[1]
                for (set in sets[-1]) {
                    int.sets[[target]] <- c(int.sets[[target]], int.sets[[set]])
                    prime.factors[[target]] <- union(prime.factors[[target]], prime.factors[[set]])
                }
                int.sets[sets[-1]] <- NULL
                prime.factors[sets[-1]] <- NULL
            }
        }
        num.sets <- length(int.sets[!is.na(int.sets)])
        output <- c(output, paste0("Case #", case, ": ", num.sets))
        case <- case + 1
    }
    print(output)
    writeLines(output, output.name)
}

PrimesTo <- function(n) {
    primes <- c()
    for (i in 2:n) {
        if (i <= 7 || (i%%2!=0 & i%%3!=0 & i%%5!=0 & i%%7!=0)) {
            if (IsPrime(i)) primes <- c(primes, i)  
        }
    }
    primes
}

PrimeFactors <- function(x, P, primes) {
    if (as.character(x) %in% names(pf.main)) {
        return(LookupPF(as.character(x), P))
    } else {
        all.factors <- c()
        for (i in 2:x) if (i%in%primes & x%%i==0) all.factors <- c(all.factors, i)
        if (length(all.factors) > 0) {
            pf.main[as.character(i)] <<- list(all.factors)
            return(LookupPF(as.character(x), P))
        } else {
            return(NA)
        }
    }
}

LookupPF <- function(x, P) {
    factors <- unlist(pf.main[x])
    to.return <- unname(factors[factors >= P])
    if (length(to.return)>0) {
        to.return
    } else {
        NA
    }
}

IsPrime <- function(n) {
    m <- 2
    max <- n
    while (m < max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m <- m+1
    }
    TRUE
}