Magicka <- function(input, output.name="") {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        l <- unlist(strsplit(lines[1]," "))
        lines <- lines[-1]
        C <- as.integer(l[1])
        combine <- Combos(C, l)
        l <- tail(l, length(l)-(C+1))        
        oppose <- Oppose(l)
        invoke <- unlist(strsplit(tail(l, 1),""))
        answer <- paste0("[",paste(Play(invoke, combine, oppose), collapse=", "),"]")
        output <- c(output, paste0("Case #", case, ": ", answer))
        case <- case + 1
    }
    writeLines(output, output.name)
}

Play <- function(invoke, combine, oppose) {
    elements <- c()
    for (base in invoke) {
        elements <- c(elements, base)
        if (length(elements)>1) elements <- CombineLast(elements, combine)
        if (length(elements)>1) elements <- ResolveOpposing(elements, oppose)
    }
    elements
}

ResolveOpposing <- function(elements, oppose) {
    if (!is.null(oppose)) {
        m <- combn(elements,2)
        pairs <- sapply(1:ncol(m), function(i) paste0(m[1,i],m[2,i]))
        if (any(pairs %in% oppose)) elements <- c()
    }
    elements
}

CombineLast <- function(elements, combine) {
    if (!is.null(combine)) {
        p <- paste0(tail(elements,2),collapse="")
        if (p %in% combine$pair) {
            elements <- c(head(elements, length(elements)-2), as.character(combine[combine$pair==p, "result"])[1]) 
        }
    }
    elements
}

Combos <- function(C, l) {
    if (C==0) return(NULL)
    combos <- l[2:(1+C)]
    pair <- c()
    result <- c()
    for (combo in combos) {
        p <- unlist(strsplit(combo, ""))
        pair <- c(pair, paste0(p[1],p[2]), paste0(p[2],p[1]))
        result <- c(result, p[3], p[3])
    }
    data.frame(pair, result)
}

Oppose <- function(l) {
    D <- as.integer(l[1])
    if (D==0) return(NULL)
    opposing <- l[2:(1+D)]
    oppose <- c()
    for (pair in opposing) {
        p <- unlist(strsplit(pair,""))
        oppose <- c(oppose, paste0(p[1],p[2]), paste0(p[2],p[1]))
    }
    oppose
}

# Unit: milliseconds
# expr       min        lq      mean    median        uq       max neval
# Magicka("B-small-practice.in", "magicka_small.out")  225.1576  226.4674  234.9723  229.2581  234.7415  278.9165    10
# Magicka("B-large-practice.in", "magicka_large.out") 1696.9610 1715.3637 1725.7661 1726.6984 1742.1244 1755.2876    10