SnapperChainLarge <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        l <- as.integer(unlist(strsplit(lines[1]," ")))
        lines <- lines[-1]
        N <- l[1]
        K <- l[2]
        output <- c(output, paste0("Case #", case, ": ", LightState(N, K)))
        case <- case+1
    }
    writeLines(output, output.name)
}

LightState <- function(N, K) {
    states <- ifelse(K%%2==0,0,1)
    if (N>1) states <- c(states, sapply(2:N, function(n) ifelse(ceiling(K/2^(n-1))%%2==0,1,0)))
    ifelse(all(states==1),"ON","OFF")
}

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# SnapperChainLarge("A-large-practice.in", "snapper_large.out") 4.306981 4.407264 4.656036 4.549997 4.615544 5.967252    10