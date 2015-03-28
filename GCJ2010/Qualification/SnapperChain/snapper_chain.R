SnapperChain <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        l <- as.integer(unlist(strsplit(lines[1]," ")))
        lines <- lines[-1]
        N <- l[1]
        K <- l[2]
        output <- c(output, paste0("Case #", case, ": ", LightState(rep(0,N), K)))
        case <- case+1
    }
    writeLines(output, output.name)
}

LightState <- function(chain, K) {
    k <- 0
    while (k<K) {
        chain <- sapply(1:length(chain), function(i) if (i==1||all(chain[1:i-1]==1)) abs(chain[i]-1) else chain[i])
        k<-k+1
    }
    ifelse(all(chain==1),"ON","OFF")
}

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# SnapperChain("A-small-practice.in", "") 27.62053 27.62053 27.62053 27.62053 27.62053 27.62053     1
