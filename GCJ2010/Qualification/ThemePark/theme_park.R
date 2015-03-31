ThemePark <- function(input, output.name="") {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        l <- as.numeric(unlist(strsplit(lines[1]," ")))
        lines <- lines[-1]
        R <- l[1] # how many times coaster runs in day
        k <- l[2] # how many people coaster holds
        groups <- as.numeric(unlist(strsplit(lines[1]," ")))
        lines <- lines[-1]
        output <- c(output, paste0("Case #", case, ": ", EurosMade(R, k, groups)))
        case <- case+1
    }
    writeLines(output, output.name)
}

EurosMade <- function(R, k, groups) {
    euros <- 0
    while (R>0) {
        ride <- c()
        while (length(ride)<length(groups) && (sum(ride)+groups[1])<=k) {
            euros <- euros + groups[1]
            ride <- c(ride, groups[1])
            groups <- c(groups[-1],groups[1])
        }
        R <- R-1
    }
    euros
}

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# ThemePark("C-small-practice.in") 291.2682 297.4696 309.5419 302.1641 308.8396 461.4129   100