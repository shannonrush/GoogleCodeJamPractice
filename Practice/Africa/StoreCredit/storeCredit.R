# One line containing the value C, the amount of credit you have at the store.
# One line containing the value I, the number of items in the store.
# One line containing a space separated list of I integers. Each integer P indicates the price of an item in the store.
# Output: Case #1: 2 3

lines <- readLines("A-large-practice.in")
num.cases <- lines[1]
elements <- lines[-1]
cases <- lapply(1:num.cases, function(i) c(elements[1+3*(i-1)], elements[3+3*(i-1)]))
output <- c()

for (x in 1:length(cases)) {
    c <- unlist(cases[x])
    credit <- as.numeric(c[1])
    items <- as.numeric(unlist(strsplit(c[2], " ")))
    for (i in 1:(length(items)-1)) {
        for (j in (i+1):(length(items)-1)) {
            if (items[i] + items[j] == credit) {
                output <- c(output, paste0("Case #", x, ": ", i, " ", j))
            }
        }
    }
}

writeLines(output, "large.txt")
