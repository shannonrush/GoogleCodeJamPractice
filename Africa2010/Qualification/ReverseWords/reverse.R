Reverse <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- unlist(lapply(1:length(lines), function(l) paste0("Case #", l, ": ", paste(rev(unlist(strsplit(lines[l], " "))), collapse=" "))))
    writeLines(output, output.name)
}

# O(m*n)
# m <- num lines in input
# n <- num words in line

