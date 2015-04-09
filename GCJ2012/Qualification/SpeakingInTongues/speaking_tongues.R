SpeakingTongues <- function(input, output.name="") {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    dict <- Dict()
    while (length(lines)>0) {
        answer <- paste(sapply(unlist(strsplit(lines[1], "")), function(c) dict[dict$input==c,"output"]),collapse="")
        output <- c(output, paste0("Case #", case, ": ", answer))
        lines <- lines[-1]
        case <- case + 1
    }
    writeLines(output,output.name)
}

Dict <- function() {
    input <- c("a","z","q"," ")
    output <- c("y","q","z"," ")
    dict <- data.frame(input, output, stringsAsFactors=F)
    sample.in <- c("ejp mysljylc kd kxveddknmc re jsicpdrysi",
                   "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd",
                   "de kr kd eoya kw aej tysr re ujdr lkgc jv")
    sample.out <- c("our language is impossible to understand",
                    "there are twenty six factorial possibilities",
                    "so it is okay if you want to just give up")
    for (i in 1:length(sample.in)) {
        s.in <- unlist(strsplit(sample.in[i],""))
        s.out <- unlist(strsplit(sample.out[i],""))
        for (j in 1:length(s.in)) {
            if (s.in[j]!=" " && !s.in[j] %in% dict$input) {
                dict <- rbind(dict, c(s.in[j], s.out[j]))
            }
        }
    }
    dict
}

# Unit: milliseconds
# expr      min       lq     mean   median       uq
# SpeakingTongues("A-small-practice.in", "tongues_small.out") 75.12295 78.62691 83.84715 80.29653 84.28321
# max neval
# 139.6686   100