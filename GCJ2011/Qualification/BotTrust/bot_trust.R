BotTrust <- function(input, output.name="") {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines)>0) {
        l <- unlist(strsplit(lines[1]," "))[-1]
        lines <- lines[-1]
        output <- c(output, paste0("Case #", case, ": ", TestSeconds(l)))
        case <- case + 1
    }
    writeLines(output, output.name)
}

TestSeconds <- function(seq) {
    seconds <- 0
    O.pos <- 1
    B.pos <- 1
    Os <- which(seq=="O")
    Bs <- which(seq=="B")
    O.move <- GetMove(seq, Os)
    B.move <- GetMove(seq, Bs)
    while ((length(Os)+length(Bs))>0) {
        if (length(Bs)==0 || (length(Os)>0 && Os[1] < Bs[1])) {
            Os <- Os[-1]
            while (O.move != O.pos) {
                O.pos <- ifelse(O.move > O.pos, O.pos+1, O.pos-1)
                if (B.move > 0 && B.move != B.pos) B.pos <- ifelse(B.move > B.pos, B.pos+1, B.pos-1)
                seconds <- seconds + 1
            }
            # push button
            if (B.move > 0 && B.move != B.pos) B.pos <- ifelse(B.move > B.pos, B.pos+1, B.pos-1)
            seconds <- seconds + 1
            O.move <- GetMove(seq, Os)
        } else {
            Bs <- Bs[-1]
            while (B.move != B.pos) {
                B.pos <- ifelse(B.move > B.pos, B.pos+1, B.pos-1)
                if (O.move > 0 && O.move != O.pos) O.pos <- ifelse(O.move > O.pos, O.pos+1, O.pos-1)
                seconds <- seconds + 1
            }
            # push button
            if (O.move > 0 && O.move != O.pos) O.pos <- ifelse(O.move > O.pos, O.pos+1, O.pos-1)
            seconds <- seconds + 1
            B.move <- GetMove(seq, Bs)
        }            
    }
    seconds
}

GetMove <- function(seq, moves) {
    ifelse(length(moves)>0, as.integer(seq[moves[1]+1]), 0)
}

# Unit: milliseconds
# expr       min        lq      mean    median        uq       max
# BotTrust("A-small-practice.in", "bot_small.out")  16.78178  17.91331  19.32124  18.90182  19.59009  51.83975
# BotTrust("A-large-practice.in", "bot_large.out") 855.00032 875.52269 890.19392 888.06230 900.01504 939.67003
# neval
# 100
# 100