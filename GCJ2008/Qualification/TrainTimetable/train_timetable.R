TrainTimetable <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines) > 0) {
        turnaround.time <- as.integer(lines[1])*60
        lines <- lines[-1]
        num.trips <- unlist(strsplit(lines[1]," "))    
        lines <- lines[-1]
        num.ab <- as.integer(num.trips[1])
        num.ba <- as.integer(num.trips[2])
        ab.trips <- TripsDF(head(lines, num.ab))
        lines <- tail(lines, length(lines)-num.ab)
        ba.trips <- TripsDF(head(lines, num.ba))
        lines <- tail(lines, length(lines)-num.ba)
        available.a <- c()
        available.b <- c()
        req.a <- 0
        req.b <- 0
        while (nrow(ab.trips)+nrow(ba.trips) > 0) {
            if (nrow(ba.trips)==0 || (nrow(ab.trips) > 0 & ab.trips[1,1] < ba.trips[1,1])) {
                if (any(available.a <= ab.trips[1,1])) {
                    available.a <- available.a[-max(which(available.a <= ab.trips[1,1]))]   
                } else {
                    req.a <- req.a + 1
                }
                available.b <- c(available.b, ab.trips[1,2]+turnaround.time)
                available.b <- available.b[order(available.b)]
                ab.trips <- ab.trips[-1,]
            } else {
                if (any(available.b <= ba.trips[1,1])) {
                    available.b <- available.b[-max(which(available.b <= ba.trips[1,1]))]   
                } else {
                    req.b <- req.b + 1
                }
                available.a <- c(available.a, ba.trips[1,2]+turnaround.time)
                available.a <- available.a[order(available.a)]
                ba.trips <- ba.trips[-1,]
            }
        }  
        output <- c(output, paste0("Case #", case, ": ", req.a, " ", req.b))
        case <- case + 1
    }
    writeLines(output, output.name)
}

TripsDF <- function(trips) {
    if (length(trips)==0) {
        return(data.frame(departures=c(),arrivals=c()))
    } else {
        times <- as.POSIXct(unlist(strsplit(trips, " ")), format="%H:%M")
        times.dept <- seq(1,length(trips)*2,2)
        trips <- data.frame(departures=times[times.dept], arrivals=times[-times.dept])
        return(trips[order(trips$departures),])
    }
}

# small: 265ms
# large: 4978ms