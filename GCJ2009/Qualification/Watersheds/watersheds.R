Watersheds <- function(input, output.name) {
    lines <- readLines(input)[-1]
    output <- c()
    case <- 1
    while (length(lines) > 0) {
        dims <- as.integer(unlist(strsplit(lines[1]," ")))    
        rows <- dims[1]
        cols <- dims[2]
        lines <- lines[-1]
        map <- matrix(as.integer(unlist(strsplit(lines[1:rows]," "))), nrow=rows, ncol=cols, byrow=T)
        lines <- lines[-c(1:rows)]
        watershed.map <- WatershedMap(map)
        output <- c(output, paste0("Case #", case, ":"))
        for (i in 1:nrow(watershed.map)) {
            output <- c(output, paste(watershed.map[i,], collapse=" "))
        }
        case <- case + 1
    }
    writeLines(output, output.name)
}

WatershedMap <- function(map) {
    w <- matrix(nrow=nrow(map), ncol=ncol(map))
    letter <- 1
    for (i in 1:nrow(map)) {
        for (j in 1:ncol(map)) {
            if (is.na(w[i,j])) {
                w <- FollowBasin(w, map, i, j, letters[letter])
                if (w[i,j]==letters[letter] && letter<26) letter <- letter+1
            }
        }
    }
    w
}

FollowBasin <- function(w, map, i, j, letter) {
    if (w[i,j] %in% letters) {
        # ran into another basin
        # replace all 0 in w with w[i,j] and return w
        w[w==0] <- w[i,j]
        return(w)
    } else {
        # mark current spot in the trail
        w[i,j] <- 0
        # find where it flows
        adj <- c(N(i,j,map), W(i,j,map), E(i,j,map), S(i,j,map))
        # is this spot a sink?
        if (all(adj>=map[i,j], na.rm=T)) {
            # this is a new basin, mark all 0 with letter and return w
            w[w==0] <- letter
            return(w)
        } else {
            # where does it flow
            dir <- which(adj==(min(adj, na.rm=T)))[1]
            if (dir==1) {
                i <- i-1
            } else if (dir==2) {
                j <- j-1
            } else if (dir==3) {
                j <- j+1
            } else {
                i<-i+1
            }
            FollowBasin(w, map, i, j, letter)
        }
    }
}

N <- function(i,j,map) {
    if (i==1) {
        return(NA)
    } else {
        return(map[i-1,j])
    }
}

W <- function(i,j,map) {
    if (j==1) {
        return(NA)
    } else {
        return(map[i,j-1])
    }
}

E <- function(i,j,map) {
    if (j==ncol(map)) {
        return(NA)
    } else {
        return(map[i,j+1])
    }
}

S <- function(i,j,map) {
    if (i==nrow(map)) {
        return(NA)
    } else {
        return(map[i+1,j])
    }
}

# Unit: milliseconds
# expr        min         lq       mean     median         uq       max neval
# Watersheds("B-small-practice.in", "watershed_small.out")   84.64415   87.43381   92.43978   88.71614   91.91512  180.8293   100
# Watersheds("B-large-practice.in", "watershed_large.out") 7981.70468 8143.03761 8226.95997 8197.20934 8303.98938 8551.2928   100