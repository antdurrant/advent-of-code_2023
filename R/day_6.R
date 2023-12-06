x <-
    "Time:      7  15   30
Distance:  9  40  200" |>
    readr::read_lines() 


str_extract_all <- \(x, pattern){
    m <- gregexpr(pattern, x)
    regmatches(x, m)
}

parse_input <- function(x){
    readLines(x) |>
        str_extract_all("\\d+") |>
        setNames(c("time", "distance")) |>
        mapply(FUN = as.integer)
}

# very simple
get_wins <- \(x, y) {
    times <- seq(x)*seq(from = (x-1), to = 0)
    length(times[times >y])
}

# integer overflow, so do a bit less
get_wins_2 <- \(time, distance){
    x <- 0
    res <- FALSE
    while(!res){
        x <- x + 1
        res <- x * (time-x) > distance
    }
    time-(2*x)+1
}

# GO
input <- parse_input("~/Downloads/input.txt")
# part 1 ----
prod(mapply(get_wins, input[,"time"], input[,"distance"]))


# part 2 ----
time <- as.integer(paste(input[,"time"], collapse = ""))
distance <- as.numeric(paste(input[,"distance"], collapse = ""))

get_wins_2(time, distance)
