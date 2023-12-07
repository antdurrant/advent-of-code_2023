# why isn't this a base function?
str_extract_all <- \(x, pattern){
    m <- gregexpr(pattern, x)
    regmatches(x, m)
}
# gives a matrix
parse_input <- function(x){
    readLines(x) |>
        str_extract_all("\\d+") |>
        setNames(c("time", "distance")) |>
        mapply(FUN = as.integer)
}

# very simple brute force
get_wins <- \(x, y) {
    times <- seq(x)*seq(from = (x-1), to = 0)
    length(times[times >y])
}
# above gets warning:
# *integer overflow results in some NAs* 
# so do a bit less
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
