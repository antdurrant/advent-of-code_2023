# Cube Conundrum

# pull block out of a bag game

# get the highest number for each of the colours for each of the games


extract_pattern <- function(x, pattern){
  regmatches(x, gregexpr(pattern, x))
}

get_max_per_colour <- function(x, colour){
  extract_pattern(x, paste0("\\d+ ", colour)) |>
    sapply(\(x) max(as.integer(unlist(extract_pattern(x, "\\d+"))))) 
}

get_max_per_game <- function(x){
  Map(\(y) get_max_per_colour(x, y), c("blue", "green", "red"))
}

# part 1 ----
# how many of the games are possible with red<=12,green<=13,blue<=14?
part_1 <- function(x, r = 12, g = 13, b = 14){
  sum(which(x$red <= r & x$green <= g & x$blue <= b))
}

# part 2 ----
# sum of red*blue*green for each game
part_2 <- function(x){
  sum(x$red * x$blue * x$green)
}

# go go go
day_2 <- readLines(file.path("data", "day_2", "input"))
games <- get_max_per_game(day_2)
part_1(games)
part_2(games)
