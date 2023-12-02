# Cube Conundrum

# pull block out of a bag game

# get the highest number for each of the colours for each of the games

# part 1 ----
# how many of the games are possible with red<=12,green<=13,blue<=14?


extract_pattern <- function(x, pattern){
  m <- gregexpr(pattern, x)
  regmatches(x, m)
}

get_max_per_colour <- function(x, colour){
  extract_pattern(x, paste0("\\d+ ", colour)) |>
    Map(f = \(x) extract_pattern(x, "\\d+")) |>
    Map(f = \(x) as.integer(unlist(x)))|>
    sapply(max)
}

get_max_per_game <- function(x){
  data.frame(
    blue = get_max_per_colour(x, "blue"),
    green = get_max_per_colour(x, "green"),
    red = get_max_per_colour(x, "red")
  )
}

part_1 <- function(x){
  sum(which(x$red <= 12 & x$green <= 13 & x$blue <= 14))
}

# part 2 ----
# sum of red*blue*green for each game
part_2 <- function(x){
  sum(x$red * x$blue * x$green)
}

games <- get_max_per_game(day_2)
part_1(games)
part_2(games)

