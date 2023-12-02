# https://adventofcode.com/2023/day/1

# Trebuchet

# goddamned kids, drawing all over the docs

#  part 1
# get the first and last numbers from each line as a two digit number
# add all the numbers

day_1 <- readLines("data/day_1/input")

# part 1 ----
 day_1 |>
  gsub("([^1-9])", "", x =_) |>
  strsplit("") |>
  sapply(\(x) as.integer(paste0(head(x,1), tail(x,1))))|>
  sum()


# part 2 ----
# some of these numbers are actually letters

## fns -----
str_rev <- function(x) {
  strsplit(x, "") |>
    sapply(\(x)  paste(rev(x), collapse = ""))
}

extract_pattern <- function(x, pattern){
  m <- regexpr(pattern, x)
  regmatches(x, m)
}

make_digits <- function(x){
  numbers <-
    c(
      one = "1",
      two = "2",
      three = "3",
      four = "4",
      five = "5",
      six = "6",
      seven = "7",
      eight = "8",
      nine = "9"
    )

  for(i in seq_along(numbers)){
    x <- sub(pattern = names(numbers)[i], numbers[i], x)
  }
  x
}

number_string <- paste(c("one|two|three|four|five|six|seven|eight|nine", 1:9), collapse = "|")

part_2 <- function(x = day_1, pattern = number_string){

  first_nums <- extract_pattern(x, pattern)
  last_nums <- str_rev(extract_pattern(str_rev(x), str_rev(pattern)))

  paste0(
    make_digits(first_nums),
    make_digits(last_nums)
  ) |>
    as.integer() |>
    sum()
}
part_2()

# works for part 1 as well, just a bit slower
part_2(num_str = paste(1:9, collapse = "|"))

