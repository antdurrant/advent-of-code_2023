
x <- readLines("data/day_3/input")

# as individual characters
split_chars <- strsplit(x, "")
# part 1
# which ones are signs
signs_pt1 <- mapply(\(x) which(!x %in% c(0:9, ".")), split_chars)

# part 2
signs_pt2 <- mapply(\(x) which(x %in% "*"), split_chars)

# funs ----

# extract all the indices
indices <- \(x,y){
  if(x== -1) {
    NA
  } else {
    x:(x+(y-1))
  }
}

get_indices <- \(m) mapply(indices, m, attr(m, "match.length"), SIMPLIFY = FALSE)

# and add the positions for one row above or below to allow the diagonals
above_below <- \(x, n) {
  if(n==1) return(c(x[[n]], x[[(n+1)]]))
  if(n==length(x)) return(c(x[[n-1]], x[[n]]))
  c(x[[n-1]], x[[n]], x[[n+1]])
}

# find numbers that the sign would be adjacent to
which_numbers <- \(sign,num){mapply(\(x) any(x %in% sign), num)}

# which ones to keep
keep <- \(x, y)  x[y]


# globals ----
# where are the numbers and how long are they?
nums <- gregexpr("\\d+", x)

# each number occupies these positions
num_indices <- mapply(get_indices, nums)

# get the numbers in each line as actual numbers
nums_as_nums <-
  regmatches(x, nums) |>
  sapply(as.numeric)

# parts -----
part_1 <- function(num_indices, nums_as_nums, signs){
  sign_indices <- mapply(\(x) c(x-1, x, x+1), signs)

  # all the positions that mean a sign would be adjacent
  sign_inc_col <- mapply(\(x) above_below(sign_indices, x), seq_along(sign_indices))

  # what indices to keep
  keep_nums <- mapply(which_numbers, sign_inc_col, num_indices)

  # keep those indices
  adjacent_numbers <- mapply(keep, nums_as_nums, keep_nums)

  # part_1 -----
  sum(unlist(adjacent_numbers))
}


part_1(num_indices, nums_as_nums, signs = signs_pt1)


# part 2 ----
# find where 2 numbers are both adjacent to a * and multiply them
# then get the sum
# yeah ok, i can't be doing with this in base

library(tidyverse)

num_positions <-
  tibble(
    row = seq_along(num_indices),
    col = num_indices,
  ) %>%
  unnest(c(col)) %>%
  group_by(row) %>%
  mutate(group_in_row = row_number()) %>%
  ungroup() %>%
  unnest(col) %>%
  filter(!is.na(col))

sign_positions <-
  tibble(
    row = seq_along(signs),
    col = signs
  ) %>%
  unnest(col) %>%
  mutate(ind = map2(row, col, ~expand_grid(row = c(.x-1, .x, .x+1), col = c(.y-1, .y, .y+1))),
         sign_group = row_number()
  ) %>%
  select(-row, -col) %>%
  unnest(ind) %>%
  inner_join(num_positions)


tibble(row = seq_along(nums_as_nums),
       number = nums_as_nums) %>%
  unnest(number) %>%
  group_by(row) %>%
  mutate(group_in_row = row_number()) %>%
  ungroup() %>%
  inner_join(sign_positions) %>%
  distinct(sign_group, group_in_row, number) %>%
  group_by(sign_group) %>%
  filter(n() > 1) %>%
  summarise(number = prod(number)) %>%
  ungroup() %>%
  summarise(sum(number)) %>%
  pull()

