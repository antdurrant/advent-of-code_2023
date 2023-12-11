
parse_input <- function(path){
  # matrix of input
  m <-
    path |>
    readLines() |>
    strsplit("") |>
    sapply( \(x) x == "#") 
  
  # which cols/rows are empty?
  add_cols <- which(apply(m, 2, \(x) !any(x)))
  add_rows <- which(apply(m, 1, \(x) !any(x)))
  
  # where are the stars?
  stars <- 
    m |>
    which(arr.ind = TRUE) |>
    as.data.frame()
  stars$id = row.names(stars)
  
  list(stars = stars, add_cols = add_cols, add_rows = add_rows)
}



day_11 <- function(data, add_val){
  
  stars <- data$stars
  add_cols <- data$add_cols
  add_rows <- data$add_rows
  
  tidyr::expand_grid(
    # get star pairs
    dplyr::rename_with(stars, ~paste0(.x, "_1")), 
    dplyr::rename_with(stars, ~paste0(.x, "_2"))
  ) |>
    # no duplicates, please!
    dplyr::filter(id_1 != id_2) |>
    dplyr::mutate(ids = purrr::map2_chr(id_1, id_2, ~paste0(sort(c(.x,.y)), collapse = ","))) |>
    dplyr::distinct(ids, .keep_all = TRUE) |>
    dplyr::mutate(
      # how many expansions in each row/column
      x_add = purrr::pmap_int(list(row_1, row_2), ~sum(add_rows > min(..1, ..2) & add_rows < max(..1, ..2))),
      y_add = purrr::pmap_int(list(col_1, col_2), ~sum(add_cols > min(..1, ..2) & add_cols < max(..1, ..2))),
      # calculate distance factoring in these expansions
      x = abs(row_2-row_1)+(add_val*x_add)-x_add,
      y = abs(col_2-col_1)+ (add_val*y_add)-y_add,
    ) |>  
    # get distance by pair
    dplyr::group_by(ids) |>
    dplyr::summarise(dist = x+y)  |> 
    dplyr::pull(dist) |>
    # add them all together
    sum()
}

day_11_input <- parse_input("data/day_11/input")

# part 1 ----
day_11(day_11_input, add_val = 2)

# part 2
day_11(day_11_input, add_val = 1e6)

