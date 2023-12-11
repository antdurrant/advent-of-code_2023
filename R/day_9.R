parse_input <- function(x){
  readLines(x) |>
    strsplit(" +") |>
    sapply(as.integer, simplify = FALSE)
}

get_next_value <- function(x, current_val = NULL){
  # first call should have no value, so take the last value of x
  if(is.null(current_val)) current_val = tail(x,1)
  
  # get diffs
  d <- diff(x) 
  
  # return if diffs ares all the same
  if(length(unique(d)) == 1){ 
    tail(d, 1) + current_val
  } else{
    # we go again (keeping track of our additions)
    Recall(d, current_val = (current_val+tail(d, 1)))
  }
}


day_9 <- parse_input("~/Downloads/9input.txt")
part_1 <- function(x){
  sum(sapply(x, get_next_value))
}


part_1(day_9)
part_1(Map(rev, day_9))
