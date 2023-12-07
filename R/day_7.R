# make fake poker

# what kind of hand is it
# x will be the counts of each card
.get_kind <- function(x){
  if(max(x) == 5) return("five")
  if(max(x) == 4) return("four")
  if(all(2:3 %in% x)) return("full_house")
  if(max(x) == 3) return("three")
  if(sum(x == 2) == 2) return("two_pair")
  if(max(x) == 2) return("one_pair")
  if(all(x == 1)) return("high")
}

# if we need to add a joker
.add_joker <- function(x, joker = "J") {
  if(joker %in% names(x)) {
    # all J case
    if(length(x) == 1) return(x)
    
    # add J count to highest non-J
    y <- x[names(x) != joker]
    x[names(which.max(y))] <- x[names(which.max(y))] + x[joker]
    x[joker] <- 0
  }
  x
}

# get kind for all hands
get_kind <- function(x, joker = NULL) {
  if(!is.null(joker)) x <- Map(.add_joker, x, joker = joker)
  res <- Map(.get_kind, x)
  ordered(res, levels = c("five", "four", "full_house", "three", "two_pair", "one_pair", "high"))
}


day_7 <- function(path = "~/Downloads/7input.txt" , joker = NULL){
  # one hand & bind per line
  input <- readLines(path)
  
  levels <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
  # joker is last if it exists
  if(!is.null(joker)) levels <- c(levels[levels != joker], joker)
  
  # keep the hands not the bids
  hands <- gsub(" .+", "", input)
  # get each card
  cards <- strsplit(hands, "")
  # keep the bids, not the hands
  bids <- as.numeric(gsub("[[:alnum:]]+ ", "", input))
  # joker or not
  kinds <- get_kind(mapply(table, cards), joker = joker)
  
  tibble::tibble(hands, cards, bids, kinds)|>
    # each card is a column
    tidyr::unnest_wider(cards, names_sep = "_") |>
    # make them ordered (enums)
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("cards_"),
        ~ordered(.x, levels = rev(levels)))
    ) |>
    # put them in order
    dplyr::arrange(dplyr::desc(kinds), cards_1, cards_2, cards_3, cards_4, cards_5) |>
    dplyr::mutate(score = bids * dplyr::row_number()) |>
    dplyr::pull(score) |>
    sum()
}
# part 1
day_7()
# part 2
day_7(joker = "J")
