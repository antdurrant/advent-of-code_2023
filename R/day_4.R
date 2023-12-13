x <- "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
y <- readr::read_lines(x)
y <- readLines("data/day_4/input")
# part 1 ---
parse_cards <- function(cards = y){
  gsub("Card \\d+: +", "", x = cards) |>
    strsplit(" +\\| +") |>
    Map(f = \(x) strsplit(x, " +") |> Map(f = as.numeric)) |>
    sapply(\(x) intersect(x[[1]], x[[2]]))
}

simple_power_2 <-function(x){
  if(length(x) == 0){
    0
  }  else{
    2^(length(x)-1)
  }
}

part_1 <- function(parsed_cards){
  sapply(parsed_cards, \(x) sum(simple_power_2(x)))
}


parsed_cards <- parse_cards(y)
sum(part_1(parsed_cards))

# part 2 ----
library(tidyverse)

part_2_ <- function(parsed_cards){

  card_counts <-
    data.frame(
      card = seq_along(parsed_cards),
      n = 1
    )

  card_content <-
    tibble::tibble(
      card = seq_along(parsed_cards),
      content =
        map2(
          card,
          parsed_cards,
          ~seq_along(.y)+.x
        )
    ) %>%
    unnest(content)

  add_cards <- function(card_counts, c){
    card_counts %>%
      bind_rows(
        card_content %>%
          filter(card == c) %>%
          left_join(card_counts, by = "card") %>%
          select(card = content, n)
      ) %>%
      count(card, wt = n) %>%
      filter(card <= length(parsed_cards))
  }

  total_cards <-
    Reduce(
      add_cards,
      seq_along(parsed_cards),
      init = card_counts
    )

  sum(total_cards$n)
}

part_2_(parsed_cards)

