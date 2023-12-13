tmp <- tempfile()
cat("???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
" , file = tmp)



library(tidyverse)

# find all patterns that use only `poss` and leave enough `poss`es 
# for the remaining `h`

# for each of those, repeat above

tibble(x = readLines(tmp)) %>%
  separate(x, into = c("chars", "hashes"), sep = " ") %>%
  mutate(s = strsplit(chars, ""),
         h = map(hashes, ~as.numeric(unlist(strsplit(.x, ",")))),
         poss = map(s, ~which(.x %in% c("?", "#"))),
         def = map(s, ~which(.x == "#"))
         )



# initial thoughts ----
parse_input <- function(x){
  readLines(x) |>
    strsplit(" ") |>
    Map(f = \(x) 
        list(
          chars = gsub("\\.", "X", unlist(strsplit(x[1], ""))), 
          hashes = as.numeric(unlist(strsplit(x[2], ",")))
        )
    )
}
test <- parse_input(tmp)


test
x <- test[[3]]
x

# won't catch _all_, but should remove a bunch of checks?

test_for_only_one <- function(x){
  
  hashes_and_necessary_dots <- sum(x$hashes)+length(x$hashes)-1 == length(x$chars)
  hashes_and_extant_dots <- sum(x$chars == "X") + sum(x$hashes) == length(x$chars)
  
  hashes_and_necessary_dots|hashes_and_extant_dots
}
test_for_only_one(x)

mapply(test_for_only_one, test)

find_first_hashes <- function(x){}

ch <- x$chars
h <- x$hashes

ch[1]
ch[2]
h[1]


ch[1]
