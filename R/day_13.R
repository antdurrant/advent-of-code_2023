# make a list on matrices ----
parse_input <- function(input){
    # smoosh together
    readLines(input) |>
        paste(collapse = "\n") |>
        strsplit("\n\n") |>
        unlist() |>
        # make the matrices
        strsplit("\n") |>
        Map(
            f = \(x) strsplit(x, "") |>
                sapply(as.matrix)
        )
}

# two types of comparison ----
# part 1 is identical
# part 2 has a single difference
compare <- function(m, n, part){
    a = m[seq(n),]
    b = m[seq(from = n*2, to = n+1),]
    switch(
        part,
        part_1 = identical(a, b),
        part_2 = sum(a != b) == 1
    )
}

# reverse a matrix ----
rev_mat <- function(mat){
    mat[rev(seq(nrow(mat))),]
}

# check for symmetry on one axis ----
.get_symmetry <- function(mat, part, rc = c("row", "col")){
    if(rc == "row"){
        ind <- nrow(mat)/2
    } else if(rc == "col"){
        # transpose the matrix for other axis
        ind <- ncol(mat)/2
        mat <- t(mat)
    }
    
    res <- 
        c( # check left-to-right
            which(mapply(\(x) compare(m = mat, n = x, part), seq(ind))),
            # check right-to-left
            # back-to-front needs adjusting to give the actual index
            ind*2 - which(mapply(\(x) compare(m = rev_mat(mat), n = x, part), seq(ind)))
        )
    if(rc == "col") res*100 else res
}

# check for symmetry on both axes ----
get_symmetry <- function(mat, part){
    unlist(mapply(\(x) .get_symmetry(mat, part, x), c("row", "col")))
}

# sum the symmetries ----
day_13 <- function(x, part){
    sum(mapply(\(x) get_symmetry(x, part), x))
}

# do it ----
input <- parse_input("data/day_13/input")
day_13(input, "part_1")
day_13(input, "part_2")
