# helper functions ----
hash <- \(curr, x) ((curr+utf8ToInt(x))*17 ) %% 256
algo <- \(x) Reduce(hash, x , init = 0)
focus_power <- \(x, n) seq_along(x)*x*n

parse_input <- \(path){
    readChar(f, file.size(f)) |>
        gsub("\\n", "", x = _) |>
        strsplit(",") |>
        unlist()
}


part_1 <- \(o){
    o |>
        strsplit("") |>
        sapply(algo) |>
        sum()
}


part_2 <- \(s){
    
    values <- sapply( strsplit(s, "=|-"), \(x) as.numeric(x[2]) |> setNames(x[1]))
    actions <- gsub("[^=-]", "", s) 
    boxes <- paste("box", sapply(strsplit(names(vals), ""), algo))
    container <- mapply(\(x) list(numeric()), paste("box", 0:255))
    
    for(i in seq_along(vals)){
        v <- names(values[i]);b <- boxes[i]; a <- actions[i];
        
        if(a == "=") container[[b]][v] <- vals[i]
        if(a == "-") container[[b]] <- subset(container[[b]], names(container[[b]]) != v)
    }
    sum(unlist(mapply(focus_power, container, seq_along(container))))
}

day_15 <- parse_input("data/day_15/input")

part_1(day_15)
part_2(day_15)

       
