# transforming filters to be passed to ZarrArray$get_orthogonal_selection()
# 
# a:b => slice(a,b)
# seq(from, to, by) => slice(start, stop, step) ? for now indices of seq(from, to, by) are passed to get_orthogonal_selection (check below, TODO)
# c(a,b,c) => c(a,b,c), combine elements are passed as indices or boolean
# empty dimension => return everything
# 
manage_filters <- function(filters) {
  lapply(filters, function(x) {
    # Proceed based on type of filter
    if(typeof(x) == "symbol") {
      # When empty dimension, return everything
      if(x == "") {
        return(NULL)
      } else {
        # TODO: is eval() always the solution here ?, e.g. ind <- c(TRUE, FALSE, TRUE, TRUE) then eval(ind) if typeof(ind) == "symbol"
        return(eval(x))
      }
    } else if(typeof(x) == "double") {
      # Return single value for dimension
      return(slice(x, x))
    } else if(typeof(x) == "language") {
      x <- as.list(x)
      # Return a range (supplied via : or seq())
      if(x[[1]] == ":") {
        return(slice(x[[2]], x[[3]]))
      } else if(x[[1]] == "seq") {
        # TODO: do we need slicing for this case ? otherwise implement slice(start, stop, step)
        arg_names <- names(x)
        from <- ifelse("from" %in% arg_names, x[[which("from" == arg_names)]], x[[2]])
        to <- ifelse("to" %in% arg_names, x[[which("to" == arg_names)]], x[[3]])
        if(length(x) > 3) {
          by <- ifelse("by" %in% arg_names, x[[which("by" == arg_names)]], x[[4]])
          return(int(seq(from, to, by)))
        } else {
          by <- NA
          return(int(seq(from, to)))
        }
        return(int(seq(from, to, by)))
      } else if(x[[1]] == "c") {
        # return elements of the combine function as indices
        check_func <- sapply(x, function(y) {
          !is.function(eval(y))
        })
        x <- x[check_func]
        
        # correct for integer or boolean vectors
        x <- sapply(x, function(y) {
          if(is.numeric(y))
            return(int(floor(y))) 
          if(is_bool(y))
            return(y)
        })
        return(x)
      } else {
        stop("Unsupported filter '", as.character(x), "' supplied")
      }
    } else {
      stop("Unsupported filter '", as.character(x), "' supplied")
    }
  })
}