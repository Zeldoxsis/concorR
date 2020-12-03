#CONCOR supplementary functions
#Tyme Suda


.blk_apply <- function(iobject, split, v = "cat") {
  o <- match(igraph::vertex.attributes(iobject)$name, split$vertex)
  o_block <- split$block[o]
  blk_return <- igraph::set.vertex.attribute(iobject, v, value = o_block)
  return(blk_return)
}

concor_make_igraph <- function(adj_list, nsplit = 1) {
  adj_list <- .concor_validitycheck(adj_list)
  concor_out <- suppressWarnings(concor(adj_list, nsplit))

  igraph_list <- lapply(adj_list, function(x) igraph::graph_from_adjacency_matrix(x))
  v <- paste("csplit", nsplit, sep = "")
  igraph_out <- lapply(igraph_list, function(x) .blk_apply(x, concor_out, v))

  return(igraph_out)
}

.name_igraph <- function(iobject) {
  l <- length(V(iobject))
  lvec <- 1:l
  n_zero <- floor(log10(l))+1
  num_list <- formatC(lvec, width = n_zero, format = "d", flag = "0")
  v <- paste0("V", num_list)
  vertex.attributes(iobject)$name <- v
  return(iobject)
}

concor_igraph_apply <- function(igraph_list, nsplit = 1) {
  b <- sapply(igraph_list, function(x) is.null(vertex.attributes(x)$name))
  if (all(b)) {
    warning("node names don't exist\nAdding default node names\n")
    igraph_list <- lapply(igraph_list, .name_igraph)
  }
  adj_list <- lapply(igraph_list, function(x) igraph::get.adjacency(x, sparse = FALSE))

  concor_out <- suppressWarnings(concor(adj_list, nsplit))
  v <- paste("csplit", nsplit, sep = "")
  igraph_out <- lapply(igraph_list, function(x) .blk_apply(x, concor_out, v))

  return(igraph_out)
}

concor_plot <- function(iobject, nsplit = NULL, vertex.label = NA, vertex.size = 5, edge.arrow.size = .3) {
  split_name <- paste0("csplit", nsplit)
  igraph::plot.igraph(iobject, vertex.color = igraph::vertex.attributes(iobject)[[split_name]],
                      vertex.label = vertex.label, vertex.size = vertex.size, edge.arrow.size = edge.arrow.size)
}

