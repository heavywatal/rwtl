#' Utilities for phylo class of ape package
#' @param x phylo object
#' @rdname phylo
#' @export
node_labels = function(x) {
  num_tips = length(x$tip.label)
  labels = as.character(seq_len(num_tips + x$Nnode))
  labels[seq_len(num_tips)] = x$tip.label
  labels
}

#' @rdname phylo
#' @export
named_edges = function(x) {
  matrix(node_labels(x)[x$edge], ncol = 2L)
}

#' @rdname phylo
#' @export
phylo2igraph = function(x) {
  named_edges(x) %>%
    igraph::graph_from_edgelist() %>%
    igraph::set_edge_attr("weight", value = x$edge.length)
}
