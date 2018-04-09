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

#' @param phy phylo object
#' @param rotate angle in radian
#' @rdname phylo
#' @export
ape_layout_unrooted = function(phy, rotate=0) {
  nodes = ape_layout_unrooted_nodes(phy, rotate = rotate)
  to_nodes = dplyr::rename(nodes, xend = "x", yend = "y")
  phy$edge %>%
    tibble::as_tibble() %>%
    stats::setNames(c("from", "to")) %>%
    dplyr::left_join(nodes %>% dplyr::select(-.data$axis), by = c(from = "id")) %>%
    dplyr::left_join(to_nodes, by = c(to = "id")) %>%
    dplyr::mutate(label = phy$tip.label[.data$to])
}

ape_layout_unrooted_nodes = function(phy, rotate=0) {
  num_tips = length(phy$tip.label)
  node_depth = ape::node.depth(phy)
  impl = function(parent) {
    edge_indices = which(phy$edge[, 1] == parent$id)
    START = parent$axis - parent$angle / 2
    this_df = NULL
    for (edge_i in edge_indices) {
      length_i = phy$edge.length[edge_i]
      child_df = tibble::tibble(
        id = phy$edge[edge_i, 2L],
        depth = node_depth[.data$id],
        angle = parent$angle * .data$depth / parent$depth,
        axis = START + .data$angle / 2,
        x = length_i * cos(.data$axis) + parent$x,
        y = length_i * sin(.data$axis) + parent$y,
      )
      START = START + child_df$angle
      this_df = dplyr::bind_rows(this_df, child_df)
      if (child_df$id > num_tips) {
        this_df = dplyr::bind_rows(this_df, impl(child_df))
      }
    }
    this_df
  }
  root = tibble::tibble(
    id = num_tips + 1L, x = 0, y = 0,
    angle = 2 * pi, axis = 0 + rotate, depth = node_depth[.data$id]
  )
  impl(root) %>%
    dplyr::bind_rows(root) %>%
    dplyr::mutate(axis = ifelse(.data$axis > pi, .data$axis - 2 * pi, .data$axis)) %>%
    dplyr::transmute(.data$id, .data$x, .data$y, .data$axis) %>%
    dplyr::arrange(.data$id)
}
