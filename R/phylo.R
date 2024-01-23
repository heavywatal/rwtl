#' Utilities for phylo class of ape package
#' @param x phylo object
#' @rdname phylo
#' @export
node_labels = function(x) {
  num_tips = length(x[["tip.label"]])
  labels = as.character(seq_len(num_tips + x[["Nnode"]]))
  labels[seq_len(num_tips)] = x[["tip.label"]]
  labels
}

#' @rdname phylo
#' @export
named_edges = function(x) {
  matrix(node_labels(x)[x[["edge"]]], ncol = 2L)
}

#' @param phy phylo object
#' @param centering boolean
#' @param rotate angle in radian
#' @rdname phylo
#' @export
ape_layout_unrooted = function(phy, centering = TRUE, rotate = 0) {
  nodes = ape_unrooted_xy(phy, rotate = rotate)
  if (centering) {
    nodes = center_range(nodes, c("x", "y"))
  }
  to_nodes = dplyr::rename(nodes, xend = "x", yend = "y")
  as.data.frame(phy[["edge"]]) |>
    tibble::new_tibble() |>
    stats::setNames(c("from", "to")) |>
    dplyr::left_join(nodes |> dplyr::select(!"axis"), by = c(from = "id")) |>
    dplyr::left_join(to_nodes, by = c(to = "id")) |>
    dplyr::mutate(label = phy[["tip.label"]][.data[["to"]]])
}

# The algorithm was originally implemented in ape:::unrooted.xy()
# https://cran.r-project.org/package=ape
ape_unrooted_xy = function(phy, rotate = 0) {
  node_depth = ape::node.depth(phy)
  edge_from = phy[["edge"]][, 1L]
  edge_to = phy[["edge"]][, 2L]
  edge_length = phy[["edge.length"]]
  impl = function(parent) {
    edge_indices = which(edge_from == parent[["id"]])
    AXIS_BASE = parent[["axis"]] - parent[["angle"]] / 2
    output = NULL
    for (edge_i in edge_indices) {
      length_i = edge_length[edge_i]
      child_df = tibble::new_tibble(tibble::lst(
        id = edge_to[edge_i],
        depth = node_depth[.data$id],
        angle = parent$angle * .data$depth / parent$depth,
        axis = AXIS_BASE + .data$angle / 2,
        x = length_i * cos(.data$axis) + parent[["x"]],
        y = length_i * sin(.data$axis) + parent[["y"]]
      ))
      AXIS_BASE = AXIS_BASE + child_df[["angle"]]
      output = dplyr::bind_rows(output, child_df, impl(child_df))
    }
    output
  }
  root = tibble::new_tibble(tibble::lst(
    id = which.max(node_depth), x = 0, y = 0,
    angle = 2 * pi, axis = rotate, depth = node_depth[.data$id]
  ))
  impl(root) |>
    dplyr::bind_rows(root) |>
    dplyr::mutate(axis = ifelse(.data$axis > pi, .data$axis - 2 * pi, .data$axis)) |>
    dplyr::transmute(.data$id, .data$x, .data$y, .data$axis) |>
    dplyr::arrange(.data$id)
}
