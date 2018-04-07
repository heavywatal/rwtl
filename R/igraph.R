#' Utilities for igraph class
#' @inheritParams igraph::layout_
#' @return tibble
#' @rdname igraph
#' @export
igraph_layout = function(graph, layout = igraph::nicely(), ...) {
  nodes = igraph_layout_nodes(graph)
  to_nodes = dplyr::rename(nodes, xend = "x", yend = "y")
  igraph::as_data_frame(graph, "edges") %>%
    tibble::as_tibble() %>%
    dplyr::left_join(nodes, by = c(from = "name")) %>%
    dplyr::left_join(to_nodes, by = c(to = "name"))
}

igraph_layout_nodes = function(graph, layout = igraph::nicely(), ...) {
  igraph::layout_(graph, layout, ...) %>%
    tibble::as_tibble() %>%
    stats::setNames(c("x", "y")) %>%
    dplyr::mutate(name = igraph::V(graph)$name)
}
