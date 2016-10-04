#' purrr::map-like function in parallel
#' @inheritParams purrr::map
#' @inheritParams foreach::foreach
#' @param .cores integer
#' @return list
#' @rdname parallel
#' @export
map_par = function(.x, .f, ...,
    .combine, .multicombine=TRUE, .inorder=TRUE,
    .cores=parallel::detectCores(logical=FALSE)) {

    cluster = parallel::makeCluster(.cores)
    on.exit(parallel::stopCluster(cluster))
    doParallel::registerDoParallel(cluster)
    .pkgs = loadedNamespaces()
    foreach::foreach(args=.x, .combine=.combine, .multicombine=.multicombine, .inorder=.inorder, .packages=.pkgs) %dopar% {
        .f(args, ...)
    }
}

#' purrr::map_df-like function in parallel
#' @inheritParams purrr::map_df
#' @return tibble
#' @rdname parallel
#' @export
map_par_df = function(.x, .f, ..., .id=NULL,
    .multicombine=TRUE, .inorder=TRUE,
    .cores=parallel::detectCores(logical=FALSE)) {

    .out = map_par(.x, .f, ..., .combine=dplyr::bind_rows, .multicombine=.multicombine, .inorder=.inorder, .cores=.cores)
    if (is.character(.id)) {
        rownames(.out) = names(.x)
        .out = tibble::rownames_to_column(.out, .id)
    }
    .out
}

#' purrr::invoke-like function in parallel
#' @inheritParams purrr::invoke
#' @return list
#' @rdname parallel
#' @export
invoke_par = function(.f, .x, ..., .env=NULL,
    .combine, .multicombine=FALSE, .inorder=TRUE,
    .cores=parallel::detectCores(logical=FALSE)) {

    cluster = parallel::makeCluster(.cores)
    on.exit(parallel::stopCluster(cluster))
    doParallel::registerDoParallel(cluster)
    foreach::foreach(args=.x, .combine=.combine, .multicombine=.multicombine, .inorder=.inorder) %dopar% {
        purrr::invoke(.f, args, ..., .env=.env)
    }
}
