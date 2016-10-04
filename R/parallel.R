#' purrr::map-like function in parallel
#' @inheritParams purrr::map
#' @inheritParams foreach::foreach
#' @param .cores integer
#' @param .cluster type of parallel::makeCluster()
#' @return list
#' @rdname parallel
#' @export
map_par = function(.x, .f, ...,
    .combine, .multicombine=TRUE, .inorder=TRUE, .packages=NULL,
    .cores=parallel::detectCores(logical=FALSE),
    .cluster=c('FORK', 'PSOCK')) {

    .cores = min(.cores, length(.x))
    cluster = parallel::makeCluster(.cores, match.arg(.cluster), outfile='')
    on.exit(parallel::stopCluster(cluster))
    doParallel::registerDoParallel(cluster)
    foreach::foreach(args=.x, .combine=.combine, .multicombine=.multicombine, .inorder=.inorder, .packages=.packages) %dopar% {
        .f(args, ...)
    }
}

#' purrr::map_df-like function in parallel
#' @inheritParams purrr::map_df
#' @return tibble
#' @rdname parallel
#' @export
map_par_df = function(.x, .f, ..., .id=NULL,
    .multicombine=TRUE, .inorder=TRUE, .packages=NULL,
    .cores=parallel::detectCores(logical=FALSE),
    .cluster=c('FORK', 'PSOCK')) {

    .out = map_par(.x, .f, ...,
        .combine=dplyr::bind_rows, .multicombine=.multicombine,
        .inorder=.inorder, .packages=.packages,
        .cores=.cores, .cluster=.cluster)
    if (is.character(.id) && !is.null(names(.x))) {
        .out = mutate_left_(.out, stats::setNames(c(~names(.x)), .id))
    }
    .out
}

#' purrr::invoke-like function in parallel
#' @inheritParams purrr::invoke
#' @return list
#' @rdname parallel
#' @export
invoke_par = function(.f, .x, ..., .env=NULL,
    .combine, .multicombine=TRUE, .inorder=TRUE, .packages=NULL,
    .cores=parallel::detectCores(logical=FALSE),
    .cluster=c('FORK', 'PSOCK')) {

    .cores = min(.cores, length(.x))
    cluster = parallel::makeCluster(.cores, match.arg(.cluster), outfile='')
    on.exit(parallel::stopCluster(cluster))
    doParallel::registerDoParallel(cluster)
    foreach::foreach(args=.x, .combine=.combine, .multicombine=.multicombine, .inorder=.inorder, .packages=.packages) %dopar% {
        purrr::invoke(.f, args, ..., .env=.env)
    }
}
