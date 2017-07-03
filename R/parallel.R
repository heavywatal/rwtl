#' purrr::map-like function in parallel
#' @inheritParams purrr::map
#' @inheritParams foreach::foreach
#' @param .cores integer
#' @param .cluster type of parallel::makeCluster()
#' @return list
#' @rdname parallel
#' @export
map_par = function(.x, .f, ...,
    .combine, .multicombine=TRUE, .inorder=TRUE, .packages=NULL, .export=NULL,
    .cores=parallel::detectCores(logical=FALSE),
    .cluster=c('FORK', 'PSOCK'),
    .errorhandling=c('stop', 'remove', 'pass')) {

    if (is.function(.f)) {
        .fun = purrr::partial(.f, ...)
    } else {
        .fun = purrr::as_function(.f, ...)
    }
    .cores = min(.cores, length(.x))
    cluster = parallel::makeCluster(.cores, match.arg(.cluster), outfile='')
    on.exit(parallel::stopCluster(cluster))
    doParallel::registerDoParallel(cluster)
    x = NULL # to suppress warning
    foreach::foreach(x=.x, .combine=.combine, .multicombine=.multicombine, .inorder=.inorder, .packages=.packages, .export=.export, .errorhandling=.errorhandling) %dopar% {
        .fun(x)
    } %>% stats::setNames(names(.x))
}

#' purrr::map_df-like function in parallel
#' @inheritParams purrr::map_df
#' @return tibble
#' @rdname parallel
#' @export
map_par_df = function(.x, .f, ..., .id=NULL,
    .multicombine=TRUE, .inorder=TRUE, .packages=NULL, .export=NULL,
    .cores=parallel::detectCores(logical=FALSE),
    .cluster=c('FORK', 'PSOCK'),
    .errorhandling=c('stop', 'remove', 'pass')) {

    if (!missing(.id) && !.inorder) {
        warning('.id is ignored because .inorder is FALSE')
        .id = NULL
    }
    res = map_par(.x, .f, ...,
        .multicombine=.multicombine, .inorder=.inorder, .packages=.packages, .export=.export,
        .cores=.cores, .cluster=.cluster, .errorhandling=.errorhandling)
    dplyr::bind_rows(res, .id=.id)
}

#' purrr::invoke-like function in parallel
#' @inheritParams purrr::invoke
#' @return list
#' @rdname parallel
#' @export
invoke_par = function(.f, .x, ..., .env=NULL,
    .combine, .multicombine=TRUE, .inorder=TRUE, .packages=NULL, .export=NULL,
    .cores=parallel::detectCores(logical=FALSE),
    .cluster=c('FORK', 'PSOCK')) {

    .cores = min(.cores, length(.x))
    cluster = parallel::makeCluster(.cores, match.arg(.cluster), outfile='')
    on.exit(parallel::stopCluster(cluster))
    doParallel::registerDoParallel(cluster)
    foreach::foreach(args=.x, .combine=.combine, .multicombine=.multicombine, .inorder=.inorder, .packages=.packages, .export=.export) %dopar% {
        purrr::invoke(.f, args, ..., .env=.env)
    }
}
