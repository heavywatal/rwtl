#' purrr::map-like function in parallel
#' @inheritParams purrr::map
#' @inheritParams foreach::foreach
#' @param .mc.cores integer
#' @param .cluster type of parallel::makeCluster()
#' @return list
#' @rdname parallel
#' @export
map_par = function(.x, .f, ..., .combine, .multicombine=TRUE,
                   .inorder=TRUE, .packages=NULL, .export=NULL,
                   .mc.cores=getOption("mc.cores", 2L),
                   .cluster=c("FORK", "PSOCK"),
                   .errorhandling=c("stop", "remove", "pass")) {
  if (is.function(.f)) {
    .fun = purrr::partial(.f, ...)
  } else {
    .fun = purrr::as_mapper(.f, ...)
  }
  .mc.cores = min(.mc.cores, length(.x))
  cluster = parallel::makeCluster(.mc.cores, match.arg(.cluster), outfile = "")
  on.exit(parallel::stopCluster(cluster))
  doParallel::registerDoParallel(cluster)
  x = NULL # to suppress warning
  foreach::foreach(x = .x, .combine = .combine, .multicombine = .multicombine, .inorder = .inorder, .packages = .packages, .export = .export, .errorhandling = .errorhandling) %dopar% {
    .fun(x)
  } %>% stats::setNames(names(.x))
}

#' purrr::map_dfr-like function in parallel
#' @inheritParams purrr::map_dfr
#' @return tibble
#' @rdname parallel
#' @export
map_par_dfr = function(.x, .f, ..., .id=NULL, .multicombine=TRUE,
                       .inorder=TRUE, .packages=NULL, .export=NULL,
                       .mc.cores=getOption("mc.cores", 2L),
                       .cluster=c("FORK", "PSOCK"),
                       .errorhandling=c("stop", "remove", "pass")) {
  if (!missing(.id) && !.inorder) {
    warning(".id is ignored because .inorder is FALSE")
    .id = NULL
  }
  res = map_par(
    .x, .f, ...,
    .multicombine = .multicombine, .inorder = .inorder, .packages = .packages, .export = .export,
    .mc.cores = .mc.cores, .cluster = .cluster, .errorhandling = .errorhandling
  )
  dplyr::bind_rows(res, .id = .id)
}

#' purrr::invoke-like function in parallel
#' @inheritParams purrr::invoke
#' @return list
#' @rdname parallel
#' @export
invoke_par = function(.f, .x, ..., .env=NULL, .combine, .multicombine=TRUE,
                      .inorder=TRUE, .packages=NULL, .export=NULL,
                      .mc.cores=getOption("mc.cores", 2L),
                      .cluster=c("FORK", "PSOCK")) {
  .mc.cores = min(.mc.cores, length(.x))
  cluster = parallel::makeCluster(.mc.cores, match.arg(.cluster), outfile = "")
  on.exit(parallel::stopCluster(cluster))
  doParallel::registerDoParallel(cluster)
  foreach::foreach(args = .x, .combine = .combine, .multicombine = .multicombine, .inorder = .inorder, .packages = .packages, .export = .export) %dopar% {
    purrr::invoke(.f, args, ..., .env = .env)
  }
}

#' @return list
#' @rdname parallel
#' @export
mcmap = function(.x, .f, ..., .mc.cores=getOption("mc.cores", 2L)) {
  if (rlang::is_formula(.f)) {
    .f = rlang::as_function(.f)
  }
  parallel::mclapply(.x, .f, ..., mc.cores = .mc.cores)
}

#' @rdname parallel
#' @export
mcmap_lgl = function(.x, .f, ..., .mc.cores=getOption("mc.cores", 2L)) {
  purrr::as_vector(mcmap(.x, .f, ..., .mc.cores = .mc.cores), logical(1L))
}

#' @rdname parallel
#' @export
mcmap_int = function(.x, .f, ..., .mc.cores=getOption("mc.cores", 2L)) {
  purrr::as_vector(mcmap(.x, .f, ..., .mc.cores = .mc.cores), integer(1L))
}

#' @rdname parallel
#' @export
mcmap_dbl = function(.x, .f, ..., .mc.cores=getOption("mc.cores", 2L)) {
  purrr::as_vector(mcmap(.x, .f, ..., .mc.cores = .mc.cores), double(1L))
}

#' @rdname parallel
#' @export
mcmap_chr = function(.x, .f, ..., .mc.cores=getOption("mc.cores", 2L)) {
  purrr::as_vector(mcmap(.x, .f, ..., .mc.cores = .mc.cores), character(1L))
}

#' @rdname parallel
#' @export
mcmap_dfr = function(.x, .f, ..., .id=NULL, .mc.cores=getOption("mc.cores", 2L)) {
  dplyr::bind_rows(mcmap(.x, .f, ..., .mc.cores = .mc.cores), .id = .id)
}
