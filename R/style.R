#' tidyverse_style but use equal sign = for assignment
#' @inheritParams styler::tidyverse_style
#' @rdname style
#' @export
eq_assign_style = function(scope = 'tokens', strict = TRUE, indent_by = 2,
    start_comments_with_one_space = FALSE,
    reindention = styler::tidyverse_reindention(),
    math_token_spacing = styler::tidyverse_math_token_spacing()) {
  x = styler::tidyverse_style(
    scope = scope, strict = strict, indent_by = indent_by,
    start_comments_with_one_space = start_comments_with_one_space,
    reindention = reindention,
    math_token_spacing =math_token_spacing)
  x$token$force_assignment_op = NULL
  x
}
