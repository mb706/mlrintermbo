

expect_names <- function(x, ...) {
  checkmate::expect_names(x = x, ..., label = paste(sprintf("'%s'", x), collapse = ", "))
}
