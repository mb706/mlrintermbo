

check_names <- function(x, ...) {
  checkmate::check_names(x = x, ..., label = paste(sprintf("'%s'", x), collapse = ", "))
}
