.onLoad <- function(libname, pkgname) {
  arguments <<- commandArgs(TRUE)
}
.onAttach <- function(libname, pkgname) {
  std::err("! Package {{{.yellow('arg')}}} is still in its experimental lifecycle stage.")
  std::err("! Use at your own risk, and submit issues here:")
  std::err("! {.link('https://github.com/rogiersbart/arg/issues')}")
  std::err("i You are attaching the {{{.yellow('arg')}}} namespace.")
  std::err("i We advise using {.link('arg::v()', 'ide:help:arg::v')} or {.link('arg::c()', 'ide:help:arg::c')} instead!")
  invisible()
}
