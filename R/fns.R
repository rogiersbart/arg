#' Argument count
#'
#' Access the number of arguments in the argument vector.
#'
#' @returns The number of raw arguments provided in the command line call.
#' @export
#'
#' @examples
#' assignInNamespace("arguments", base::c("one", "two","three"), "arg") # for interactive testing
#' arg::v()
c <- function() {
  length(arguments)
}

#' Argument vector
#'
#' Access the argument vector. If an argument(s) and/or option(s) specification
#' is provided, the argument vector is preprocessed first, and a named list with
#' arguments and options is returned instead.
#'
#' @param ... A `tibble::tribble()` argument(s) and/or option(s) specification,
#'   with `name`, `short`, `long`, `default`, `type` and `help` columns.
#' @param title The title for the documentation.
#' @param header An introductory header for the documentation.
#' @param footer A closing footer for the documentation.
#' @param expose Should the arguments and options be attached to the calling
#'   environment instead of being returned as a named list?
#'
#' @returns A named list of processed options and arguments, except if ... is
#'   not defined, in case which the raw argument vector is returned, or if
#'   expose is TRUE, in case which the options and arguments are attached to the
#'   calling environment and nothing is returned.
#' @export
#'
#' @examples
#' assignInNamespace("arguments", base::c("one", "2", "-o", "-h"), "arg") # for interactive testing
#' arg::v(
#'   ~name, ~short, ~long, ~default, ~type, ~help,
#'   "arg1", NA, NA, "default_arg1_value", "character", "First argument",
#'   "arg2", NA, NA, "1", "integer", "Second argument",
#'   "opt1", "o", "option1", NA, "logical", "Logical option",
#'   "opt2", "p", "option2", "Default", "character", "Character option",
#'   header = "This command line tool provides functionality to ..."
#' )
v <- function(
  ...,
  title = "Documentation",
  header = "",
  footer = "",
  expose = FALSE
) {
  df <- tibble::tribble(...)
  df$help <- ifelse(is.na(df$default), df$help, df$help |> paste("(default = \"%default\")"))
  df_list <- apply(df, 1, \(x) as.list(x[!is.na(x)]))
  expose_env <- parent.frame()
  do.call(v_list, df_list |> append(list(title = title, header = header, footer = footer, expose = expose, expose_env = expose_env)))
}
v_list <- function(..., title, header, footer, expose, expose_env) {
  dots <- list(...)
  if (length(dots) == 0) return(arguments)
  is_opt <- lapply(dots, \(x) any(names(x) %in% base::c("short", "long"))) |> as.logical()
  args <- dots[!is_opt]
  opts <- dots[is_opt]
  p <- optparse::OptionParser(
    formatter = \(object) {
      options_list <- object@options
      std::err("# {title}")
      if (header != "") {
        cat("\n", file = stderr())
        std::err("{header}")
      }
      if (length(args) != 0) {
        cat("\nArguments:\n\n", file = stderr())
        for (ii in seq_along(options_list)) {
          option <- options_list[[ii]]
          if (!grepl("^--ARG-", option@long_flag)) next
          cat("  ", file = stderr())
          if (!is.null(option@long_flag)) {
            argname <- toupper(gsub("^ARG-", "", option@metavar))
            std::err('{.red(argname)}')
          }
          cat("\n", file = stderr())
          std::err(paste0("i ", sub("%default", optparse:::as_string(option@default), option@help)))
          cat("\n", file = stderr())
        }
      }
      if (length(opts) != 0) {
        cat("\nOptions:\n\n", file = stderr())
        for (ii in seq_along(options_list)) {
          option <- options_list[[ii]]
          if (grepl("^--ARG-", option@long_flag)) next
          flags <- list(short = "", long = "")
          if (!is.na(option@short_flag)) {
            flags$short <- option@short_flag
            if (optparse:::option_needs_argument(option)) {
              flags$short <- paste(flags$short, toupper(option@metavar))
            }
          }
          if (!is.null(option@long_flag)) {
            flags$long <- option@long_flag
            if (optparse:::option_needs_argument(option)) {
              flags$long <- paste0(flags$long, "=", toupper(option@metavar))
            }
          }
          cat("  ", file = stderr())
          std::err("{.blue(flags$short)}, {.blue(flags$long)}")
          cat("\n", file = stderr())
          std::err(paste0("i ", sub("%default", optparse:::as_string(option@default), option@help)))
          cat("\n", file = stderr())
        }
      }
      if (footer != "")  {
        cat("\n", file = stderr())
        std::err("{footer}")
      }
    }
  )
  arg_names <- rep(NA_character_, length(args))
  if (length(opts) != 0) {
    for (i in 1:length(opts)) {
      # TODO expose metavar?
      p <- p |> optparse::add_option(
        dest = opts[[i]][["name"]],
        opt_str = base::c(paste0("--", opts[[i]][["long"]]),
                    paste0("-", opts[[i]][["short"]])),
        default = if (opts[[i]]$type == "logical") FALSE else opts[[i]][["default"]],
        action = if (opts[[i]]$type == "logical") "store_true" else "store",
        help = opts[[i]]$help,
        type = opts[[i]]$type
      )
    }
  }
  if (length(args) != 0) {
    for (i in 1:length(args)) {
      arg_names[i] <- args[[i]][["name"]]
      # TODO expose metavar?
      p <- p |> optparse::add_option(
        dest = args[[i]][["name"]],
        opt_str = paste0("--ARG-", args[[i]][["name"]]),
        default = args[[i]][["default"]],
        action = "store",
        help = args[[i]]$help,
        type = args[[i]]$type
      )
    }
  }
  is_opt <- which(grepl("^-", arguments))
  is_opt <- base::c(is_opt, is_opt + 1)
  is_arg <- 1:length(arguments)
  is_arg <- is_arg[!is_arg %in% is_opt]
  if (length(args) != 0) {
    for (i in seq_along(is_arg)) {
      arguments <- append(
        arguments,
        paste0("--ARG-", arg_names[i]),
        after = is_arg[i] - 2 + i
      )
    }
  }
  a <- optparse::parse_args(
    p,
    arguments,
    positional_arguments = base::c(0, length(args)),
    convert_hyphens_to_underscores = TRUE
  )
  a$options <- a$options[!names(a$options) == "help"]
  if (expose) {
    # FIXME avoid help here!
    list2env(a$options, expose_env)
    return(invisible())
  }
  a$options
}
