#' @title R6 Class wrapping a collection of `futile.logger` logs
#'
#' @description
#' Wrapper for a collection of `futile.logger` logs. Allows each log
#' to be given a separate threshold but only needs a single call to
#' write to them all. Order of logging levels is (see `futile.logger`
#' for more details):
#' \[ `TRACE` | `DEBUG` | `INFO` | `WARN` | `ERROR` | `FATAL` \]
#' @export
LogCollection <- R6::R6Class(
  "LogCollection",
  private = list(
    # list of named logs
    .log_list = c(),

    # helper function to iterate across logs
    .log_loop = function(.flog_func, msg, ...) {
      for (n in private$.log_list) {
        .flog_func(msg, ..., name = n)
      }
    }
  ),

  public = list(
    #' @description
    #' Write to log at `TRACE` level.
    #' @param msg (`character()`)\cr
    #' The message to log (can be `sprintf` format string).
    #' @param ... \cr
    #' Optional arguments to populate the msg format string.
    trace = function(msg, ...) {
      private$.log_loop(futile.logger::flog.trace, msg, ...)
    },

    #' @description
    #' Write to log at `DEBUG` level.
    #' @param msg (`character()`)\cr
    #' The message to log (can be `sprintf` format string).
    #' @param ... \cr
    #' Optional arguments to populate the msg format string.
    debug = function(msg, ...) {
      private$.log_loop(futile.logger::flog.debug, msg, ...)
    },

    #' @description
    #' Write to log at `INFO` level.
    #' @param msg (`character()`)\cr
    #' The message to log (can be `sprintf` format string).
    #' @param ... \cr
    #' Optional arguments to populate the msg format string.
    info = function(msg, ...) {
      private$.log_loop(futile.logger::flog.info, msg, ...)
    },

    #' @description
    #' Write to log at `WARN` level.
    #' @param msg (`character()`)\cr
    #' The message to log (can be `sprintf` format string).
    #' @param ... \cr
    #' Optional arguments to populate the msg format string.
    warn = function(msg, ...) {
      private$.log_loop(futile.logger::flog.warn, msg, ...)
    },

    #' @description
    #' Write to log at `ERROR` level.
    #' @param msg (`character()`)\cr
    #' The message to log (can be `sprintf` format string).
    #' @param ... \cr
    #' Optional arguments to populate the msg format string.
    error = function(msg, ...) {
      private$.log_loop(futile.logger::flog.error, msg, ...)
    },

    #' @description
    #' Write to log at `FATAL` level.
    #' @param msg (`character()`)\cr
    #' The message to log (can be `sprintf` format string).
    #' @param ... \cr
    #' Optional arguments to populate the msg format string.
    fatal = function(msg, ...) {
      private$.log_loop(futile.logger::flog.fatal, msg, ...)
    },

    #' @description
    #' Add a log to the logger collection.
    #' @param log_name (`character()`)\cr
    #' Name of log to add to logger collection.
    #' @param log_appender (`futile.logger` appender)\cr
    #' Appender type see `futile.logger` for details.
    add = function(log_name, log_appender = NULL) {
      stopifnot("`log_name` must be character string." = is.character(log_name))

      if (!(log_name %in% private$.log_list)) {
        private$.log_list <- c(private$.log_list, log_name)
      }

      if (!is.null(log_appender)) {
        invisible(
          futile.logger::flog.appender(log_appender, name = log_name)
        )
      }
    },

    #' @description
    #' Set threshold of log in logger collection.
    #' @param log_name (`character()`) \cr
    #' Name of log to add to logger.
    #' @param log_threshold
    #' ("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")\cr
    #' Level to set threshold at.
    set_threshold = function(log_name, log_threshold) {
      thresholds <-
        list(
          "TRACE" = futile.logger::TRACE,
          "DEBUG" = futile.logger::DEBUG,
          "INFO" = futile.logger::INFO,
          "WARN" = futile.logger::WARN,
          "ERROR" = futile.logger::ERROR,
          "FATAL" = futile.logger::FATAL
        )
      stop_msg <- paste(
        "`log_threshold` must be one of",
        "(\"TRACE\", \"DEBUG\", \"INFO\", \"WARN\", \"ERROR\", \"FATAL\")"
      )
      if (!(log_threshold %in% names(thresholds))) {
        stop(stop_msg)
      }

      self$add(log_name)
      invisible(
        futile.logger::flog.threshold(
          thresholds[[log_threshold]],
          name = log_name
        )
      )
    },

    #' @description
    #' Get all logs in logger collection.
    get_log_names = function() {
      return(private$.log_list)
    }
  )
)


#' Get a log collection
#'
#' @description
#' Return a log collection with one log to write to
#' console and another to write to file. Logs are named
#' `prefix`.console and `prefix`.file.
#'
#' @param log_file_path Path to log file (will be created if not present).
#' @param prefix Prefix to use on log names.
#'
#' @return A log collection.
#' @export
get_dhsc_logger <- function(log_file_path = "output/log.txt", prefix = "log") {
  stopifnot(
    "`prefix` must be a character string" = is.character(prefix)
  )

  stopifnot(
    "`log_file_path` must be a character string" = is.character(log_file_path)
  )

  dir.create(dirname(log_file_path), recursive = TRUE, showWarnings = FALSE)

  log <- LogCollection$new()

  # names for file and console log
  console_log_name <- paste(prefix, "console", sep=".")
  file_log_name <- paste(prefix, "file", sep=".")

  # add logs to logger
  log$add(console_log_name)
  log$add(
    file_log_name,
    log_appender = futile.logger::appender.file(log_file_path)
  )

  # set default file logging levels to INFO
  log$set_threshold(console_log_name, "INFO")
  log$set_threshold(file_log_name, "INFO")

  invisible(log)
}