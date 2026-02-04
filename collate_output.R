#' Capture and Collate Output from an Expression
#'
#' Executes an expression while capturing all output (print, cat, messages,
#' warnings), then displays everything at the end as a single block. For capturing
#' multi-line code, ensure that everything is wrapped in curly braces.
#'
#' @param expr The expression to evaluate (can be wrapped in curly braces for
#'             multi-line code blocks)
#' @param show_header Logical; whether to show a decorative header/footer. Default FALSE.
#' @param header_text Text to display in the header if show_header = TRUE.
#' @param capture_messages Logical; whether to capture message() output
#' @param capture_warnings Logical; whether to capture warnings
#' @param width Numeric; width of the header/footer line
#' @param timestamp Logical; whether to include a timestamp in the header
#'
#' @return Invisibly returns a list containing:
#'         - output: character vector of captured output
#'         - result: the result of the evaluated expression
#'         - messages: any captured messages
#'         - warnings: any captured warnings
#'
#' @examples
#' collate_output({
#'   print("Hello")
#'   cat("World\n")
#'   print(summary(mtcars$mpg))
#' })

collate_output <- function(expr,
                           show_header = FALSE,
                           header_text = "COLLATED OUTPUT",
                           capture_messages = TRUE,
                           capture_warnings = TRUE,
                           width = 60,
                           timestamp = FALSE) {

  # Capture the expression
  expr <- substitute(expr)

  # Initialise storage for messages and warnings
  collected_messages <- character(0)
  collected_warnings <- character(0)

  # Build handler list dynamically
  handlers <- list()

  if (capture_messages) {
    handlers$message <- function(m) {
      collected_messages <<- c(collected_messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  }

  if (capture_warnings) {
    handlers$warning <- function(w) {
      collected_warnings <<- c(collected_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  }

  # Capture all printed output
  captured_output <- capture.output({
    if (length(handlers) > 0) {
      result <- do.call(
        withCallingHandlers,
        c(list(eval(expr, envir = parent.frame(2))), handlers)
      )
    } else {
      result <- eval(expr, envir = parent.frame(2))
    }
  })

  # Build the complete output
  all_output <- captured_output

  # Append warnings if any were captured
  if (length(collected_warnings) > 0) {
    all_output <- c(all_output, "",
                    paste0("--- Warnings (", length(collected_warnings), ") ---"),
                    paste0("• ", trimws(collected_warnings)))
  }

  # Append messages if any were captured
  if (length(collected_messages) > 0) {
    all_output <- c(all_output, "",
                    paste0("--- Messages (", length(collected_messages), ") ---"),
                    trimws(collected_messages))
  }

  # Display the collected output
  if (show_header) {
    cat("\n", strrep("=", width), "\n", sep = "")
    cat(header_text)
    if (timestamp) {
      cat(" | ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), sep = "")
    }
    cat("\n", strrep("=", width), "\n\n", sep = "")
  }

  if (length(all_output) > 0) {
    cat(all_output, sep = "\n")
  } else {
    cat("[No output captured]\n")
  }

  if (show_header) {
    cat("\n", strrep("=", width), "\n", sep = "")
  }

  # Return results invisibly
  invisible(list(
    output = all_output,
    result = result,
    messages = collected_messages,
    warnings = collected_warnings
  ))
}
