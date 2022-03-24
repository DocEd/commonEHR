#' @title Parse range limits
#'
#' @param ranges a character vector of length 1.
#'
#' @return
#' @export
#'
#' @importFrom rlang abort
#' @importFrom stringr str_split str_sub
#' @importFrom readr parse_number
#' @importFrom dplyr case_when
#'
#' @examples
#' # Inclusive interval
#' parse_range("[0, 100]")
#' # Non-inclusive interval
#' parse_range("(0, 100)")
#' # Open positive interval
#' parse_range("[0, Inf)")
parse_range <- function(ranges = NULL) {

  if (class(ranges) != "character") {
    abort("You must supply a range in the format `(##, ##)`/`[##, ##]` or similar")
  }

  if (is.na(ranges)) {
    return(
      list(
        lims = c(-Inf, Inf),
        lower_lim = `>=`,
        upper_lim = `<=`)
    )
  }

  raw_lims <- str_split(ranges, ",", simplify = TRUE)

  if (length(raw_lims) != 2) {
    abort("You must supply a range in the format `(##, ##)`/`[##, ##]` or similar")
  }

  check_infinites <- grepl("inf", raw_lims, ignore.case = TRUE)
  lims <- vector("numeric", 2)

  if (check_infinites[1]) {
    lims[1] <- -Inf
  } else {
    lims[1] <- parse_number(raw_lims[1])
  }

  if (check_infinites[2]) {
    lims[2] <- Inf
  } else {
    lims[2] <- parse_number(raw_lims[2])
  }

  lower_boundaries <- str_sub(ranges, 1, 1)

  lower_lim <- case_when(
    lower_boundaries == "[" ~ c(`>=`),
    lower_boundaries == "(" ~ c(`>`),
    TRUE ~ list(NA_character_)
  )[[1]]

  upper_boundaries <- str_sub(ranges, nchar(ranges), nchar(ranges))

  upper_lim <- case_when(
    upper_boundaries == "]" ~ c(`<=`),
    upper_boundaries == ")" ~ c(`<`),
    TRUE ~ list(NA_character_)
  )[[1]]

  if (!is.function(lower_lim) | !is.function(upper_lim)) {
    abort("You must supply a range in the format `(##, ##)`/`[##, ##]` or similar")
  }

  return(list(lims = lims, lower_lim = lower_lim, upper_lim = upper_lim))

}
