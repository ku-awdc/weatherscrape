## TODO:  S7 class

#' @importFrom lubridate as_date today year wday isoyear isoweek
#' @importFrom stringr str_c str_replace_all
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom checkmate qassert assert_date

make_week.numeric <- function(week, wyear=year(today()), wday=NULL, compress=is.null(wday)){

  if(!identical(week, 1L)){
    week <- as.numeric(week)
    qassert(week, "X+[1,53]")
    stopifnot(week <= max_week(wyear))
  }

  compress <- force(compress)
  floor_week <- is.null(wday)
  if(is.null(wday)) wday <- 1

  bnd <- if(compress) "" else "-"
  jan1 <- as_date(str_c(wyear,"01-01"))

  tibble(
    Date = jan1 + (week*7 + wday - wday(jan1+3, week_start=1) - 3) - 1
  ) |>
    mutate(Year = isoyear(.data$Date), Week = isoweek(.data$Date), Day = wday) |>
    mutate(String = str_c(format(.data$Year, width=4), bnd, "W", format(.data$Week, width=2), if(!floor_week) str_c(bnd, .data$Day)) |> str_replace_all(" ", "0")) ->
    rv

  stopifnot(rv$Year == wyear, rv$Week == week, rv$Day == wday)
  return(rv)

}

make_week.date <- function(date = today(), floor_week=TRUE, compress=floor_week){

  assert_date(date, any.missing=FALSE, min.len=1L)
  if(floor_week) date <- floor_date(date, unit="week", week_start=1L)
  bnd <- if(compress) "" else "-"

  tibble(
    Date = date
  ) |>
    mutate(Year = isoyear(.data$Date), Week = isoweek(.data$Date), Day = wday(.data$Date)) |>
    mutate(String = str_c(format(.data$Year, width=4), bnd, "W", format(.data$Week, width=2), if(!floor_week) str_c(bnd, .data$Day)) |> str_replace_all(" ", "0"))
}

max_week <- function(year){
  isoweek(make_week.numeric(1L, wyear=as.numeric(year)+1L, wday=1L)$Date - 7L)
}
