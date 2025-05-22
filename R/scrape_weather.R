#' Scrape weather data for multiple locations (DO NOT USE FROM INSIDE KU OR KU VPN)
#' @name scrape_weather
#'
#' @param year a single year to fetch (ignored if start_date/end_date are changed)
#' @param start_date start date for the relevant period (inclusive)
#' @param end_date end date for the relevant period (inclusive)
#' @param locations a data frame of locations to scrape (optional - otherwise \code{\link{weather_locations}} is used)
#' @param path a path to a folder to save results
#' @param max_scrapes the maximum number of scrapes to run
#' @param interval the interval between scrapes (should end in s, m or h)
#' @param fail_interval a pause interval following an unsuccessful scrape (should end in s, m or h)
#' @param progress the type of progress update to show - either "pb", "log" or "none"
#'
#' @returns a data frame showing the scraping status of each location, invisibly
#'
#' @details
#' NOTE:  DO NOT USE THESE FUNCTIONS FROM INSIDE A KU NETWORK OR WHILE CONNECTED TO THE VPN
#'
#' The OpenMeto fair usage policy (https://open-meteo.com/en/terms) allows us
#' to make 10000 API calls per day and/or 5000 per day and/or 600 per minute.
#' Scraping a full year of data for a single location is just under 60 API calls.
#' This equates to 15 minute intervals for continuous running (5760 API calls
#' per day), or a maximum of 60 locations with 15 second delays (3600 API calls
#' in around 15 mins). Note: some safety is deliberately built in to these
#' limits - please do not go over them!
#'
#' This means you can either start the scrape_continual function and leave it
#' running continuously, or run the scrape_burst_dk or scrape_burst_eu functions
#' intermittently BUT A MAXIMUM OF ONCE PER DAY.
#'
#' Note that API limits are shared by everyone on your network, and any calls
#' to \code{\link{fetch_weather}} also count against this.
#'
#' @importFrom qs qsave qread
#' @importFrom pbapply pblapply
NULL

#' @rdname scrape_weather
#' @export
scrape_burst_dk <- function(year, path = "~/weather_scrape"){
  locations <- weatherscrape::weather_locations |> filter(.data$GridScale=="10x10km")
  scrape_weather(year=year, path=path, max_scrapes = 60L, interval = "15s", fail_interval = "abort", progress = "pb")
}

#' @rdname scrape_weather
#' @export
scrape_burst_eu <- function(year, path = "~/weather_scrape"){
  locations <- weatherscrape::weather_locations |> filter(.data$GridScale=="100x100km")
  scrape_weather(year=year, path=path, max_scrapes = 60L, interval = "15s", fail_interval = "abort", progress = "pb")
}


#' @rdname scrape_weather
#' @export
scrape_continual <- function(year, path = "~/weather_scrape"){
  scrape_weather(year=year, path=path, max_scrapes = NA_integer_, interval = "15m", fail_interval = "1h", progress = "log")
}


#' @rdname scrape_weather
#' @export
scrape_weather <- function(year, start_date = as_date(str_c(year, "-01-01")), end_date = as_date(str_c(year, "12-31")), locations = NULL, path = "~/weather_scrape", max_scrapes = 60L, interval = "15s", fail_interval = "abort", progress = c("pb", "log", "none")){

  cat("\n----------------------------------------------------------------------\n")
  cat("IMPORTANT NOTE:\n\tPlease check that you are not on a KU network (including VPN).\n\tIf you are, then abort this function call now!!!\n")
  cat("----------------------------------------------------------------------\n\n")

  assert_date(start_date, any.missing=FALSE, len=1L, lower=as_date("1900-01-01"))
  assert_date(end_date, any.missing=FALSE, len=1L, lower=start_date, upper=(Sys.Date() - 7L))
  stopifnot(strftime(start_date, "%Y") == strftime(end_date, "%Y"))
  year <- strftime(start_date, "%Y")

  qassert(path, str_c("S1"))
  if(!dir.exists(path)) dir.create(path)
  if(!dir.exists(file.path(path, year))) dir.create(file.path(path, year))

  if(is.null(locations)){
    locations <- weatherscrape::weather_locations
  }

  stopifnot(
    is.data.frame(locations),
    c("ID", "Latitude", "Longitude", "MeanElevation") %in% names(locations),
    !c("Status", "Index") %in% names(locations)
  )

  qassert(locations[["ID"]], "S*")
  assert_vector(locations[["ID"]], unique=TRUE)
  assert_numeric(locations[["Latitude"]], any.missing=FALSE, lower=-90, upper=90)
  assert_numeric(locations[["Longitude"]], any.missing=FALSE, lower=-180, upper=180)
  assert_numeric(locations[["MeanElevation"]], any.missing=TRUE, lower=-500, upper=10000)

  # Function to check if all files are present
  files_present <- function(x){
    present <- list.files(file.path(path, year))
    expected <- str_c(x, ".rqs")
    expected %in% present
  }
  fctl <- c("Pending","Failed","Complete")
  locations |>
    mutate(Status = if_else(files_present(.data$ID), "Complete", "Pending") |> fct(levels=fctl)) ->
    locations
  if(!"Priority" %in% names(locations)) locations <- locations |> mutate(Priority = row_number())

  # Check max_scrapes and interval
  units <- str_sub(interval, start=-1L, end=-1L)
  stopifnot(units %in% c("s", "m", "h", "f"))
  interval_s <- str_sub(interval, start=1L, end=-2L) |> as.numeric() |> {\(x) x * case_when(units%in%c("s","f") ~ 1L, units=="m" ~ 60L, units=="h" ~ 60L^2L)}()
  assert_number(interval_s, lower=if_else(units!="f", 15, 1), finite=TRUE)
  qassert(max_scrapes, "x1[1,)")
  if(is.na(max_scrapes)) max_scrapes <- Inf
  if(units!="f" && interval_s < 15*60){
    if(max_scrapes > 60L) stop("A minimum interval of 15m is required for max_scrapes > 60")
    cf <- file.path(path, "last_burst_date.rqs")
    if(file.exists(cf)){
      dt <- qread(cf)
      if(dt == Sys.Date()) stop("You have already run a short-interval scrape today; please wait until tomorrow to re-run")
    }
    qsave(Sys.Date(), cf)
  }

  qassert(fail_interval, "S1")
  if(fail_interval=="abort") fail_interval <- "Infs"
  units <- str_sub(fail_interval, start=-1L, end=-1L)
  stopifnot(units %in% c("s", "m", "h", "f"))
  fail_interval_s <- str_sub(fail_interval, start=1L, end=-2L) |> as.numeric() |> {\(x) x * case_when(units%in%c("s","f") ~ 1L, units=="m" ~ 60L, units=="h" ~ 60L^2L)}()
  assert_number(fail_interval_s, lower=if_else(units!="f", 15*60, 1))

  progress <- match.arg(progress)
  if(progress=="pb"){
    applyfun <- pblapply
    progressfun <- function(x){ }
  }else if(progress=="log"){
    applyfun <- lapply
    progressfun <- function(x){
      cat("\tScraping ", x[["ID"]], " at ", as.character(Sys.time()), "...\n", sep="")
    }
  }else if(progress=="none"){
    applyfun <- lapply
    progressfun <- function(x){ }
  }else{
    stop("Unrecognised progress type")
  }

  if(is.infinite(max_scrapes)){
    cat("Beginning continual scraping with interval = ", interval, " on ", as.character(Sys.time()), "\n", sep="", append=TRUE, file=file.path(path, year, "log.txt"))
  }else{
    cat("Beginning ", max_scrapes, " scrapes with interval = ", interval, " on ", as.character(Sys.time()), "\n", sep="", append=TRUE, file=file.path(path, year, "log.txt"))
  }

  # Loop over locations
  pass <- 1L
  scrapes_remaining <- max_scrapes

  while(!all(files_present(locations[["ID"]]))){
    if(is.infinite(max_scrapes)){
      cat("Starting pass #", pass, " (continual scraping with interval = ", interval, ")\n", sep="")
    }else{
      cat("Starting pass #", pass, " (max_scrapes = ", scrapes_remaining, " with interval = ", interval, ")\n", sep="")
    }

    locations |>
      mutate(Index = row_number()) |>
      filter(.data$Status != "Complete") |>
      arrange(.data$Status, .data$Priority) |>
      pull(.data$Index) ->
      indexes_using

    if(length(indexes_using) == 0L) stop("An unexpected error occured (indexes_using has length 0) - please report to Matt!")

    stopifnot(scrapes_remaining > 0L)
    if(!is.infinite(scrapes_remaining) && scrapes_remaining < length(indexes_using)){
      indexes_using <- indexes_using[1:scrapes_remaining]
    }

    ## Hack to avoid sleeping before the initial scrape:
    if(pass==1L){
      stopifnot(!is.na(locations[["Status"]]))
      locations[["Status"]][indexes_using[1L]] <- NA_character_
    }

    locations |>
      slice(indexes_using) |>
      rowwise() |>
      group_split() |>
      applyfun(\(x){

        if(!is.na(x[["Status"]])) Sys.sleep(interval_s)
        progressfun(x)
        ss <- try({
          wthr <- fetch_weather(latitude=x[["Latitude"]], longitude=x[["Longitude"]], elevation=x[["MeanElevation"]], start_date=start_date, end_date=end_date, format=TRUE)
        })
        if(inherits(ss, "try-error")){
          if(is.infinite(fail_interval_s)) stop("Error encountered: aborting")
          cat("Error encountered for ID ", x[["ID"]], ": pausing for ", fail_interval, "\n", sep="")
          Sys.sleep(fail_interval_s)
          return(fct("Failed", levels=fctl))
        }

        wthr <- wthr |> mutate(ID = x[["ID"]]) |> select(.data$ID, everything())
        qsave(wthr, file.path(path, year, str_c(x[["ID"]], ".rqs")), preset="archive")

        return(x |> mutate(Status = fct("Complete", levels=fctl)))

      }) |>
      bind_rows() ->
      new_status

    locations |>
      full_join(
        new_status |> select(.data$ID, NewStatus = .data$Status),
        by = join_by("ID")
      ) |>
      mutate(Status = if_else(is.na(.data$NewStatus), .data$Status, .data$NewStatus)) |>
      select(-.data$NewStatus) ->
      locations

    if(!is.infinite(max_scrapes)){
      scrapes_remaining <- scrapes_remaining - length(indexes_using)
      if(scrapes_remaining < 0L) stop("An unexpected error occured (scrapes_remaining is ", scrapes_remaining, ") - please report to Matt!")
      if(scrapes_remaining == 0L) break
    }
    pass <- pass + 1L
  }

  # If all complete, save with archive, but re-check each output
  if(all(locations[["Status"]] == "Complete")){
    cat("Scraping complete: beginning final pass to load and re-save...\n")

    pblapply(locations[["ID"]], function(x){
      qread(file.path(path, year, str_c(x, ".rqs")))
    }) |>
      bind_rows() ->
      all_wthr

    days_per_id <- all_wthr |> count(.data$ID)
    stopifnot(
      days_per_id[["n"]] == ((end_date-start_date)+1L),
      nrow(days_per_id) == nrow(locations),
      locations[["ID"]] %in% days_per_id[["ID"]],
      sapply(all_wthr[["hourly"]], nrow) == 24L,
      strftime(all_wthr[["date"]], "%Y") == year
      )

    outfile <- str_c("weather_", year, "_", nrow(all_wthr), "locns.rqs")
    stopifnot(!file.exists(outfile))
    qsave(all_wthr, file.path(path, outfile), preset="archive")

    ## TODO:
    # - fix error with 2024 data
    # - auto-change year to week or quarter or month or half depending on date range

    cat("Scraping completed on ", as.character(Sys.time()), " - please send '", outfile, "' to Matt.\n", sep="", append=TRUE, file=file.path(path, year, "log.txt"))
    cat("Scraping completed - please send '", outfile, "' to Matt.\n", sep="")
  }else{
    complete <- sum(locations[["Status"]]=="Complete")
    failed <- sum(locations[["Status"]]=="Failed")
    cat("Scraping terminated with ", complete, " (", round(complete/nrow(locations)*100, 1), "%) completed locations", if(failed>0L) str_c(" (and ", failed, " that failed during this session)"), ".", sep="")
  }

  # Return invisibly
  invisible(locations)
}
