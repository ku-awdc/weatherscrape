#' Title
#'
#' @param credentials
#' @param historical
#' @param current
#' @param forecast
#'
#' @export
save_credentials <- function(credentials, historical=FALSE, current=FALSE, forecast=FALSE){

  stopifnot(test_weather_folder())

  ## TODO: fix
  if(file.exists(file.path(ws_env[["folder"]], "credentials.rqs"))){
    current <- qread(file.path(ws_env[["folder"]], "credentials.rqs"))
  }

  qsave(list(default=credentials), file.path(ws_env[["folder"]], "credentials.rqs"))

  invisible(TRUE)

}


load_credentials <- function(){
  qread(file.path(ws_env[["folder"]], "credentials.rqs"))[["default"]]
}
