example_internal <- function(argument){

  ## Check that the argument is numeric and not missing:
  stopifnot(is.numeric(argument), !is.na(argument))

  ## It is good practice to use the .data pronoun within dplyr functions:
  df <- tibble(Variable = "Test", Value = argument) |>
    mutate(Value2 = .data$Value^2)

  return(df)

}


## If we need the package to remember things within this R session, we can set them in an environment:
package_env <- new.env()
