example_function <- function(optional_argument = 42L){

  # Internal functions are visible within your package code:
  df <- example_internal(optional_argument)

  # It is a good idea to use simple S3 classes to allow custom print/summary/autoplot etc methods:
  class(df) <- c("ExampleClass", class(df))

  return(df)
}
