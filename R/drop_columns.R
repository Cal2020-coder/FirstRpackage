#' Drops NA
#' @description  Drops variables that have too man NA Values
#'
#' @param df This is a structured dataframe with any datatypes inside
#' @param threshold cutoff for missing data from 0-1
#'
#' @return
#' @export
#'
#' @examples
#' drop_columnnas( df, 0.1)
drop_columnnas = function(df, threshold){
  num_obs = nrow(df)
  df = df[ ,colSums(is.na(df)) < num_obs * threshold]
}
