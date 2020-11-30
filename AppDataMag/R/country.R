#' Country
#'
#' This function will give us information for coutry from covid data.
#' @return
#' @export
#'
#' @examples
#' country_info()
country_info <- function(){
  covid_df = get_current_covid_data()
  country = readline(prompt = "What Country would you like to know about?: ")
  return(covid_df[covid_df["Country_Region"] == country,])
}
