#' Dog
#'
#' @param love Do you love dogs? Defaults to TRUE
#'
#' @keywords Dogs
#' @return
#' @export
#'
#' @examples
#' dog_loves(Love)
dog_loves <- function(love=TRUE){
    if(love==TRUE){
        print("I love Husky!")
    }
    else {
        print("I am not a cool person.")
    }
}
