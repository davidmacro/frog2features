
   
#' Fitler exclude_on_token_content
#'
#' @param dtfrog 
#' @param token 
#' @param items 
#' @param exclude.what 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' @seealso 
exclude_on_token_content <- function(dtfrog, 
    token, 
    items, 
    exclude.what = "inside", ...){  
     
    stop("Error: the dtfrog does not contain a column named " %+% token %+% ".") %if% (token %notin% (dtfrog %>% colnames))
    stop("Error: exclude.what should be 'inside' or 'outside'.") %if% (exclude.what %notin% c("inside","outside"))

    if(exclude.what == "inside"){
        return(dtfrog[!(get(token) %chin% items), ])
        
    } else {
         return(dtfrog[(get(token) %chin% items), ])
    }
     
}  
    
