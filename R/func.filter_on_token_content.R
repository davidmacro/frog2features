#' Frog DH: filter based on token content
#'
#' This function filters items from a frogged dataset (\emph{i.e.,} \code{dtfrog}) based on 
#' the token's content. 
#'
#' Initially, this function was intended as an (invisible) back-end to the 
#' \code{default_rule_filter} function. But since this function can also be 
#' used on it's own, it seemed natural to expose the function publically. 
#' 
#' @param dtfrog 
#' Frogged dataset. Note: \code{dtfrog} must be a \code{data.table} and must pass the \code{is.frogged(dtfrog)} check.
#' 
#' @param where.token 
#' Name of the column to select values of. Note: \code{where.token} must be a column in \code{dtfrog}.
#'
#' @param where.token.value 
#' Value to match against. If \code{where.token.value} is a vector, the selector is valid if at least one of the item matches. 
#' For this, the match type given in \code{where.token.value.match.type} determines how each item is matched. 
#' 
#' @param where.token.value.match.type 
#' Type of matching. Should be any of the following: (\code{"equals"}, \code{"like"}, or \code{"in"}).
#' 
#' @param where.expression 
#' Alternative selection method using a direct data.table 'i'-expression; see the examples. 
#' Note: if \code{where.expression} is specified, \code{where.token} should not be specified, and vice versa. 
#' 
#' @param remove.what 
#' \code{"match"} removes all rows matched; \code{"nomatch"} removes all non-matched rows. 
#' 
#' @param make.copy 
#' Boolean flag to indicate whether to \code{data.table::copy()} the provided \code{dtfrog} object, 
#' or to apply the changes in place. 
#' \emph{Note:} this parameter will not do anything until \code{data.table} implements by-reference row deletion. 
#'
#' @param dry.run 
#' Boolean flag to indicate whether to actually apply the filter, or to just print out descriptive information. 
#' If \code{dry.run = TRUE}, the original \code{dtfrog} object is returned unchanged. 
#'
#' @return 
#' A filtered dtfrog object.
#' 
#' @export
#' 
#' @usage
#' filter_on_token_content(dtfrog, 
#'     where.token                  = NULL,
#'     where.token.value            = NULL, 
#'     where.token.value.match.type = c("in","like", "equals"), 
#'     where.expression             = NULL, 
#'     remove.what                  = c("match", "nomatch", "inside", "outside"), 
#'     verbose                      = FALSE, 
#'     make.copy                    = FALSE,
#'     dry.run                      = FALSE
#' )
#'
#' @examples
#' \dontrun{
#' 
#' # ---------------------------------- #
#' # Example 1. Remove Dutch stopwords  #
#' # ---------------------------------- #
#'
#' # Load the example data
#' data("wb.sr.frogged") 
#'
#' # Count the number of lemmas in the example dataset are Dutch stopwords:
#' wb.sr.frogged[lemma %in% entities_dutch_stopwords(), .N]
#' 
#' # Filter Dutch stopwords.
#' filtered.data <- filter_on_token_content(dtfrog = wb.sr.frogged, 
#'    where.token       = "lemma",
#'    where.token.value = entities_dutch_stopwords(),
#'    remove.what       = "match" 
#' )
#'
#' # Verify that there are no stopwords left:
#' filtered.data[lemma %in% entities_dutch_stopwords(), .N]
#' 
#'
#' 
#' # --------------------------------------------------- #
#' # Example 2. Remove lemmas starting with 'a' or 'b'.  #
#' # --------------------------------------------------- #
#'
#' # Load the example data
#' data("wb.sr.frogged") 
#'  
#' # Filter lemma starting with 'a' or 'b':
#' filtered.data <- filter_on_token_content(dtfrog = wb.sr.frogged,
#'     where.token                  = "lemma",                  
#'     where.token.value            = c("\\\\<a","\\\\<b"),     # These are regular expressions with properly applied escape characters. 
#'     where.token.value.match.type = "like",                   # This causes 'where.token.value' to be evaluated as a list of regular expressions 
#'     remove.what                  = "match"                   # Remove matched elements. 
#' )
#'
#'
#'
#' # --------------------------------------------------- #
#' # Example 3. Remove using complex where expression    #
#' # --------------------------------------------------- #
#'
#' # Load the example data
#' data("wb.sr.frogged") 
#'
#' # Look at the various types of 'staat' 
#' wb.sr.frogged[lemma == "staat", .N, by = c("lemma","ner", "majorpos")][order(-N)]
#' 
#' # Output: 
#' # ----------------------------------------- #
#' #     lemma |    ner  | majorpos   |  N     #
#' # --------------------|-------------------- #
#' #  1: staat |      O  |        N   | 56     #
#' #  2: staat |  B-ORG  |        N   |  9   <------ entries to filter
#' #  3: staat | B-MISC  |        N   |  1     #
#' # ----------|----------------------|------- #
#'
#' # Suppose we only want to filter the second type of entries: 
#' filtered.data <- filter_on_token_content(dtfrog = wb.sr.frogged, 
#'     where.expression  = "ner == 'B-ORG' & lemma == 'staat'",
#'     remove.what       = "match",
#'     verbose           = T
#' ) 
#' 
#' # Verify:
#' filtered.data[lemma == "staat", .N, by = c("lemma","ner", "majorpos")][order(-N)]
#' }  
#'
filter_on_token_content <- function(dtfrog,   
    where.token                    = NULL,  
    where.token.value              = NULL,  
    where.token.value.match.type   = c("in","like", "equals"),
    where.expression               = NULL, 
    remove.what                    = c("match","nomatch","inside", "outside"),
    verbose                        = FALSE,  
    make.copy                      = TRUE,  
    dry.run                        = FALSE 
){   
    
    stopifnot(is.frog(dtfrog))
    args.given  <- match.call() %>% as.list %>% names %-% ""
    
    # Check 1. We need either where.token or where.token.expression
    if(!(c("where.token", "where.expression") %in% args.given %>% sum == 1)){
        stop("\r\n Error: Filter rule invalid: either specify where.token or where.expression (not neither, not both)")
    }
    
    # Check 2. If where.token is specified, we need where.token.value and a valid remove.what
    if("where.token" %in% args.given){ 

        remove.what                   <- match.arg(remove.what) 
        where.token.value.match.type  <- match.arg(where.token.value.match.type) 
    
        if(!("where.token.value" %in% args.given)){
            stop("\r\n Error: Filter rule invalid: where.token.value should be given.")
        } else {
            if(is.null(where.token.value)){
                stop("\r\n Error: Filter rule invalid: where.token.value should not be null.")
            }
        }
    
        if(!("remove.what" %in% args.given)){
            stop("\r\n Error: Filter rule invalid: remove.what should be given.")
        } else {
            if(is.null(where.token.value)){
                stop("\r\n Error: Filter rule invalid: remove.what should not be null.")
            } 
            if(remove.what %notin% c("match","nomatch", "inside", "outside")){
                stop("Error: exclude.what should be 'match', 'nomatch', 'inside', or 'outside'")
            } 
        }  
    } 
     
    if(verbose){ 
        cat(" \r\n Data transformation: \r\n")
        cat(" \r\n  --------------------------------------------------------------------------- ")  
        cat(" \r\n  Filter (remove) items in FROG object" %+% ifelse(dry.run, " (dryrun)", " (real)")) 
        cat(" \r\n  --------------------------------------------------------------------------- ")   
    }  
    
    # Filter variant 1: select on a given where.expression
    if(!is.null(where.expression)){ 
        cat(" where: " %+% where.expression %+% "\r\n") %ifnotnull% where.expression
                 
        if(length(where.expression > 1)){
            where.expression <- paste0(where.expression, collapse = " | ")
        } 
    }
    
    # Filter variant 2: select on where.token equals/in/like where.token.value
    if(!is.null(where.token)){ 
 
        # Filter variant S2a. build expression based on partial string matching (see data.table's %like% operator)
        if(where.token.value.match.type == "like"){   
            where.expression <- paste0(where.token %+% " %like% " %+% "\"" %+% where.token.value %+% "\"", collapse = " | ") 
        }
        
        # Filter variant S2b. build expression based on partial string matching (see data.table's %chin% operator)
        if(where.token.value.match.type == "in"){
            where.expression <- where.token %+% " %chin% c(" %+% paste0("\"" %+% where.token.value %+% "\"", collapse = ",") %+% ")"
        }
         
        # Filter variant S2c. build expression based on equality
        if(where.token.value.match.type == "equals"){
            where.expression <- paste0(where.token %+% "==" %+% "\"" %+% where.token.value %+% "\"", collapse = " | ")
        }   
    }
 
    where.expression.print <- where.expression
      
    if(stringi::stri_length(where.expression.print) > 80){
        where.expression.print %<>% stringi::stri_sub(from=1, length = 80) 
        where.expression.print <- where.expression.print %+% "...\r\n"
    }
    
    if(remove.what %in% c("inside", "match")){
        if(verbose){
            cat("\r\n\r\n  DELETE FROM frogged  data.table \r\n\r\n\t WHERE (i): \r\n\t\t" %+% where.expression.print %+% "\r\n") 
        } 
    }
    
    if(remove.what %in% c("outside", "nomatch")){
        if(verbose){
            cat("\r\n\r\n  DELETE FROM frogged  data.table \r\n\r\n\t WHERE NOT (i): \r\n\t\t" %+% where.expression.print %+% "\r\n") 
        }
    }
  
    if(dry.run %>% not){ 
        if(remove.what %in% c("inside", "match")){
           return(dtfrog[!eval(parse(text=where.expression)), ])
        }  
        if(remove.what %in% c("outside", "nomatch")){
           return(dtfrog[eval(parse(text=where.expression)), ])
        }   
    }  else {
        return(dtfrog)
    }    
}  
    
