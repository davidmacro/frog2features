
#' Frog DH: replace items in frogged dataset
#'
#' This function can be used to replace items in a frogged dataset. 
#' Note that this function was intended to be an (invisible) back-end to the 
#' \code{default_rule_replace} function. Since this function can also be 
#' used on it's own, it seemed natural to expose it publically. 
#'   
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
#' @param replace.what.token 
#' Name of the column that contains the token to be replaced. If the column does not exist, it is created. 
#' Note: if the intent is to tag columns based on matched values, it is recommended to use the \code{tag_in_frog} function instead. 
#' 
#' @param replace.with.value 
#' Replace matched tokens in \code{replace.what.token} with a specific value.
#' 
#' @param replace.with.value.get 
#' Replace matched tokens in \code{replace.with.value.get} with a specific value.
#' 
#' @param replace.with.value.expression 
#' Replace matched tokens in \code{replace.with.value.get} with a valid data.table j expression.
#' 
#' @param prefix 
#' Optional prefix for the replacement value
#' 
#' @param suffix 
#' Optional suffix for the replacement value
#' 
#' @param verbose 
#' Boolean flag to specify whether to print verbose info (defaults to FALSE)
#' 
#' @param make.copy 
#' Boolean flag to indicate whether to \code{data.table::copy()} the provided \code{dtfrog} object, or to apply the changes in place. 
#' Defaults to \code{FALSE} for speed. Note that since \code{data.table} objects have reference symantics, the default behavior
#' of this function changes the initial object, \emph{even if} the result of this function is assigned to a new variable. 
#' 
#' 
#' @param dry.run 
#' Boolean flag to indicate whether to actually apply the replace, or to just print out descriptive information. 
#' If \code{dry.run = TRUE}, the original \code{dtfrog} object is returned unchanged. 
#' 
#' @return A dtfrog object.
#' 
#' @export
#'
#' @usage 
#' 
#' # Full specification: 
#' replace_in_frog(dtfrog,
#'     where.token                    = NULL,  
#'     where.token.value              = NULL,  
#'     where.token.value.match.type   = c("equals","like", "in"), 
#'     replace.what.token             = "lemma",  
#'     replace.with.value             = NULL,  
#'     replace.with.value.get         = NULL,   
#'     replace.with.value.expression  = NULL,  
#'     prefix                         = "",  
#'     suffix                         = "",  
#'     verbose                        = FALSE,  
#'     make.copy                      = FALSE,  
#'     dry.run                        = FALSE
#' )
#'
#' # Typical specification to replace specific lemmas:
#' replace_in_frog(dtfrog,
#'     where.token                    = "lemma",  
#'     where.token.value              = c("value1", "value2", "..."),  
#'     where.token.value.match.type   = c("in"), 
#'     replace.what.token             = "lemma",  
#'     replace.with.value             = "replacement.value"  
#' )
#' 
#'
#' @examples 
#' 
#' \dontrun{
#' 
#' # ------------------------------------------------------------#
#' # Example 1. Replace specific lemma with a code               #
#' # ------------------------------------------------------------#
#' 
#' # Load the example data:
#' data("wb.sr.frogged") 
#' 
#' # Apply a replace:
#' replaced <- replace_in_frog(
#'    dtfrog                       = wb.sr.frogged,
#'    where.token                  = "lemma",
#'    where.token.value            = c("het", "een", "de"),
#'    where.token.value.match.type = "in",
#'    replace.what.token           = "lemma",
#'    replace.with.value           = "<lidwoord>",
#'    make.copy                    = TRUE   
#' )
#' 
#' # Check visually that replaces have occured: 
#' replaced %>% print
#'   
#'  
#'  
#' # ------------------------------------------------------------#
#' # Example 2. Replace Dutch Stopwords with a Code              #
#' # ------------------------------------------------------------#
#' 
#' # Load the example data:
#' data("wb.sr.frogged") 
#' 
#' # Apply a replace:
#' replaced <- replace_in_frog(
#'    dtfrog                       = wb.sr.frogged,
#'    where.token                  = "lemma",
#'    where.token.value            = entities_dutch_stopwords(),
#'    where.token.value.match.type = "in",
#'    replace.what.token           = "lemma",
#'    replace.with.value           = "<stopwoord>",
#'    make.copy                    = TRUE   
#' )
#' 
#' # Verify that the new dataset has no stopwords:  
#' replaced[lemma %in% entities_dutch_stopwords(), .N]  
#' 
#' # Verify that the old wb.sr.frogged dataset still has stopwords
#' wb.sr.frogged[lemma %in% entities_dutch_stopwords(), .N]  
#'    
#'  
#' # ------------------------------------------------------------#
#' # Example 3. Reference semantics: in place vs copy            #
#' # ------------------------------------------------------------#
#' 
#' # Load the example data:
#' data("wb.sr.frogged") 
#' 
#' # Verify that wb.sr.frogged has stopwords
#' wb.sr.frogged[lemma %in% entities_dutch_stopwords(), .N]  
#' 
#' # Apply the replace from Example 2, but with make.copy = false
#' replaced <- replace_in_frog(
#'    dtfrog                       = wb.sr.frogged,
#'    where.token                  = "lemma",
#'    where.token.value            = entities_dutch_stopwords(),
#'    where.token.value.match.type = "in",
#'    replace.what.token           = "lemma",
#'    replace.with.value           = "<stopwoord>",
#'    make.copy                    = FALSE   
#' )
#' 
#' # Verify that the new dataset has the replaces:  
#' replaced[lemma %in% entities_dutch_stopwords(), .N]    
#'    
#' # Check what happened in wb.sr.frogged
#' wb.sr.frogged[lemma %in% entities_dutch_stopwords(), .N]    
#' 
#' # Note: for more information, see data.table's manual on reference semantics. 
#' # This is a complicated topic, but great performance gains can be made by using data.table's
#' # in-place pointer operations as much as possible, rather than R's traditional copy/assign logic.
#' # 
#' # Where possible, Frog2Features makes use of this behavior, but the unintended consequence of this is
#' # that input objects can be changed by reference (and this is not always desirable). If not,
#' # set make.copy = TRUE; this will enforce R's traditional copy/asign logic, but the code can in some cases be (much) slower. 
#'       
#' }
#' 
#' 
#' 
replace_in_frog <- function(dtfrog,  
    where.token                    = NULL,
    where.token.value              = NULL,
    where.token.value.match.type   = c("like"),
    where.expression               = NULL, 
    replace.what.token             = "lemma",  
    replace.with.value             = NULL, 
    replace.with.value.get         = NULL, 
    replace.with.value.expression  = NULL, 
    prefix                         = "", 
    suffix                         = "", 
    verbose                        = FALSE,
    make.copy                      = FALSE,
    dry.run                        = FALSE){
     
    if(make.copy){ dtfrog <- data.table::copy(dtfrog) }
    
    args.given <- match.call() %>% as.list %>% names %-% ""
       
    # Check 1. We need either where.token or where.token.expression
    if(!(c("where.token", "where.expression") %in% args.given %>% sum == 1)){
        stop("\r\n Replace rule invalid: either specify where.token or where.expression (not neither, not both)")
    }
 
    # Check 2. If where.token is specified, we need where.token.value
    if(!("where.token" %in% args.given)){
        if(!("where.token.value" %in% args.given)){
            stop("\r\n Error: Replace rule invalid: where.token.value should be given.")
        } else {
            if(is.null(where.token.value)){
                stop("\r\n Error: Replace rule invalid: where.token.value should not be null.")
            }
        }
    }
     
    
    # Check 2: does the specified where.token exist in colname of dtfrog?
    #stopifnot(where.token %in% dtfrog 
  
    
    if(verbose){ 
        cat(" \r\n Data transformation: \r\n")
        cat(" \r\n  --------------------------------------------------------------------------- ")  
        cat(" \r\n  Replace items in FROG object" %+% ifelse(dry.run, " (dryrun)", " (real)")) 
        cat(" \r\n  --------------------------------------------------------------------------- ")   
    } 
    
    stopifnot(xor(where.token %>% is.null, where.expression %>% is.null))
    
    # First, build the where.expression 
    
    # Selector variant 1: select on a given where.expression
    if(!is.null(where.expression)){ 
        cat(" where: " %+% where.expression %+% "\r\n") %ifnotnull% where.expression
                 
        if(length(where.expression > 1)){
            where.expression <- paste0(where.expression, collapse = " | ")
        } 
    }
    
    # Selector variant 2: select on where.token equals/in/like where.token.value
    if(!is.null(where.token)){ 
 
        # Selector variant S2a. build expression based on partial string matching (see data.table's %like% operator)
        if(where.token.value.match.type == "like"){   
            where.expression <- paste0(where.token %+% " %like% " %+% "\"" %+% where.token.value %+% "\"", collapse = " | ") 
        }
        
        # Selector variant S2b. build expression based on partial string matching (see data.table's %chin% operator)
        if(where.token.value.match.type == "in"){
            where.expression <- where.token %+% " %chin% c(" %+% paste0("\"" %+% where.token.value %+% "\"", collapse = ",") %+% ")"
        }
         
        # Selector variant S2c. build expression based on equality
        if(where.token.value.match.type == "equals"){
            where.expression <- paste0(where.token %+% "==" %+% "\"" %+% where.token.value %+% "\"", collapse = " | ")
        }  
        
        cat("\r\n\r\n  UPDATE frogged  data.table \r\n\r\n\t WHERE (i): \r\n\t\t" %+% where.expression %+% "\r\n") 
    }
 
    if(dry.run %>% not){
        # Replacement variant R1a. Replace based on value (if replace.with.value is given)
        if(!(is.null(replace.with.value))){ 
            cat("\r\n\t SET (j): \r\n\t\t" %+% replace.what.token %+% " = " %+% prefix %+% replace.with.value %+% suffix) 
            dtfrog[eval(parse(text = where.expression)), (replace.what.token) := prefix %+% replace.with.value %+% suffix, ]
        }
            
        # Replacement variant R1b. Replace based on value from other column (if replace.with.value is given)
        if(!(is.null(replace.with.value.get))){ 
            cat("\r\n\t SET (j):\r\n\t\t " %+% replace.what.token %+% " = " %+% prefix %+% replace.with.value %+% suffix )   
            dtfrog[eval(parse(text = where.expression)), (replace.what.token) := prefix %+% get(replace.with.value.get) %+% suffix,  ]
        }
              
        # Replacement variant R1b. Replace based on value from other column (if replace.with.value is given)
        if(!(is.null(replace.with.value.expression))){
            cat("\r\n\t SET (j):\r\n\t\t" %+% replace.what.token %+% " = " %+% prefix %+% replace.with.value %+% suffix)
            dtfrog[eval(parse(text = where.expression)), (replace.what.token) := prefix %+% replace.with.value.expression %>% parse(text = .) %>% eval %+% suffix,  ]
        }
         
        if(make.copy){
            setorderv(dtfrog, c("docid","sent","position"))
        }
    } 
     
    return(dtfrog)   
}
    
