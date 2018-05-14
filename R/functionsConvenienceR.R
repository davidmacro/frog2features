#  -------------------------------------------------------------------------------------#                                                                        
#                                                                                       #
#  Auteur: D.A. Macro                                                                   #
#           d.a.macro@minvenj.nl // davidmacro@gmail.com                                #
#                                                                                       #
#  -------------------------------------------------------------------------------------# 

lookupArgs <- function(fname, rule, drop = NULL){
    
	require(data.table)
	
    # Note: this only works for atomic arguments; function is not (yet) recursive
     
    # Get the arguments required for the given funtion
    args.required       <- fname %>% formals  

    # Begin with the default values
    args.resolved <- args.required
    
	if(rule %>% is.data.table){
	    rule %<>% as.list
	}
	
    # Override defaults with user-specified arguments
    for(arg in args.required %>% names){
        
        if(args.required[arg] %>% length == 1){
            
            if(arg %in% drop){
                args.resolved[arg] <- NA
            } else {
            
                if(rule[[arg]] %>% is.null){
                    # User has not specified what to do; keep default
                } else {
                    args.resolved[arg] <- rule[arg]
                }        
            }
            
        } 
    } 
    return(args.resolved) 
} 
 
 
 
 #' Convert all factor variables in a data.table to st44rings. 
#' 
#' @import data.table
#' @import magrittr
#' @param dtable the imput data.table
#' @return a modified data.table object with strings instead of factors. 
#' @examples
#' \donttest{ 
#' x.fixed <- x %>% FixStringsFactors 
#' }
FixStringsFactors <- function(dtable){ 
	i         <- sapply(dtable, is.factor)  %>% c
	dframe    <- data.frame(dtable) 
	dframe[i] <- lapply(dframe[i], as.character)
	return(data.table(dframe)) 
}
 
 
`%w/o%` <- function(x, y) x[!x %in% y] 
 
#' Performs set union
#'
#' @param x A vector
#' @param y A vector
#' @return The union of \code{x} and \code{y}
#' @examples
#' \dontrun{
#' c(1,2) %union% c(3,4)
#' x %union% y %union% z
#' }
`%union%` <- function(x,y){
	return(union(x,y))
}

#' Performs set intersect
#'
#' @param x A vector
#' @param y A vector
#' @return The union of \code{x} and \code{y}
#' @examples
#' \dontrun{
#' c(1,2) %union% c(3,4)
#' x %union% y %union% z
#' }
`%intersect%` <- function(x,y){
	return(intersect(x,y))
}


#' Merge two frog data.table objects
#'
#' @param x A frog data.table  
#' @param y A frog data.tables
#' @return The union of \code{x} and \code{y}
#' @examples
#' \dontrun{
#' c(1,2) %union% c(3,4)
#' x %union% y %union% z
#' }
`%frogmerge%` <- function(a,b){
	return(merge(a,b, by="docid", all = TRUE))
}

#' Merge two frog data.table objects
#'
#' @param x A frog data.table  
#' @param y A frog data.tables
#' @return The union of \code{x} and \code{y}
#' @examples
#' \dontrun{
#' c(1,2) %union% c(3,4)
#' x %union% y %union% z
#' }
`%frogmerge.na.zeros%` <- function(a,b){

	merged <- merge(a,b, by="docid", all = TRUE)
	merged[is.na(merged)] <- 0

	return(merged)
} 

#' Concatenate strings.
#'    
#' @param x,y A string or a vector of strings; numerics are converted to strings. 
#' @return The concatenated strings. Elements are recycled. 
#' @examples
#' \dontrun{
#' c("a","b") %+% c("d","e")
#' } 
`%+%` <- function(x,y){
	x <- ifelse(is.na(x) | is.null(x),"",x)
	y <- ifelse(is.na(y) | is.null(y),"",y)
	return(paste0(x,y, sep=''))
}
 
#' Concatenate strings.
#'    
#' @param x,y A string or a vector of strings; numerics are converted to strings. 
#' @return The concatenated strings. Elements are recycled. 
#' @examples
#' \dontrun{
#' c("a","b") %+% c("c","d")  # // This should return ["ac", "bd"]
#' }
`%&%` <- function(x,y){
	x <- ifelse(is.na(x) | is.null(x),"",x)
	y <- ifelse(is.na(y) | is.null(x),"",y)
	return(paste0(x,y, sep=''))
}

#' Cross product string concatenation
#'
#' @param x,y A string or a vector of strings; numerics are converted to strings. 
#' @return The concatenated strings. Elements are recycled. 
#' @examples
#' \dontrun{
#' c("a","b") %+% c("c","d")  
#' }
`%++%` <- function(x,y){
	xx <- ifelse(is.na(x) | is.null(x),"",x)
	yy <- ifelse(is.na(y) | is.null(y),"",y) 
	dframe <- expand.grid(xx,yy, stringsAsFactors = F) 
	return(dframe[, 1] %+% dframe[,2])
}

#' Cross product string concatenation
#'
#' @param x,y A string or a vector of strings; numerics are converted to strings. 
#' @return The concatenated strings. Elements are recycled. 
#' @examples
#' \dontrun{
#' c("a","b") %+% c("c","d")  
#' }
`%X%` <- function(x,y){
	xx <- ifelse(is.na(x) | is.null(x),"",x)
	yy <- ifelse(is.na(y) | is.null(y),"",y) 
	dframe <- expand.grid(xx,yy, stringsAsFactors = F) 
	return(dframe[, 1] %+% dframe[,2])
}

 
`%-%` <-function(x,y){
   return(setdiff(x,y))
}

#' Alternative if
#'
#' @param x Command to be executed; must be parenthesized. 
#' @return y The condition to satisfy; must be parenthesizd. 
#' @examples
#' \dontrun{
#' (print(test)) %if% (1 == 1) 
#' }
`%if%` <- function(x,y){ 
	if(isTRUE(eval(y))){
		eval(x)
	} 
}


`%ifexists%`<- function(x,y){ 
	if(quote(get(y,pos=1)) %>% deparse %>% exists){
		eval(x)
	} 
}
 
`%ifnotexists%`<- function(x,y){  
	if(!(quote(get(y,pos=1)) %>% deparse %>% exists)){
		eval(x)
	} 
}

`%ifnotexists.here%`<- function(x,y){  
	if(!(quote(get(y,pos=-1)) %>% deparse %>% exists)){
		eval(x)
	} 
}
   
`%ifnot%` <- function(x,y){ 
	if(!isTRUE(eval(y))){
		eval(x)
	} 
}

  
`%identical%` <- function(x,y){ 
	return(identical(x, y, ignore.environment=TRUE, single.NA = FALSE))
}

`%or_if_notexists%`<-function(x,y){ 
    {({return(y);break})} %ifnotexists.here% (x)
	{({return(y);break})} %ifnotexists% (x)
	{({return(y);break})} %ifnull% (x)
	{({return(y);break})} %if.na% (x)
	return(x)
}

 `%or_if_empty%`<-function(x,y){ 
   
	return.y = FALSE
	
	if(is.null(x)){
		return.y = TRUE
	} else {
		if(length(x) == 0){
			return.y = TRUE
		} else {
			if(all(is.na(x))){
				return.y = TRUE
			}
		}
	} 
	if(return.y){
		return(y)
	} else {
		return(x)
	} 
}

divide_safe <- function(x,y, replace.asymptotic = 0){
    
    asymp.val <- rep(replace.asymptotic, length(x))
    zeros     <- rep(0, length(x))
        
    answer <- ifelse(y == zeros, asymp.val , x/y)
    
    return(answer)
    
} 
 
`%/safe/%` <- function(x,y){
    return(divide_safe(x,y))
}
  
layout.rotate <- function(x, degree = 0, rads=((2*pi)/360)*degree, tolerance = 10e-3, truncate = 0){
    
    rot.m <- matrix(c(cos(rads),sin(rads),
                     -sin(rads),cos(rads)),ncol=2)
     
    rotated <- x %*% rot.m
   rotated
}
 

`%or_if_absent%`<-function(x,y){ 

#	# Absent means:
#	#    either not existent, null, or NA
#	
#	check_exists  <- !(quote(get(y,pos=1)) %>% deparse %>% exists)
#	check_notnull <- (!(is.null(y))  		%if% (!check_exists)
#	check_notna   <- (!(is.na(y)))			%if% (!check_notnull)

    {({return(y);break})} %ifnotexists% (x)
	{({return(y);break})} %ifnull% (x)
	{({return(y);break})} %if.na% (x)
	return(y) 
}

`%or_if_present%`<-function(x,y){ 
    {({return(x);break})} %ifnotexists% (y)
	{({return(x);break})} %ifnull% (y)
	{({return(x);break})} %if.na% (y)
	return(y)
}


`%or_if_na%`<-function(x,y){  
	{({return(y);break})} %if.na% (x)
	return(x)
}

`%or_if_null%`<-function(x,y){  
 	{({return(y);break})} %ifnull% (x) 
	return(x)
}

#' Alternative  
`%if.na%` <- function(x,y){
	if(is.na(eval(y))){
		eval(x)
	}  
}

`%ifnull%` <- function(x,y){
	if(is.null(eval(y))){
		return(eval(x))
	}  
}

`%ifnotnull%` <- function(x,y){
	if(!is.null(eval(y))){
		eval(x)
	}  
}

`%ifnotna%` <- function(x,y){
	if(!is.na(eval(y))){
		eval(x)
	}  
}

`%allin%` <- function(x,y){
	return(all(x %in% y))

}

`%notin%` <- function(x,y){
	return(!( x %in% y)) 
}


`%xor%` <- function(x,y){
	return(xor(x,y))
}

`%has%` <- function(x,y){
    y %in% names(x) %>% all %>% return
}
 
`%hasno%` <- function(x,y){ 
	y %notin% names(x) %>% all %>% return
}

`%has.all%` <- function(x,y){
    y %in% names(x) %>% all %>% return
}
 
`%hasno.all%` <- function(x,y){ 
	y %notin% names(x) %>% all %>% return
}

`%has.any%` <- function(x,y){
    y %in% names(x) %>% any %>% return
}
 
`%hasno.any%` <- function(x,y){ 
	y %notin% names(x) %>% any %>% return
}


`%has.not.null%` <- function(x,y){
    
    if(!is.environment(x)){
        print("LHS is not an environment")
        break;
    }
    
    
    if(!is.character(y)){
        print("RHS is not a character vector")
        break;
    }
    
    if(exists(y, envir=x)){
        return(is.null(get(y, envir=x)))
    } else {
        return(FALSE)
    } 
}

`%all.not.null%` <- function(x,y){
    	
    if(!is.list(x)) {
        if(!all(sapply(X = x, FUN = is.environment))){
            print("LHS is not a list of environments")
            return(FALSE)
        }
        
    }   
    if(!is.character(y)){
        print("RHS is not a character vector")
        return(FALSE)
    } 
    
    return(all(sapply(X = x, FUN = function(x){
        x %has.not.null% y
    })))
    
}
 

 