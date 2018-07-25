#' special.lemma.separators
#'
#' @return vector of lemmas that are typically used as separators.
#' @export 
#'
#' @examples
special.lemma.separators <- function(){  
	c(";", ")", "(", "{", "}", "[","]",":",",") %>% return  
}
 
#' special.lemma.punctuation 
#'
#' @return vector of lemmas that are typically used as punctuation characters.
#' @export
#'
#' @examples
special.lemma.punctuation <- function(){ 
    
	c(".",";", ")", "(", "{", "}", 
	"[","]","/","\\", ":", 
	"\"",",","!","!!","%","$","*") %>% return
  
}

 
majorpos.N <- function(){
    return(c("N"))
}  
 
majorpos.ADV <- function(){
    return(c("ADJ"))
}  
 
majorpos.ADJ <- function(){
    return(c("ADV"))
}   
 
majorpos.punctuation <- function(){
    return(c("LET"))
} 

majorpos.special <- function(){
    return(c("SPEC"))
} 

majorpos.semantic <- function(){
    return(c("WW", "ADJ", "N"))
}

majorpos.svo <- function(){
    return(c("WW", "ADJ", "N"))
}

majorpos.svo <- function(){
    return(c("WW", "ADJ", "N"))
}



get.lemma.synonyms.dutch <- function(){
    #return(lemma.synonyms.dutch$lemma)
}
  

special.majorpos.punctuation <- majorpos.punctuation
special.majorpos.special     <- majorpos.special    
special.majorpos.semantic    <- majorpos.semantic   
special.majorpos.svo         <- majorpos.svo        
 
