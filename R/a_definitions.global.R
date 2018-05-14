get.global.timeout <- function(x){
    
    switch(x, 
        apply_tag         =  get.global.timeout("max.timeout"),
        create_dictionary =  get.global.timeout("max.timeout"),
        max.timeout = 13 ,
        19
    ) %>% return
     
}

majorpos.punctuation <- function(){
    return(c("LET", "SPEC"))
} 
majorpos.semantic <- function(){
    return(c("WW", "ADJ", "N"))
}

majorpos.svo <- function(){
    return(c("WW", "ADJ", "N"))
}

valid_match_types <- c("match.data.table.expression", "match.regular.expresion")
 
getValidRuleTypes <- function(){
    
    c("idempotent",
      "input",
      "apply.filter",
      "apply.tag.chain",
      "apply.tag",
      "create.ngram",
      "output",
      "merge", 
      "create.dictionary",
      "save.dictionary", 
      "load.dictionary",
      "load.input", 
      "construct.features", 
      "replace.rule",
      "replace.chain") %>% return
  
}



properties.to.serialize <- c("tokens", 
                             "items",
                             "filters", 
                             "replace.items", 
                             "tag.items",
                             "token.by.x", 
                             "token.by.y")


# # Default options for the frogdh objects
# options.frogdh <- function(type = "strict", ...){
# 	
# 	if(type == "strict"){ 
# 		return(list(
#             use_log             = TRUE, 
#             consolidate_initial = TRUE,
#             enable_undo         = TRUE,
#             make_checksums      = TRUE,
#             ...
# 		))  
# 	}
# 	
# 	if(type == "fast"){
# 		return(list(
#             use_log             = TRUE, 
#             consolidate_initial = FALSE,
#             enable_undo         = FALSE,
#             make_checksums      = FALSE,
#             ...
# 		)) 
# 	} 
# 	
# 	if(type == "minimal"){
# 		return(list(
#             use_log             = FALSE,
#             consolidate_initial = FALSE,
#             enable_undo         = FALSE,
#             make_checksums      = FALSE,
#             ...
# 		)) 
# 	}
#  
#  }
#  
# options.frogdh.strict  <- options.frogdh("strict")
# options.frogdh.fast    <- options.frogdh("fast")
# options.frogdh.minimal <- options.frogdh("minimal")


# dictionary.options <- function(type = "strict"){
# 	
# 	if(type == "strict"){ 
# 		return(list(
# 			use_log             = TRUE, 
# 			consolidate_initial = TRUE,
# 			enable_undo         = TRUE,
# 			make_checksums      = TRUE 
# 		)) 
# 	}
# 	
# 	if(type == "fast"){
# 		return(list(
# 			use_log             = TRUE, 
# 			consolidate_initial = FALSE,
# 			enable_undo         = FALSE, 
# 			make_checksums      = FALSE 
# 		)) 
# 	}
# 	
# 	if(type == "minimal"){
# 		return(list(
# 			use_log             = FALSE,
# 			consolidate_initial = FALSE,
# 			enable_undo         = FALSE,
# 			make_checksums      = FALSE 
# 		)) 
# 	}
#  
#  }
#  
# dict.options.strict  <- dictionary.options(type="strict")
# dict.options.fast    <- dictionary.options(type="fast")
# dict.options.minimal <- dictionary.options(type="minimal")
#  
 
 
