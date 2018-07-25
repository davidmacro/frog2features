# get.global.timeout <- function(x){
#     
#  # switch(x, 
#  #     apply_tag         =  get.global.timeout("max.timeout"),
#  #     create_dictionary =  get.global.timeout("max.timeout"),
#  #     max.timeout = 13 ,
#  #     19
#  # ) %>% return
#      
# # }
# #  
# valid_match_types <- c("match.data.table.expression", "match.regular.expresion")
 
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

#  
# properties.to.serialize <- c("tokens", 
#     "items",
#     "filters", 
#     "replace.items", 
#     "tag.items",
#     "token.by.x", 
#     "token.by.y")

 
