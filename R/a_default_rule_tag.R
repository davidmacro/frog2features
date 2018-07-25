default_rule_tag <- function(x, override = NULL, ...){
    
    opts.dotted  <- list(...)    
    override     <- list() %if% is.null(override)
      
    if(opts.dotted %>% length %>% is_greater_than(0)){ 
        override %<>% modifyList(opts.dotted, keep.null = T)
    }  
    
    newrule <- switch(x,
                      
        tag.persons = list(
			type             = "apply.tag",
			where.data.table.expression = "(pos %like% \"SPEC\\\\(deeleigen\\\\)\") & (ner %like% \"B\\\\-PER\") & (ner %like% \"B\\\\-ORG\" %>% not)",
			tag.column.name  = "is_person" ,
			match.type       = "match.data.table.expression",
			casefold         = 1,
			tagged.value     = 1,
			tagged.value.not = 0,
			set.not          = 1
		),
                      
		tag.websites = list(
			type             = "apply.tag",
			where.data.table.expression = "((word %>% casefold) %like% \"\\\\.nl\" | (word %>% casefold) %like% \"\\\\.be\" | (word %>% casefold) %like% \"http\" | (word %>% casefold) %like% \"\\\\.com\" | (word %>% casefold) %like% \"www\\\\.\")",
			tag.column.name  = "is_website", 
			match.type       = "match.data.table.expression",
			casefold         = 1,
			set.not          = 1
		),
		
		tag.words.encryption = list(
			type             = "apply.tag",
			where.token      = "word",
			where.list       = "entities.dutch.encryption",
			match.type       = "match.entity.list",
			tag.column.name  = "is_encryption" , 
			tagged.value     = 1,
			tagged.value.not = 0,
			casefold         = 1,
			set.not          = 1
		),  
		
		tag.words.ddos = list(
			type             = "apply.tag",
			where.token      = "word",
			where.list       = "entities.dutch.ddos",
			token            = "is_ddos" ,
			tagged.value     = 1,
			tagged.value.not = 0,
			casefold         = 1
		),
		
		tag.words.hacking = list(
			type             = "apply.tag",
			where.token      = "word",
			where.list       = "entities.dutch.hacking",
			token            = "is_hacking" ,
			tagged.value     = 1,
			tagged.value.not = 0,
			match.type       = "match.entity.list",
			casefold         = 1
		),
		
		tag.dutch.banks = list(
			type             = "is_bank",
			where.token      = "lemma",
			where.list       = "dictionary.dutch.banks",  
			token            = "is_bank" ,
			tagged.value     = 1,
			tagged.value.not = 0,
			match.type       = "match.entity.list",
			casefold         = 1
		), 
		
		tag.dutch.municipality = list(
			type             = "is_municipality",
			where.token      = "word",
			entities         =  "entities.dutch.municipalities", 
			match.type       = "match.entity.list",
			tag.column.name  = "is_municipality" 
			
		) 
    ) 
    
    if(length(override)>0){
        newrule %<>% modifyList(override, keep.null=T)       
    }
    
    if(newrule$allow.replay %>% is.null){
        newrule$allow.replay <- 1
    } 
    
    return(newrule)  
}



