


get.lemma.synonyms.dutch <- function(){
    return(lemma.synonyms.dutch$lemma)
}
 
default.filter.rule <- function(x, override = NULL){
    
    newrule <-  switch(x,
        exclude.infrequent.lemmas = list(
			count.type   = "frequency",
			filter.type  = "exclude",
			exclude.what = "outside",
			ll           = 10,
			ul           = Inf, 
			token        = "lemma",
            type         = "apply.filter",
			allow.replay = 0
        ),
			
        exclude.infrequent.ngrams = list(
			count.type   = "frequency",
			filter.type  = "exclude",
			exclude.what = "outside",
			ll           = 10,
			ul           = Inf,
			token        = "ngram",
            type         = "apply.filter",
			allow.replay = 0
        ),			
			
		exclude.dutch.stopwords = list( 
			filter.type    = "exclude",
			exclude.what   = "inside", 
			token          = "lemma",
			items          =  stopwords.dutch.extended(), 
            tag            =  "stopwords.dutch.extended()",
			vertex.label   = "Filter Dutch Stopwords",
			type           = "apply.filter",
			allow.replay   = 1
        ),	
		  
        exclude.dutch.stopwords.lemma = list( 
			filter.type    = "exclude",
			exclude.what   = "inside", 
			token          = "lemma",
			items          =  stopwords.dutch.extended(), 
			tag            =  "stopwords.dutch.extended()",
			vertex.label   = "Filter Dutch Stopwords",
			type           = "apply.filter" ,
			allow.replay   = 1 
        ),
		 
        exclude.majorpos.not.in.default.semantic = list( 
			filter.type    = "exclude",
			exclude.what   = "outside", 
			token          = "lemma",
			items          =  majorpos.semantic() ,
			type           = "apply.filter",
			allow.replay   = 1
        ),			
			
        exclude.lemma.not.in.dutch.synonym.list = list( 
			filter.type    = "exclude",
			exclude.what   = "outside", 
			token          = "lemma",
			items          =  get.lemma.synonyms.dutch() ,
			type           = "apply.filter",
			allow.replay   = 1
        ),			
			 
		exclude.punctuation = list(
		    filter.type    = "exclude",
			exclude.what   = "inside", 
			token          = "majorpos",    
		    items          =  majorpos.punctuation(),
			tag            =  "majorpos.punctuation()",
			type           = "apply.filter",
			allow.replay   = 1
		),
 
		
		include.only.svo = list(
		    filter.type    = "exclude",
			exclude.what   = "outside", 
			token          = "majorpos",    
		    items          =  majorpos.svo(),
            tag            =  "majorpos.svo()",
			type           = "apply.filter",
			allow.replay   = 0
		),
		 
		exclude.rare.word = list(
			count.type     = "proportion",
			filter.type    = "exclude",
			exclude.what   = "outside",
			ll             = 0,
			ul             = 0.5,
			token          = "word" ,
			type           = "apply.filter",
			allow.replay   = 0
        ),
		
		exclude.rare.bigram = list(
			count.type     = "proportion",
			filter.type    = "exclude",
			exclude.what   = "outside",
			ll             = 0,
			ul             = 0.5,
			token          = "bigram" ,
			type           = "apply.filter",
			allow.replay   = 0
        ),
        	
        exclude.frequent.lemma = list(
			count.type   = "frequency",
			filter.type  = "exclude",
			exclude.what = "outside",
			ll           = 2,
			ul           = Inf,
			token        = "lemma"  ,
			type         = "apply.filter",
			allow.replay = 0
        ),
        	
        exclude.frequent.word = list(
        	count.type   = "proportion",
        	filter.type  = "exclude",
        	exclude.what = "outside",
        	ll           = 0,
        	ul           = 0.5,
        	token        = "lemma"   ,
			type         = "apply.filter",
			allow.replay = 0
        ),
        	
        exclude.frequent.bigram = list(
        	count.type   = "frequency",
        	filter.type  = "exclude",
        	exclude.what = "outside",
        	ll           = 2,
        	ul           = Inf,
        	token        = "ngram",
			type         = "apply.filter",
			allow.replay = 0
        ),
        	
        exclude.frequent.word = list(
			count.type   = "proportion",
			filter.type  = "exclude",
			exclude.what = "outside",
			ll           = 0,
			ul           = 0.5,
			token        = "ngram" ,
			type         = "apply.filter",
			allow.replay = 0
        )  
    ) 
     
    if(!is.null(override)){ 
		if(is.list(override)){
			newrule %<>% modifyList(override, keep.null =T)   
		}
    } 
    
    newrule$type <- "apply.filter"
    
    return(newrule)  
}
 
default.replace.chain <- function(x, override = NULL){
    
    newrule <- list(type   = "replace.chain",
         replace.items = list(
             default.replace.rule("substitute.majorpos.count"),
             default.replace.rule("substitute.names.person"),
             default.replace.rule("substitute.website"),
             default.replace.rule("substitute.dutch.banks")
         )  
    ) 
    return(newrule)  
}
    
default.tag.chain <- function(x, override = NULL){
    
    switch(x, 
        newrule = list(
                type   = "apply.tag.chain",  
                     tag.items = list(
                        default.tag.rule("substitute.majorpos.count"),
                        default.tag.rule("substitute.names.person"),
                        default.tag.rule("substitute.website"),
                        default.tag.rule("substitute.dutch.banks")
            )   
        )  
    ) %>% return
} 
 

default.tag.rule <- function(x, override =NULL){
      
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
            match.type       = "match.list",
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
            tag.column.name  = "is_municipality" 
   
        ) 
    ) 
    
    if(!is.null(override)){ 
		if(is.list(override)){
			newrule %<>% modifyList(override, keep.null=T)   
		}
    }  
    
    newrule$allow.replay <- 1
	 
	return(newrule) 
  
}


dictionary.dutch.banks <- function(){
    return(c( "\\\\<Rabo", 
			  "\\\\<rabo", 
			  "\\\\<Sns", 
			  "\\\\<NIBC", 
			  "\\\\<Royal Bank", 
              "\\\\<ING", 
			  "\\\\<SNS", 
              "\\\\<ABN\\\\>", 
			  "AMRO\\\\>",
			  "\\\\<ASN", 
              "\\\\<Knab\\\\>", 
			  "\\\\<DNB\\\\>", 
			  "\\\\<Triodos\\\\>", 
			  "\\\\<Binck\\\\>", 
			  "\\\\<Bunq\\\\>"))
}

    
 

default.replace.rule <- function(x, override = NULL){
    
    newrule <- switch(x,
        substitute.majorpos.count = list(
            type  = "replace.rule",
            where = "majorpos %in% c('TW')",
            token = "lemma",
            value.get = "majorpos",
            prefix = "[",
            suffix = "]"
        ),
        substitute.names.person = list(
            type = "replace.rule",
            where = "(pos %like% \"SPEC\\\\(deeleigen\\\\)\") & (ner %like% \"B\\\\-PER\") & (ner %like% \"B\\\\-ORG\" %>% not)",
            token = "lemma",
            value.expression = NA,
            value  = "persoon",
            prefix = "[",
            suffix = "]"
        ),
        substitute.website = list(
            type = "replace.rule",
            where = "(word %like% \"\\\\.nl\" | word %like% \"\\\\.be\" | word %like% \"http\" | word %like% \"\\\\.com\" | word %like% \"www\\\\.\")",
            token = "lemma",
            value.expression = NA,
            value  = "website",
            prefix = "[",
            suffix = "]"
        ),
        substitute.dutch.banks = list(
            type = "replace.rule",
            where = "word",
            where.list = dictionary.dutch.banks(), 
            token = "lemma",
            value.expression = NA,
            value  = "bank",
            prefix = "[",
            suffix = "]"
        )
    )
    
    if(!is.null(override)){ 
		if(is.list(override)){
			newrule %<>% modifyList(override, keep.null=T)   
		}
    }  
	
	allow.replay <- 1
	 
	return(newrule)  
    
}



default.merge.rule <- function(x, override = NULL){

	newrule <-  switch(x,
	
		merge.lemma.synonym = list( 
		    type              = "merge", 
		    token.by.x        =  c("lemma", "majorpos"),
			token.by.y        =  c("lemma", "majorpos"),
		    token.replacement = "lemma.replacement.2",
			merge.type        = "synset",
			merge.database    = "lemma.synonyms.dutch",
            allow.replay      = 1
		),
		
		merge.lemma.hyperonym = list( 
			token.by.x        = "lemma",
			token.by.y        = "lemma",
			merge.type        = "hyperonymset",
			merge.database    = "default.hyperonym.database",
			degree            = 1,
			allow.replay      = 1
		),
		
		merge.lemma.property = list( 
			token.by.x        = "lemma",
			token.by.y        = "lemma",
			property          = "",
			merge.type        = "synset",
			merge.database    = "default.property.database",
            degree            = 1,
			allow.replay      = 1 
		) 
		
	)
	 	
    if(!is.null(override)){ 
		if(is.list(override)){
			newrule %<>% modifyList(override, keep.null=T)   
		}
    }  
	
	allow.replay <- 1
	
	return(newrule)  
	
} 
  
 
default.dictionary.rule <- function(x, override = NULL, serialized = T){
    
    retval.rule <- getNaRuleList()
    
    newrule <-  switch(x,  
        lemma.unigram              = list(type = "create.dictionary", token = "lemma",    prefix = "lemma_1_"),
        lemma.bigram               = list(type = "create.dictionary", token = "ngram",    prefix = "lemma_2_"),
        lemma.bigram.adjacency     = list(type = "create.dictionary", token = "ngram",    prefix = "lemma_2_"),
        lemma.bigram.syntactic     = list(type = "create.dictionary", token = "ngram",    prefix = "lemma_3_"),
        majorpos.unigram           = list(type = "create.dictionary", token = "majorpos", prefix = "majorpos_1_"),
        majorpos.bigram.adjacency  = list(type = "create.dictionary", token = "ngram",    prefix = "majorpos_2_"),
    )
    
    stop("No token specified") %if% (newrule$token %>% is.null) 
    
    if(!is.null(override)){ 
		if(is.list(override)){
			newrule %<>% modifyList(override)   
		}
    }  
	
    newrule$save.data <- 1
 	
	return(newrule)  
}

default.ngram.rule <- function(x, override = NULL, serialized = T){

    retval.rule <- getNaRuleList()
    
	newrule <-  switch(x,
	
		word.bigram.adjacency        = list(type = "create.ngram", tokens = c("word","word"),                    degree = 1,   proximity.criterium   = "adjacency"),
		word.trigram.adjacency       = list(type = "create.ngram", tokens = c("word","word","word"),             degree = 2,   proximity.criterium   = "adjacency"), 
		
		lemma.bigram.adjacency        = list(type = "create.ngram", tokens = c("lemma","lemma"),                 degree = 1,   proximity.criterium   = "adjacency"),
		lemma.trigram.adjacency       = list(type = "create.ngram", tokens = c("lemma","lemma","lemma"),         degree = 2,   proximity.criterium   = "adjacency"), 
		
		majorpos.bigram.adjacency    = list(type = "create.ngram", tokens = c("majorpos","majorpos"),             degree = 1,   proximity.criterium   = "adjacency"),
		majorpos.trigram.adjacency   = list(type = "create.ngram", tokens = c("majorpos","majorpos","majorpos"),  degree = 2,   proximity.criterium   = "adjacency"), 
		 
        word.bigram.syntactic        = list(type = "create.ngram", tokens = c("word","word"),                    degree = 1,   proximity.criterium   = "syntactic"),
		word.trigram.syntactic       = list(type = "create.ngram", tokens = c("word","word","word"),             degree = 2,   proximity.criterium   = "syntactic"), 
		
		lemma.bigram.syntactic        = list(type = "create.ngram", tokens = c("lemma","lemma"),                 degree = 1,   proximity.criterium   = "syntactic"),
		lemma.trigram.syntactic       = list(type = "create.ngram", tokens = c("lemma","lemma","lemma"),         degree = 2,   proximity.criterium   = "syntactic"), 
		
		majorpos.bigram.syntactic    = list(type = "create.ngram", tokens = c("majorpos","majorpos"),             degree = 1,   proximity.criterium   = "syntactic"),
		majorpos.trigram.syntactic   = list(type = "create.ngram", tokens = c("majorpos","majorpos","majorpos"),  degree = 2,   proximity.criterium   = "syntactic"), 
		 
		FALSE
	)
	
	stop("No token specified")                             %if% (newrule$tokens %>% is.null)
	stop("Degree not specified")                           %if% (newrule$degree %>% is.null)
	stop("Degree should be a positive, non-zero integer.") %if% (newrule$degree %>% is.integer)
	 	
    if(newrule$filters %>% is.null){
        
        newrule$filters <- list()
        
        for(i in 1:(newrule$degree+1)){
            newrule$filters[["majorpos_" %+% i]] <- list(default.filter.rule("exclude.punctuation")     %>% serialize_rule)
            newrule$filters[["lemma_" %+% i]]    <- list(default.filter.rule("exclude.dutch.stopwords") %>% serialize_rule)
        }  
    }
	
    if(!is.null(override)){ 
		if(is.list(override)){
			newrule %<>% modifyList(override)   
		}
    }  
	
	newrule$allow.replay <- T
	
    for(token_filter in newrule$filters){
        for(filter in token_filter){
            if(filter$allow.replay %>% not){
                newrule$allow.replay <- F
            }
        }
    }
 
	return(newrule)  
	
}


serialize_rule <- function(rule){
 
    for(name in names(rule)){  
         
        if(any(rule[[name]] %>% is.list(),
               rule[[name]] %>% is.atomic %>% not, 
               rule[[name]] %>% is.recursive,
               rule[[name]] %>% length %>% is_greater_than(1) )){
       
           
            if(name %in% properties.to.serialize){ 
                rule[[name]] <- {rule[[name]] %>% enquote %>% as.character}[[2]] 
            }
      
        }   
             
    }
    rule %>% return
}


unserialize_rule <- function(rule){
       
    for(name in names(rule)){ 
         
        if(any(rule[[name]] %>% is.list(),
               rule[[name]] %>% is.atomic %>% not, 
               rule[[name]] %>% is.recursive,
               rule[[name]] %>% length %>% is_greater_than(1))){
            

        } else { 
            if(name %in% properties.to.serialize){   
                 try(rule[[name]] <- rule[[name]] %>% parse(text = .) %>% eval, silent=T)  
            } 
        }
         
    } 
    rule %>% return
}
   

getEmptyRuleList <- function(){
    
    return(list( 
    	    type                = character(),
            order               = integer(),
            degree              = integer(),
            proximity.criterium = character() ,
            count.type          = character(),
            filter.type         = character(),
            include.what        = character(),
            exclude.what        = character(),
    	    dictionary.filename = character(),
            ll                  = numeric(),
            ul                  = numeric(),
            token               = character(),
            token.by.x          = character(),
            token.by.y          = character(),
            merge.type          = character(),
            items               = character(),
            property            = character(),
            tag                 = character(),
    	    bigram_1_filter     = character(),
    	    bigram_2_filter     = character(),
            trigram_1_filter    = character(),
    	    trigram_2_filter    = character(),
    	    trigram_3_filter    = character(),
            vertex.label        = character(),
            vertex.shape        = character(),
            vertex.color        = character(),
            vertex.size         = numeric(),
            vertex.size2        = numeric(),
    	    call                = character(),
    	    enabled             = integer(),
    	    save.data           = integer(),
            prefix              = character(),
    	    suffix              = character(),
    	    value               = character(),
    	    value.expression    = character(),
    	    value.get           = character(),
    	    tagged.value                = numeric(),
            tagged.value.not            = numeric(),
            tagged.value.expression     = character(),
            tagged.value.expression.not = character(),
    	    where               = character(),
    	    datasetid           = integer(),
    	    allow.replay        = integer())) 
}
 

getNaRuleList <- function(){
    
    return(list( 
    	    type          = as.character(NA),
            order         = as.integer(NA),
            degree        = as.integer(NA),
            proximity.criterium = as.character(NA) ,
            count.type    = as.character(NA),
            filter.type   = as.character(NA),
            include.what  = as.character(NA),
            exclude.what  = as.character(NA),
    	    dictionary.filename = as.character(NA),
            ll            = as.numeric(NA),
            ul            = as.numeric(NA),
            token         = as.character(NA),
            token.by.x    = as.character(NA),
            token.by.y    = as.character(NA),
            merge.type    = as.character(NA),
            items         = as.character(NA),
            property      = as.character(NA),
            tag           = as.character(NA),
    	    bigram_1_filter     = as.character(NA),
    	    bigram_2_filter     = as.character(NA),
            trigram_1_filter    = as.character(NA),
    	    trigram_2_filter    = as.character(NA),
    	    trigram_3_filter    = as.character(NA),
            vertex.label  = as.character(NA),
            vertex.shape  = as.character(NA),
            vertex.color  = as.character(NA),
            vertex.size   = as.numeric(NA),
            vertex.size2  = as.numeric(NA),
    	    call          = as.character(NA),
    	    enabled       = as.integer(1),
    	    save.data     = as.integer(0),
    	    prefix        = as.character(""),
    	    suffix        = as.character(""),
    	    value         = as.character(NA),
    	    value.expression = as.character(NA),
            value.get = as.character(NA),
            tagged.value                = as.numeric(1),
            tagged.value.not            = as.numeric(0),
            tagged.value.expression     = as.character(NA),
            tagged.value.expression.not = as.character(NA),
    	    where         = as.character(NA),
    	    datasetid     = as.integer(NA),
            allow.replay  = as.integer(1))) 
} 
 
is.valid.dt.rule <- function(rule){
    
    if("data.table" %in% (rule %>% class)){
        rule %<>% as.list
    }
    
    if("list" %notin% (rule %>% class)){
        return(FALSE)
    }
    
    # Make an empty rule object
    tmp.rule <- getNaRuleList()
    
    # Replace all provided rule-components
    tmp.rule %<>% modifyList(rule, keep.null = T)

    return(TRUE)
    
} 

is.valid.filter.rule <- function(rule){
    
    # Type 1: filter based on counts 
    type1 <- all(rule %has% "filter.type",       
                 rule %has% "count.type",     
                 rule %has% "include.what",   
    			 rule %has% "ll",             
                 rule %has% "ul" ,            
                 rule %has% "token")          
            
    
    # Type 2: filter based on counts 
    type2 <- all(rule %has% "rule.type",
                 rule %has% "count.type",
                 rule %has% "exclude.what",
                 rule %has% "ll",
                 rule %has% "ul",          
                 rule %has% "token",
                 rule %has% "items")          
    
    
    
    return(retval)
}

is.valid.merge.rule <- function(rule){
    
    return(retval)
}

is.valid.ngram.rule <- function(rule){
     
    
    return(retval)
}







