

f_unigram_word <- function(dtfrog, 
                           dictionary, 
                           dictionary.token                      = "word", 
                           return.measure                        = "count",
                           return.data.type                      = "wide",
                           degree                                = 0,
                           prefix                                = "f_unigram_word_",
                           dataset.token			             = "word", 
                           dataset.token.transformed             = "word",
                           dataset.exclude                       = NULL,
                           dataset.exclude.frequency             = NULL,
                           dataset.transformed.exclude           = NULL,
                           dataset.transformed.exclude.frequency = NULL ){
    
	return(
	    f_ngram(
	        dtfrog                                = dtfrog,                                     
	        dictionary                            = dictionary,                                 
	        dictionary.token                      = dictionary.token,                           
	        degree                                = degree,                                     
	        prefix                                = prefix,                                     
	        return.measure                        = return.measure,                             
	        return.data.type                      = return.data.type,                           
	        dataset.token			              = dataset.token ,                             
	        dataset.token.transformed             = dataset.token.transformed,                  
            dataset.exclude                       = dataset.exclude,                            
            dataset.exclude.frequency             = dataset.exclude.frequency,                  
            dataset.transformed.exclude           = dataset.transformed.exclude,                
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency       
        )
    )
} 
 

f_unigram_lemma <- function(dtfrog, 
                            dictionary, 
                            dictionary.token                      = "lemma", 
                            return.measure                        = "count",
                            return.data.type                      = "wide",
                            degree                                = 0,
                            prefix                                = "f_unigram_word_",
                            dataset.token			              = "lemma", 
                            dataset.token.transformed             = "lemma",
                            dataset.exclude                       = NULL,
                            dataset.exclude.frequency             = NULL,
                            dataset.transformed.exclude           = NULL,
                            dataset.transformed.exclude.frequency = NULL ){
    
	return(
	    f_ngram(
	        dtfrog                                = dtfrog,                                     
	        dictionary                            = dictionary,                                 
	        dictionary.token                      = dictionary.token,                           
	        degree                                = degree,                                     
	        prefix                                = prefix,                                     
	        return.measure                        = return.measure,                             
	        return.data.type                      = return.data.type,                           
	        dataset.token			              = dataset.token ,                             
	        dataset.token.transformed             = dataset.token.transformed,                  
            dataset.exclude                       = dataset.exclude,                            
            dataset.exclude.frequency             = dataset.exclude.frequency,                  
            dataset.transformed.exclude           = dataset.transformed.exclude,                
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency       
        )
    )
} 

f_unigram_majorpos <- function(dtfrog, 
                               dictionary, 
                               dictionary.token                      = "majorpos", 
                               return.measure                        = "count",
                               return.data.type                      = "wide",
                               degree                                = 0,
                               prefix                                = "f_unigram_majorpos_",
                               dataset.token			             = "majorpos", 
                               dataset.token.transformed             = "majorpos",
                               dataset.exclude                       = NULL,
                               dataset.exclude.frequency             = NULL,
                               dataset.transformed.exclude           = NULL,
                               dataset.transformed.exclude.frequency = NULL ){
                               
	return(
	    f_ngram(
	        dtfrog                                = dtfrog,                                     
	        dictionary                            = dictionary,                                 
	        dictionary.token                      = dictionary.token,                           
	        degree                                = degree,                                     
	        prefix                                = prefix,                                     
	        return.measure                        = return.measure,                             
	        return.data.type                      = return.data.type,                           
	        dataset.token			              = dataset.token ,                             
	        dataset.token.transformed             = dataset.token.transformed,                  
            dataset.exclude                       = dataset.exclude,                            
            dataset.exclude.frequency             = dataset.exclude.frequency,                  
            dataset.transformed.exclude           = dataset.transformed.exclude,                
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency       
        )
    )
} 

 
f_unigram_pos <-      function(dtfrog, 
                               dictionary, 
                               dictionary.token                      = "pos", 
                               return.measure                        = "count",
                               return.data.type                      = "wide",
                               degree                                = 0,
                               prefix                                = "f_unigram_pos_",
                               dataset.token			             = "pos", 
                               dataset.token.transformed             = "pos",
                               dataset.exclude                       = NULL,
                               dataset.exclude.frequency             = NULL,
                               dataset.transformed.exclude           = NULL,
                               dataset.transformed.exclude.frequency = NULL ){
                               
	return(
	    f_ngram(
	        dtfrog                                = dtfrog,                                     
	        dictionary                            = dictionary,                                 
	        dictionary.token                      = dictionary.token,                           
	        degree                                = degree,                                     
	        prefix                                = prefix,                                     
	        return.measure                        = return.measure,                             
	        return.data.type                      = return.data.type,                           
	        dataset.token			              = dataset.token ,                             
	        dataset.token.transformed             = dataset.token.transformed,                  
            dataset.exclude                       = dataset.exclude,                            
            dataset.exclude.frequency             = dataset.exclude.frequency,                  
            dataset.transformed.exclude           = dataset.transformed.exclude,                
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency       
        )
    )
} 

  
f_unigram_synset <-   function(dtfrog, 
                               dictionary, 
                               dictionary.token                      = "synset", 
                               return.measure                        = "count",
                               return.data.type                      = "wide",
                               degree                                = 0,
                               prefix                                = "f_unigram_pos_",
                               dataset.token			             = "lemma", 
                               dataset.token.transformed             = "synset",
                               dataset.exclude                       = NULL,
                               dataset.exclude.frequency             = NULL,
                               dataset.transformed.exclude           = NULL,
                               dataset.transformed.exclude.frequency = NULL ){
                               
	return(
	    f_ngram(
	        dtfrog                                = dtfrog,                                     
	        dictionary                            = dictionary,                                 
	        dictionary.token                      = dictionary.token,                           
	        degree                                = degree,                                     
	        prefix                                = prefix,                                     
	        return.measure                        = return.measure,                             
	        return.data.type                      = return.data.type,                           
	        dataset.token			              = dataset.token ,                             
	        dataset.token.transformed             = dataset.token.transformed,                  
            dataset.exclude                       = dataset.exclude,                            
            dataset.exclude.frequency             = dataset.exclude.frequency,                  
            dataset.transformed.exclude           = dataset.transformed.exclude,                
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency       
        )
    )
} 
 
f_unigram_hyperonym <-   function(dtfrog, 
                                  dictionary, 
                                  dictionary.token                      = "hyperonym", 
                                  return.measure                        = "count",
                                  return.data.type                      = "wide",
                                  degree                                = 0,
                                  prefix                                = "f_unigram_hyp_",
                                  dataset.token			                = "lemma", 
                                  dataset.token.transformed             = "hyperonym",
                                  dataset.exclude                       = NULL,
                                  dataset.exclude.frequency             = NULL,
                                  dataset.transformed.exclude           = NULL,
                                  dataset.transformed.exclude.frequency = NULL ){
                               
	return(
	    f_ngram(
	        dtfrog                                = dtfrog,                                     
	        dictionary                            = dictionary,                                 
	        dictionary.token                      = dictionary.token,                           
	        degree                                = degree,                                     
	        prefix                                = prefix,                                     
	        return.measure                        = return.measure,                             
	        return.data.type                      = return.data.type,                           
	        dataset.token			              = dataset.token ,                             
	        dataset.token.transformed             = dataset.token.transformed,                  
            dataset.exclude                       = dataset.exclude,                            
            dataset.exclude.frequency             = dataset.exclude.frequency,                  
            dataset.transformed.exclude           = dataset.transformed.exclude,                
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency       
        )
    )
} 
 
   
# Set meta properties for feature-construction functions

# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (1) Word unigram counts, tf-idf, etc.																						                   #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_unigram_word, "name")                                         <- "Word unigram statistics"
attr(f_unigram_word, "description")                                  <- "Word unigram" 
attr(f_unigram_word, "feature_class")                                <- "lexicographic"
attr(f_unigram_word, "feature_sub_class")                            <- "word"

attr(f_unigram_word, "datasource")				                     <- "frog"

attr(f_unigram_word, "dataset.token")								 <- "word"
attr(f_unigram_word, "dataset.token.transformed")					 <- "word"

attr(f_unigram_word, "degree")					                     <- 0        
attr(f_unigram_word, "proximity.criterium")                          <- "sentence"       
  
attr(f_unigram_word, "dictionary.prefix")                            <- "w_"  
attr(f_unigram_word, "dictionary.token")			                 <- "word"

attr(f_unigram_word, "dictionary.override")			                 <- FALSE  

attr(f_unigram_word, "dictionary.exclude")						     <- list(docids    = NULL, 
																			 majorpos  = c("LET","SPEC","TW"), 
																			 words     = NULL,   
																			 lemmas    = NULL)
																			 
attr(f_unigram_word, "dictionary.exclude.frequency")				<- list(ll = .04,  ul = .80)                                                                     																			 
# ---------------------------------------------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (2) Lemma unigram counts, tf-idf, etc.																						               #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_unigram_lemma, "name")                                          <- "Lemma unigram statistics"
attr(f_unigram_lemma, "description")                                   <- "Lemma unigram" 
attr(f_unigram_lemma, "feature_class")                                 <- "lexicographic"
attr(f_unigram_lemma, "feature_sub_class")                             <- "lemma"

attr(f_unigram_lemma, "datasource")				                       <- "frog"

attr(f_unigram_lemma, "dataset.token")								   <- "lemma"
attr(f_unigram_lemma, "dataset.token.transformed")					   <- "lemma"

attr(f_unigram_lemma, "degree")					                       <- 0        
attr(f_unigram_lemma, "proximity.criterium")                           <- "sentence"      

attr(f_unigram_lemma, "dictionary.token")							   <- "lemma"
attr(f_unigram_lemma, "dictionary.prefix")                             <- "l_"  
attr(f_unigram_lemma, "dictionary.override")			               <- FALSE  
attr(f_unigram_lemma, "dictionary.exclude")						       <- list(docids    = NULL, 
																			   majorpos  = c("LET","SPEC","TW"), 
																			   words     = NULL,   
																		  	   lemmas    = NULL)
																			   
attr(f_unigram_lemma, "dictionary.exclude.frequency")				   <- list(ll = .04,  ul = .80)                                                                     																			 

 
# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (3) Part-of-speech (fine-grained) counts, tf-idf, etc.																						                   #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_unigram_pos, "name")										       <- "Part-of-speech unigram statistics"
attr(f_unigram_pos, "description")                                     <- "Part-of-speech unigram statistics" 
attr(f_unigram_pos, "feature_class")                                   <- "syntactic"
attr(f_unigram_pos, "feature_sub_class")                               <- "pos"
                                                                       
attr(f_unigram_pos, "datasource")				                       <- "frog"
                                                                       
attr(f_unigram_pos, "dataset.token")								   <- "lemma"
attr(f_unigram_pos, "dataset.token.transformed")					   <- "lemma"

attr(f_unigram_pos, "degree")					                       <- 0        
attr(f_unigram_pos, "proximity.criterium")                             <- "sentence"            

attr(f_unigram_pos, "dictionary.prefix")                               <- "pos_"                                                                          
attr(f_unigram_pos, "dictionary.override")			                   <- FALSE  
                        
attr(f_unigram_pos, "dictionary.token")								   <- "lemma"
						
attr(f_unigram_pos, "dictionary.exclude")						       <- list(docids    = NULL, 
                                                                               majorpos  = c("LET","SPEC","TW"), 
																  	           words     = NULL,   
																  	           lemmas    = NULL)
																  	    	 
attr(f_unigram_pos, "dictionary.exclude.frequency")				       <- list(ll = .04,  ul = .80)   
 
 
# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (4) Part-of-speech (major types; majorpos) counts, tf-idf, etc.																			   #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_unigram_majorpos, "name")									    <- "Part-of-speech unigram statistics"
attr(f_unigram_majorpos, "description")                                 <- "Part-of-speech unigram statistics" 
attr(f_unigram_majorpos, "feature_class")                               <- "syntactic"
attr(f_unigram_majorpos, "feature_sub_class")                           <- "pos"

attr(f_unigram_majorpos, "datasource")			                        <- "frog"

attr(f_unigram_majorpos, "dataset.token")								<- "majorpos"
attr(f_unigram_majorpos, "dataset.token.transformed")					<- "majorpos"


attr(f_unigram_majorpos, "degree")				                        <- 0        
attr(f_unigram_majorpos, "proximity.criterium")                         <- "sentence"          
  
attr(f_unigram_majorpos, "dictionary.token")							<- "majorpos"   
attr(f_unigram_majorpos, "dictionary.override")			                <- FALSE  
attr(f_unigram_majorpos, "dictionary.prefix")                           <- "majorpos_"    

attr(f_unigram_majorpos, "dictionary.exclude")						    <- list(docids    = NULL, 
																	            majorpos  = c("LET","SPEC","TW"), 
																	       	    words     = NULL,   
																		        lemmas    = NULL)
																			 
attr(f_unigram_majorpos, "dictionary.exclude.frequency")				<- list(ll = .04,  ul = .80)   
  

# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (5) Synonym-classe unigram counts, tf-idf, etc.																						       #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_unigram_synset, "name")                                       <- "Synonym-set unigram statistics"
attr(f_unigram_synset, "description")                                <- "Synonym-set unigram" 
attr(f_unigram_synset, "feature_class")                              <- "semantic"
attr(f_unigram_synset, "feature_sub_class")                          <- "lemma"

attr(f_unigram_synset, "datasource")			                     <- "frog"

attr(f_unigram_synset, "dataset.token")								 <- "lemma"
attr(f_unigram_synset, "dataset.token.transformed")				     <- "synset"


attr(f_unigram_synset, "degree")				                     <- 0        
attr(f_unigram_synset, "proximity.criterium")                        <- "sentence"       
  
attr(f_unigram_synset, "dictionary.override")			             <- FALSE 
attr(f_unigram_synset, "dictionary.prefix")                          <- "synset_"    
attr(f_unigram_synset, "dictionary.token")						     <- "synset" 

attr(f_unigram_synset, "dictionary.exclude")					     <- list(docids    = NULL, 
																    	 majorpos  = c("LET","SPEC","TW"), 
																    	 words     = NULL,   
																    	 lemmas    = NULL)
																    	 
attr(f_unigram_synset, "dictionary.exclude.frequency")				<- list(ll = .04,  ul = .80)   
   
# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (6) Hyperonym-classe unigram counts, tf-idf, etc.																						                   #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_unigram_hyperonym, "name")                                       <- "Hyperonym unigram statistics"
attr(f_unigram_hyperonym, "description")                                <- "Hyperonym unigram" 
attr(f_unigram_hyperonym, "feature_class")                              <- "semantic"
attr(f_unigram_hyperonym, "feature_sub_class")                          <- "lemma"

attr(f_unigram_hyperonym, "datasource")			                        <- "frog"
attr(f_unigram_hyperonym, "dataset.token")								<- "lemma"
attr(f_unigram_hyperonym, "dataset.token.transformed")				    <- "hyperonym" 

attr(f_unigram_hyperonym, "dictionary.token")							<- "hyperonym"
attr(f_unigram_hyperonym, "dictionary.prefix")                          <- "hyperonym_"   

attr(f_unigram_hyperonym, "degree")				                        <- 0    
    
attr(f_unigram_hyperonym, "proximity.criterium")                        <- "sentence"       
attr(f_unigram_hyperonym, "dictionary.override")			            <- FALSE  
attr(f_unigram_hyperonym, "dictionary.exclude")					        <- list(docids    = NULL, 
																       	        majorpos  = c("LET","SPEC","TW"), 
																       	        words     = NULL,   
																       	        lemmas    = NULL)
																    	 
attr(f_unigram_hyperonym, "dictionary.exclude.frequency")			    <- list(ll = .04,  ul = .80)      
    
   
f_ngram <- function(dtfrog,  
					dictionary,
					prefix                                =  "f_unigram_",  
					dataset.token			              =  "lemma", 
                    dataset.token.transformed             =  "lemma", 
					dictionary.token		              =  "lemma",
                    degree                                =   0,
					return.measure                        =  "count",
					return.data.type                      =  "wide",
                    dataset.exclude                       =   NULL,
                    dataset.exclude.frequency             =   NULL, 
                    dataset.transformed.exclude           =   list(docids    = NULL, 
                                                                   majorpos  = c("LET","SPEC","TW"), 
                                                                   words     = NULL,   
                                                                   lemmas    = NULL), 
                    dataset.transformed.exclude.frequency  = list(ll = .05, ul = .80)){
                       
 
    dtfrog <- dtfrog %>% data.table %>% FixStringsFactors
 
 
    cat("Degree:", degree)
    
	if(!return.measure %chin% c("count", "binary", "mean")) {
		stop("\r\n\t Ongeldige waarde voor return.measure; geldige waarden zijn: 'count', 'binary' of 'mean'. \r\n Kneus.")
	} 
	
	if(!return.data.type %chin% c("wide", "long", "dtm")) {
		stop("\r\n\t Ongeldige waarde voor return.data.type; geldige waarden zijn: 'long', 'wide', 'dtm'. \r\n Kneus.")
	} 
     	 
	# --------------------------------------------------------------------------------------#  
	# Pas de dictionary toe op het frog-object                                              #  
    # --------------------------------------------------------------------------------------# 
	#                                                                                       #  
	#      Toelichting:                                                                     #  
	#      ---------------------------------------------------------------------------------#  
	#      We willen van alle entries in de dictionary weten of en hoe vaak ze voorkomen    #  
	#      in een document/zin/documentgroep in het frog-object.                            #   
	# 	                                                                                    #  
	#      Dit kunnen we bewerkstelligen door:                                              #
	#                                                                                       #
	#       (a) een full-factorial data.table te maken van het Carthesiaans product van     #
	#           alle unieke document-ids en alle unieke entries in de dictionary;           #
	#                                                                                       #
    #       (b) o.b.v. het originele frog-object een geaggregeerde data.table te maken      #
	#           met counts van document -- entry                                            #
	#                                                                                       #
	#       (c) de data.tables van (a) en (b) te mergen;                                    #
    #                                                                                       #
    #       (d) de counts van de niet-voorkomende entries (NA in het gemergede object) op   #
	#           nul te zetten.                                                              #
	#                                                                                       # 
	# --------------------------------------------------------------------------------------# 
	
	# (a) Maak een full factorial data.table met het CJ() commando (cross join)
    dt.ff <- data.table()
	
    cat("Dict.dataset.token.transformed ", "\r\n") 
    
     
	(dt.ff <- CJ(docid = dtfrog$docid, word     = dictionary$word,     unique = TRUE)) %if% (dictionary.token == "word") 
	(dt.ff <- CJ(docid = dtfrog$docid, lemma    = dictionary$lemma,    unique = TRUE)) %if% (dictionary.token == "lemma") 
	(dt.ff <- CJ(docid = dtfrog$docid, majorpos = dictionary$majorpos, unique = TRUE)) %if% (dictionary.token == "majorpos") 
	(dt.ff <- CJ(docid = dtfrog$docid, ngram    = dictionary$ngram,    unique = TRUE)) %if% (dictionary.token == "ngram")
 
	# (b) Filter het originele frog-object op woorden die _wel_  in de dictionary voorkomen.       
	dt.filtered <- dtfrog
    
    (  dt.filtered <- dtfrog[word     %chin% unique(dictionary$word),])		%if% (dictionary.token == "word")
	(  dt.filtered <- dtfrog[lemma    %chin% unique(dictionary$lemma),]) 	%if% (dictionary.token == "lemma") 
 	(  dt.filtered <- dtfrog[majorpos %chin% unique(dictionary$majorpos),]) %if% (dictionary.token == "majorpos") 
 	(  dt.filtered <- dtfrog[ngram    %chin% unique(dictionary$ngram),]) 	%if% (dictionary.token == "ngram")
	(  dt.filtered <- dtfrog[synset   %chin% unique(dictionary$synset),]) 	%if% (dictionary.token == "synset")																							   
  
     
	# (c) Maak counts per entry                                                                    
                                               
    dt.filtered[, count := .N, by = c("docid",dataset.token.transformed)]                                     
    dt.filtered <- dt.filtered[, c("docid", dictionary.token = dataset.token.transformed, "count"), with=F] %>% unique  
 

	# (d) Merge dt.ff en dt.filtered                                                               
    (by.merge <- c("docid","word"))       %if% (dictionary.token == "word")
    (by.merge <- c("docid","lemma"))      %if% (dictionary.token == "lemma") 
    (by.merge <- c("docid","majorpos"))   %if% (dictionary.token == "majorpos") 
    (by.merge <- c("docid","ngram"))      %if% (dictionary.token == "ngram")
    (by.merge <- c("docid","synset"))     %if% (dictionary.token == "synset")
	
    cat("\r\n\tdataset: dt.ff \t\t\r\n"); dt.ff %>% print   
    cat("\r\n\tdataset: dt.filtered\t\t\r\n"); dt.filtered %>% print   
    
	dt.merged <- merge(x     = dt.ff,  
                       y     = dt.filtered,  
                       by    = by.merge,    
                       all.x = TRUE,  
                       all.y = FALSE)  
	    
    dt.merged[is.na(count ), count := 0]
	
	
	# (e) merge eventuele overige properties uit de dictionary                                     
    (by.merge_properties <- c("word"))       %if% (dictionary.token == "word")
    (by.merge_properties <- c("lemma"))      %if% (dictionary.token == "lemma") 
    (by.merge_properties <- c("majorpos"))   %if% (dictionary.token == "majorpos") 
	(by.merge_properties <- c("ngram"))      %if% (dictionary.token == "ngram") 
	 
	dt.merged <- merge(x     = dt.merged,   
                       y     = dictionary,  
                       by    = by.merge_properties,  
                       all.x = T,  
                       all.y = F,allow.cartesian=T)
	                    
	(setkeyv(dt.merged,     c("docid",dictionary.token))) %if% (dictionary.token %in% c("word","lemma","majorpos","ngram", "synset","hyperonym")) 
    
#	(setcolorder(dt.merged, c("docid",dictionary.token, colnames(dt.merged) %-% c("docid",dictionary.token)))) %if% (dictionary.token %in% c("word","lemma","majorpos","ngram", "synset","hyperonym")) 
	
	retval <- dt.merged
	
	if(return.data.type == "long"){ 
    
		retval <- dt.merged 
	
    } else {  
     
	    return.formula <- as.formula("docid~" %+% dictionary.token %+%"Name")
        
        value.var      <- c("count")
        cat("\r\nMerged data:\r\n\r\n")
        dt.merged %>% print
        
        retval <- dcast(data       = dt.merged,   
                        formula    = return.formula,   
                        value.var  = c(value.var))  %>% data.table
                        
        retval
			  
	} 
	return(retval %>% data.table %>% FixStringsFactors)
}