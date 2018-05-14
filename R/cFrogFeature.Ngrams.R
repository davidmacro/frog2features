f_ngram_word <- function(dtfrog, 
                         dictionary, 
                         dictionary.token                      = "ngram", 
                         return.measure                        = "count",
                         return.data.type                      = "wide",
                         degree                                = 1,
                         prefix                                = "f_ngram_word_",
                         dataset.token			               = "word", 
                         dataset.token.transformed             = "ngram",
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
 

f_ngram_lemma <- function(dtfrog, 
                            dictionary, 
                            dictionary.token                      = "ngram", 
                            return.measure                        = "count",
                            return.data.type                      = "wide",
                            degree                                = 1,
                            prefix                                = "f_ngram_lemma_",
                            dataset.token			              = "lemma", 
                            dataset.token.transformed             = "ngram",
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

f_ngram_majorpos <- function(dtfrog, 
                               dictionary, 
                               dictionary.token                      = "majorpos", 
                               return.measure                        = "count",
                               return.data.type                      = "wide",
                               degree                                = 1,
                               prefix                                = "f_unigram_majorpos_",
                               dataset.token			             = "majorpos", 
                               dataset.token.transformed             = "ngram",
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

 
f_ngram_pos <-      function(dtfrog, 
                               dictionary, 
                               dictionary.token                      = "pos", 
                               return.measure                        = "count",
                               return.data.type                      = "wide",
                               degree                                = 1,
                               prefix                                = "f_ngram_pos_",
                               dataset.token			             = "pos", 
                               dataset.token.transformed             = "ngram",
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

  
f_ngram_synset <-   function(dtfrog, 
                              dictionary, 
                              dictionary.token                      = "synset", 
                              return.measure                        = "count",
                              return.data.type                      = "wide",
                              degree                                = 1,
                              prefix                                = "f_ngram_synset_",
                              dataset.token			                = "lemma", 
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
 
f_ngram_hyperonym <-   function(dtfrog, 
                                dictionary, 
                                dictionary.token                      = "hyperonym", 
                                return.measure                        = "count",
                                return.data.type                      = "wide",
                                degree                                = 1,
                                prefix                                = "f_ngram_hyperonym_",
                                dataset.token			              = "lemma", 
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
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency   )
    )
}  

f_ngram_syntactic <- function(dtfrog,  
                              dictionary,  
                              dictionary.token                      = "ngram",  
                              return.measure                        = "count", 
                              return.data.type                      = "wide", 
                              degree                                = 1, 
                              prefix                                = "f_ngram_syntactic", 
                              dataset.token			                = "lemma",  
                              dataset.token.transformed             = "ngram", 
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
            dataset.transformed.exclude.frequency = dataset.transformed.exclude.frequency  )
    )
}    
   
# Set meta properties for feature-construction functions

# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (1) Word unigram counts, tf-idf, etc.                                                                                                        #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_ngram_word, "name")                         <- "Word unigram statistics"
attr(f_ngram_word, "description")                  <- "Word unigram" 
attr(f_ngram_word, "feature_class")                <- "lexicographic"
attr(f_ngram_word, "feature_sub_class")            <- "word"
attr(f_ngram_word, "datasource")                   <- "frog"
                                                   
attr(f_ngram_word, "dataset.token")                <- "word"
attr(f_ngram_word, "dataset.token.transformed")    <- "ngram"
                                                   
attr(f_ngram_word, "degree")                       <- 0        
attr(f_ngram_word, "proximity.criterium")          <- "sentence"       
                     

attr(f_ngram_word, "dictionary.token")             <- "ngram"                      
attr(f_ngram_word, "dictionary.override")          <- FALSE 
attr(f_ngram_word, "dictionary.prefix")            <- "wn_"                                                    
                                                   
attr(f_ngram_word, "dictionary.exclude")           <- list(docids    = NULL, 
                                                           majorpos  = c("LET","SPEC","TW"), 
                                                           words     = NULL,   
                                                           lemmas    = NULL)
                                                                             
attr(f_ngram_word, "dictionary.exclude.frequency") <- list(ll = 0,  ul = 1)                                                                                                                                                  
# ---------------------------------------------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (2) Lemma unigram counts, tf-idf, etc.                                                                                                       #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_ngram_lemma, "name")                         <- "Lemma unigram statistics"
attr(f_ngram_lemma, "description")                  <- "Lemma unigram" 
attr(f_ngram_lemma, "feature_class")                <- "lexicographic"
attr(f_ngram_lemma, "feature_sub_class")            <- "lemma"
    
attr(f_ngram_lemma, "datasource")                   <- "frog"
    
attr(f_ngram_lemma, "dataset.token")                <- "lemma"
attr(f_ngram_lemma, "dataset.token.transformed")    <- "ngram"

attr(f_ngram_lemma, "degree")                       <- 1        
attr(f_ngram_lemma, "proximity.criterium")          <- "sentence"  
    
attr(f_ngram_lemma, "dictionary.override")          <- FALSE 
attr(f_ngram_lemma, "dictionary.token")             <- "ngram" 
attr(f_ngram_lemma, "dictionary.prefix")            <- "ln_"    
attr(f_ngram_lemma, "dictionary.exclude")           <- list(docids    = NULL, 
                                                            majorpos  = c("LET","SPEC","TW"), 
                                                            words     = NULL,   
                                                            lemmas    = NULL)
                                                                            
attr(f_ngram_lemma, "dictionary.exclude.frequency")<- list(ll = 0,  ul = 1)                                                                                                                                                  

 
# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (3) Part-of-speech (fine-grained) counts, tf-idf, etc.                                                                                       #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_ngram_pos, "name")                           <- "Part-of-speech unigram statistics"
attr(f_ngram_pos, "description")                    <- "Part-of-speech unigram statistics" 
attr(f_ngram_pos, "feature_class")                  <- "syntactic"
attr(f_ngram_pos, "feature_sub_class")              <- "pos"
                                                    
attr(f_ngram_pos, "datasource")                     <- "frog"
                                                    
attr(f_ngram_pos, "dataset.token")                  <- "pos"
attr(f_ngram_pos, "dataset.token.transformed")      <- "ngram"
                                                    
                                                    
attr(f_ngram_pos, "degree")                         <- 1        
attr(f_ngram_pos, "proximity.criterium")            <- "sentence"  
                                                    
                                                    
attr(f_ngram_pos, "dictionary.override")            <- FALSE 
attr(f_ngram_pos, "dictionary.token")               <- "ngram"                                                        
attr(f_ngram_pos, "dictionary.prefix")              <- "posn_"      
attr(f_ngram_pos, "dictionary.exclude")             <- list(docids    = NULL, 
                                                            majorpos  = c("LET","SPEC","TW"), 
                                                            words     = NULL,   
                                                            lemmas    = NULL)
                                                                
attr(f_ngram_pos, "dictionary.exclude.frequency")    <- list(ll = 0,  ul = 1)
 
 
# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (4) Part-of-speech (major types; majorpos) counts, tf-idf, etc.                                                                              #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_ngram_majorpos, "name")                          <- "Part-of-speech unigram statistics"
attr(f_ngram_majorpos, "description")                   <- "Part-of-speech unigram statistics" 
attr(f_ngram_majorpos, "feature_class")                 <- "syntactic"
attr(f_ngram_majorpos, "feature_sub_class")             <- "pos"
   
attr(f_ngram_majorpos, "datasource")                    <- "frog"
   
attr(f_ngram_majorpos, "dataset.token")                 <- "majorpos"
attr(f_ngram_majorpos, "dataset.token.transformed")     <- "ngram"
   
attr(f_ngram_majorpos, "degree")                        <- 1        
   
attr(f_ngram_majorpos, "proximity.criterium")           <- "sentence"       
attr(f_ngram_majorpos, "dictionary.token")              <- FALSE 
attr(f_ngram_majorpos, "dictionary.override")           <- "ngram" 
attr(f_ngram_majorpos, "dictionary.prefix")             <- "majorposn_"        
attr(f_ngram_majorpos, "dictionary.exclude")            <- list(docids    = NULL, 
                                                               majorpos  = c("LET","SPEC","TW"), 
                                                               words     = NULL,   
                                                               lemmas    = NULL)
                                                               
attr(f_ngram_majorpos, "dictionary.exclude.frequency") <- list(ll = 0,  ul = 1)
  
 
# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (5) Synset (major types; majorpos) counts, tf-idf, etc.                                                                                      #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_ngram_synset, "name")                             <- "Synonym-set memberschip features"
attr(f_ngram_synset, "description")                      <- "Synonym-set memberschip features" 
attr(f_ngram_synset, "feature_class")                    <- "semantic"
attr(f_ngram_synset, "feature_sub_class")                <- "synset"
                                                         
attr(f_ngram_synset, "datasource")                       <- "frog"
                                                         
attr(f_ngram_synset, "dataset.token")                    <- "lemma"
attr(f_ngram_synset, "dataset.token.transformed")        <- "synset" 
                                                         
attr(f_ngram_synset, "degree")                           <- 1
                                                         
attr(f_ngram_synset, "proximity.criterium")              <- "sentence"       
attr(f_ngram_synset, "dictionary.token")                <- "ngram"                                                              
attr(f_ngram_synset, "dictionary.override")              <- FALSE 
attr(f_ngram_synset, "dictionary.prefix")                <- "synsetn_"                        
attr(f_ngram_synset, "dictionary.exclude")               <- list(docids    = NULL, 
                                                                 majorpos  = c("LET","SPEC","TW"), 
                                                                 words     = NULL,   
                                                                 lemmas    = NULL)
                                                                 
attr(f_ngram_synset, "dictionary.exclude.frequency")    <- list(ll = 0,  ul = 1)
                                                         
  
# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (6) Semantic hyperonym feature-construction                                                                                                  #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_ngram_hyperonym, "name")                          <- "Hyperonym-set memberschip features"
attr(f_ngram_hyperonym, "description")                   <- "Hyperonym-set memberschip features" 
attr(f_ngram_hyperonym, "feature_class")                 <- "semantic"
attr(f_ngram_hyperonym, "feature_sub_class")             <- "hyperonym"
                                                         
attr(f_ngram_hyperonym, "datasource")                    <- "frog"
                                                         
attr(f_ngram_hyperonym, "dataset.token")                 <- "lemma" 
attr(f_ngram_hyperonym, "dataset.token.transformed")     <- "hyperonym" 
                                                         
attr(f_ngram_hyperonym, "data.degree")                   <- 1        
attr(f_ngram_hyperonym, "data.proximity.criterium")      <- "sentence"       
                                                         
attr(f_ngram_hyperonym, "dictionary.override")           <- FALSE 
attr(f_ngram_hyperonym, "dictionary.token")              <- "ngram"                                                          
attr(f_ngram_hyperonym, "dictionary.prefix")             <- "hyperonymn_"                                                                                 
                                                         
attr(f_ngram_hyperonym, "dictionary.exclude")            <- list(docids    = NULL, 
                                                                 majorpos  = c("LET","SPEC","TW"), 
                                                                 words     = NULL,   
                                                                 lemmas    = NULL)
                                                                   
attr(f_ngram_hyperonym, "dictionary.exclude.frequency") <- list(ll = 0,  ul = 1)
  

# ---------------------------------------------------------------------------------------------------------------------------------------------#
# (7) Syntactic relation features (major types; majorpos) counts, tf-idf, etc.                                                                 #
# ---------------------------------------------------------------------------------------------------------------------------------------------#
attr(f_ngram_syntactic, "name")                          <- "Syntactic n-gram"
attr(f_ngram_syntactic, "description")                   <- "Syntactic n-gram" 
attr(f_ngram_syntactic, "feature_class")                 <- "syntactic"
attr(f_ngram_syntactic, "feature_sub_class")             <- "syntactic"
                                                        
attr(f_ngram_syntactic, "datasource")                    <- "frog"
                                                        
attr(f_ngram_syntactic, "dataset.token")                 <- "lemma"
attr(f_ngram_syntactic, "dataset.token.transformed")     <- "ngram"
 
attr(f_ngram_syntactic, "degree")                        <- 1        
attr(f_ngram_syntactic, "proximity.criterium")           <- "syntactic"       
                                                        
attr(f_ngram_syntactic, "dictionary.override")           <- FALSE 
attr(f_ngram_syntactic, "dictionary.token")              <- "ngram" 

attr(f_ngram_syntactic, "dictionary.prefix")             <- "syntn_" 
                                                        
attr(f_ngram_syntactic, "dictionary.exclude")            <- list(docids    = NULL, 
                                                                 majorpos  = c("LET","SPEC","TW"), 
                                                                 words     = NULL,   
                                                                 lemmas    = NULL)
                                                                             
attr(f_ngram_syntactic, "dictionary.exclude.frequency")  <- list(ll = 0,  ul = 1)
  
 