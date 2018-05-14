f2f_reliabilities_jaccard <- function(objf2f, 
        paths = "all", 
        type = "dictionary", 
        times = 1, 
        seed = NA, 
		return.dictionaries = F,
		return.list = T,
		verbose=T){

    cat("Calulating reliabilities: \r\n") %if% verbose
    cat("--------------------------------------------------------------------------\r\n") %if% verbose
    
    cat(" - Clone the original f2f object \r\n") %if% verbose
    tmp_objf2f <- objf2f$clone(deep = T)
  
    cat(" - Remove raw features and processed datasets from the cloned object") %if% verbose
    tmp_objf2f$Data$Raw.Features <- list()
    tmp_objf2f$Data$Raw.List <- list()
    
    cat(" - Garbace colletion \r\n") %if% verbose
    gc()
      
    cat(" - Define returnlist for ", times, " iterations \r\n") %if% verbose
    returnlist <- list()
    
    cat(" - Define seed \r\n") %if% verbose
    if(seed %>% is.na %>% not){ set.seed(seed) }
    
    # Retrieve docids from the original object:
    ids   <- tmp_objf2f$Data$Raw$docid %>% unique 
    
    # Copy the raw data of the original object to a separate table
    
    f2f_cols <-   c("docid", "sent", "position", "word", "lemma", "pos", "ner",  "parse1", "parse2", "majorpos")
    
	rawdata <- {tmp_objf2f$Data$Raw %>% copy}[, ..f2f_cols]
 
    # Remove the raw data from the original object:
    tmp_objf2f$Data$Raw <- NULL
    
    gc()
     
    for(i in 1:times){
      
        ids_a <- ids %>% sample(size = ((ids %>% length) / 2) %>% ceiling) %>% sort
        ids_b <- setdiff(ids, ids_a)

         # Make two new f2f objects, each containing a (disjoint) split of the original data: 
        splitA <- tmp_objf2f$clone(deep = T)
        splitA$Data$Raw.List    <- list()
        splitA$Data$Replay.List <- list()
        splitA$setRawData(rawdata[docid %in% ids_a,] %>% copy)
        
        splitA$Make()
        splitA$Data$Raw <- NULL 
         
        splitA$Data
        
        # Make two new f2f objects, each containing a (disjoint) split of the original data: 
        splitB <- tmp_objf2f$clone(deep = T)
        splitB$Data$Raw.List    <- list()
        splitB$Data$Replay.List <- list()
        splitB$setRawData(rawdata[docid %in% ids_b,] %>% copy)
  
        splitB$Make()
        splitB$Data$Raw <- NULL 
       
        gc()
        
		if(paths == "all"){
			features <- splitA$Data$Raw.Features %>% names
		} else {
			features <- paths
		}
		 
        returnlist[[i]] <-list()
        
        for(feature in features){
            
            # Determine if the feature has a dictionary; if so, calculate dictionary similarity: 
            dict.a   <- splitA$getDictionary(feature)
            dict.b   <- splitB$getDictionary(feature)
            
            # Determine the name of the dictionary column for which to calculate the similarities
            token.A <- splitB$
                            FeatureDefinitions[[feature]]$
                            DataTransformators$
                            DataTransformators[token %>% is.na %>% not, token] %>% tail(1)
            
            token.B <- splitB$
                            FeatureDefinitions[[feature]]$
                            DataTransformators$
                            DataTransformators[token %>% is.na %>% not, token] %>% tail(1)
            
            length_a <- dict.a %>% nrow
            length_b <- dict.b %>% nrow
         
            jaccards <- rep(NA,10)
            
            counter <- 1
            
            for(perc in c(10,20,30,40,50,60,70,80,90,100)){
              
                if(perc < 100){
                    nmax.a <- dict.a %>% nrow %>% multiply_by(perc / 100) %>% floor     
                    nmax.b <- dict.b %>% nrow %>% multiply_by(perc / 100) %>% floor     
                } else {
                    nmax.a <- dict.a %>% nrow   
                    nmax.b <- dict.b %>% nrow  
                }
                 
                lenght_union_ab     <- (dict.a[[token.A]][1:nmax.a] %union%      dict.b[[token.B]][1:nmax.b]) %>% length
                lenght_intersect_ab <- (dict.a[[token.A]][1:nmax.a] %intersect%  dict.b[[token.B]][1:nmax.b]) %>% length

                jaccards[[counter]] <- lenght_intersect_ab / lenght_union_ab
                  
                counter <- counter + 1                  
            }
            
            lenght_union_ab     <- (dict.a[[token.A]] %union%      dict.b[[token.B]]) %>% length
            lenght_intersect_ab <- (dict.a[[token.A]] %intersect%  dict.b[[token.B]]) %>% length
         
            retval <- list(
                length_a            = length_a,
                length_b            = length_b,
                lenght_union_ab     = lenght_union_ab,
                lenght_intersect_ab = lenght_intersect_ab, 
                jaccards            = jaccards
            ) 
			
			if(return.dictionaries){
				retval$dictionary_a <- dict.a %>% copy
				retval$dictionary_b <- dict.b %>% copy
			}
			
            returnlist[[i]][[feature]] <- retval
        } 
        
        splitA <- NULL
        splitB <- NULL
        
        gc()
         
    } 
	
	if(return.list){
	
		returnlist %>% return 	
	
	} else {  
		returnlist %>% lapply(function(x.atomic){ 
			x.atomic %>% lapply(function(x.feature){  
				dt <- x.feature$jaccards %>% data.table(value = .) 
				dt[, jaclevel :=  1:10 * 10] 
				dt  
				}) %>% rbindlist(idcol = "feature") 
		}) %>% rbindlist %>% return
		 
	}  
}

 


















