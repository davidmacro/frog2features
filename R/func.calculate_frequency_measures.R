calculate_frequency_measures <- function(dtfrog, 
										 token       = "lemma", 
										 return.type = "all",
										 setkeys     = T){

	dtfrog <- dtfrog %>% data.table %>% FixStringsFactors
	
	stopifnot(is.frog(dtfrog))
 
    name_n       <- token %+% "_in_ndocs"
    name_p       <- token %+% "_in_pdocs"
	name_tfidd_1 <- token %+% "_tfidf1"
	
	name_per_doc <- token %+% "_name_per_doc"
	
    ndocs  <- uniqueN(dtfrog$docid)  
	
	if(!token %in% colnames(dtfrog)){ 
		cat("Error - Column name ", token, " does not exist in the given FROG object.") 
	    
	} else {  
	    setkeyv(dtfrog, token) %if% setkeys
	    
		dtfrog[, (name_n) := uniqueN(docid), by = c(token)]  	
		dtfrog[, (name_p) := get(name_n) / ndocs, ]   
	 
	    ids <- c("docid","sent","position")
	    
	    if(setkeys){ 
	      setkeyv(dtfrog, ids) 
	    }    
	    setcolorder(dtfrog,neworder = c(ids, colnames(dtfrog) %-% ids))
	}
	
	if(return.type == "all"){ 
		return(dtfrog)
	} else {
		if(return.type == "summary"){
			return(dtfrog[, c(token,name_n,name_p), by = c(token), with=F] %>% unique)
		} else {
			return(FALSE)
		}	
	}   
}