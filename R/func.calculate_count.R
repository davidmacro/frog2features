calculate_count <- function(dtfrog, 
    dictionary     = NULL,
    token          = NULL,
    token.x        = NULL,
    token.y		   = NULL,
    return.type    = "wide",
    return.measure = "count",
    prefix         = NULL){
 
    stopifnot(is.frog(dtfrog)) 
	  
	if(is.null(token.x) & is.null(token.y)){
		token.x <- token
		token.y <- token
	} 
    
	if(is.null(dictionary)){
		if(is.null(token.y)){
			token.y <- token.x
		}
		if(is.null(prefix)){
			dictionary <- create_dictionary(dtfrog, token = token.y, include.metrics = F) 
		} else {
			dictionary <- create_dictionary(dtfrog, token = token.y, prefix, include.metrics = F) 
		} 
	}  
	
    cat("# Create full factorial frame\r\n\r\n")
    dt.ff <- CJ(docid = dtfrog$docid %>% sort %>% unique, 
                token = dictionary[[token.y]] %>% sort %>% unique , unique = TRUE) 
    
	dt.ff[, c(token.x) := token]
	dt.ff[, token := NULL]
  
	
    cat("# Merge dictionary on token.x = " %+% token.x %+% 
        " and token.y = " %+% token.y %+% "\r\n\r\n")
    	
    dt.ff <- merge(
        x   = dt.ff, 
        y   = {dictionary %>% copy}[,count:=NULL], 
        by.x=c(token.x), 
        by.y=c(token.y)
    ) 
 
	cat("# Make aggregated dtfrog\r\n")
    dtfrog.aggregated <- dtfrog[, c("docid", token.x), with=F] %>% copy
     
    # (tf) calculate the term frequency by counting the number of times the term occurs in the document 
    dtfrog.aggregated <- dtfrog.aggregated[, token_in_doc_n:=.N, by = c("docid", token.x)] %>% unique
   
 
    cat("# Make merged dtfrog\r\n") 
    dt.merged <- merge(x = dt.ff,
                       y = dtfrog.aggregated,
                       by.x = c("docid",token.x), 
                       by.y = c("docid",token.y),
                       all.x = T,
                       all.y = F)[token_in_doc_n %>% is.na, token_in_doc_n := 0] 
      
    # (idf) calculate the number of documents in the corpus: 
    dt.merged[, ndocs := uniqueN(docid)] 
     
    # calculate the number of documents in which the term can be found 
    dt.merged[, token_in_n_doc := sum(token_in_doc_n > 0) + 1L , by = c(token.x)]
    
    # calculate the number of documents in which the term can be found 
    dt.merged[, tfidf := token_in_doc_n * log(ndocs/token_in_n_doc)]
       
    if(return.type == "wide"){
        
        return.formula <- as.formula("docid ~ name" )  
      	
        if(return.measure %in% c("count", "term.frequency")){
            value.var <- "token_in_doc_n"    
        }
        
        if(return.measure == "tfidf"){
            value.var <- "tfidf"    
        }
         	
        retval <- dcast(data        = dt.merged, 
                        formula		= return.formula, 
                         value.var 	= c(value.var))   
        
        return(retval)
    } 
    
    if(return.type == "long"){
        return(dt.merged)
    } 
} 