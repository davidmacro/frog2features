calculate_tf_idf <- function(dtfrog            = NULL,
							 dt.tf             = NULL,
                             dictionary        = NULL,
							 token             = NULL,
                             token.x           = NULL,
                             token.y		   = NULL,
                             return.type       = "wide",
                             return.measure    = "count",
							 prefix            = NULL, 
							 natural           = FALSE, 
							 margins           = NULL){
    
	stopifnot(is.data.table(tf))
  
	if (is.null(margins)){ 
		if (natural == FALSE){
			idf <- log10(nrow(tf) / colSums(tf > 0))
		} else {
			idf <- log(nrow(tf) / colSums(tf > 0))
		}
	} else {
        
	}
    
	tf_idf <- sweep(tf, 2L, idf, "*", check.margin = FALSE)
    
	return(tf_idf)
}
