create_dictionary <- function(dtfrog, 
							  token, 
							  prefix  = "f_",
							  include.metrics = T){
     
    stop("The data-object does not contain a column named " %+% token) %if% (token %notin% (dtfrog %>% colnames))
 
    setkeyv(dtfrog, token)
    token <<- token
    retval <- dtfrog[, .(count = .N), by=c(token)] 
     
	if(include.metrics){
	
		dt.metrics <- calculate_frequency_measures(
			dtfrog      = dtfrog, 
			token       = token, 
			return.type = "summary"
		) 
		
		retval <- merge(
			x     = retval,
			y     = dt.metrics, 
			by.x  = token,
			by.y  = token,  
			all.x = T
		)
	}
	
    setorder(retval, -count)
   
    retval[, name := prefix %+% .I]
	
	if(include.metrics){
		setcolorder(retval, c("name", "count", token, dt.metrics %>% colnames %-% token))
	} else {
		setcolorder(retval, c("name", "count", token))
	}
	
    return(retval)
}   