# Count (raw, standardized
# Merge properties 

cDictionaryAction <- R6Class("feature_selection",
	public = list(
	
		dictionary      = NULL,
		action.type     = NULL,
		action.settings = NULL,
	 
		initialize = function(dictionary = NULL, 
							  action.type = "count"){ 
					
			(self$dictionary <- dictionary) %ifnotnull% dictionary
					
			self$do_action  <- switch(action.type, 
				exists = dictionary_action_exists,
				count  = dictionary_action_count
			)
							  
		} ,
		
		preprocessing = function(dtfrog,exclude 
								 ){
		
			dtfrog.tmp <- dtfrog
		
			dtfrog.tmp %<>% FixStringsFactors  
			dtfrog.tmp %<>% filter_frog(exclude)
		 
		}
	
		 
	), lock_object = FALSE 
)


dictionary_action_exists <- function(dtfrog = NULL, 
								     dictionary = NULL,
									 exclude = NULL){
 
}


dictionary_action_count <- function(dtfrog, 
									dictionary){
cat("count")


}

dictionary_action_merge_properties <- function(dtfrog, 
										       dictionary){



}