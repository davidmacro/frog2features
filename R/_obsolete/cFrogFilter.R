cFrogFilter <- R6Class("frogfilter", 

	public = list(
	
		Exclude       	  	 = NULL,      
		ExcludeDocFrequency  = NULL,    
		
		Include              = NULL,    
		IncludeDocFrequency	 = NULL,    
		
		Select            	 = NULL,    	
	 
		Strict 			  	 = NULL,
			 
		AppliedTo         	 = NULL,  
		AppliedToHash     	 = NULL,
	 
		Order 	          	 = NULL,
 
		initialize = function(){

			self$Order <- private$default_order	
		
			invisible(self)
		},
		
		add = function(type, token, values){
		
			if(!(token %in% private$valid_tokens)){
				
				
			} else {
			 
				if(type %in% private$types_drop){
					 
				}
				
				if(type %in% private$types_keep){
				
				
				}
			
			}
		},
		
		setOrder = function(order){
					
		},
		
		getOrder = function(){
			print(self$Order)
		},
			
		addExclude = function(value){
		
		
		},
		
		getValidTokens = function(){
			return(private$valid_tokens)
		},
		
		setValidTokens = function(tokens){
			private$valid_tokens <- tokens
		},
		
		addExcludeDocFrequency = function(value){
		
		
		}, 
		
		addExcludeDocRatio = function(value){
		
		
		}, 
		

		show = function(){


		},		
	
		apply = function(){
			
					
		
		}
	
	
	), 
	
	private = list(
	
		types_drop = c("exclude","delete","remove","Exclude","Delete","Remove", "keep"),
		
		types_keep = c("include","keep","Include","Require" ),
		
		valid_tokens = c("docid","lemma","word","majorpos","pos", "synset","hyperonymset")
		
	)
  
)