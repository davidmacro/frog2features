cLogBook <- R6Class("logbook", 

	public = list(
		
		logbook = NULL,
		name = NULL,
		description = NULL,
		author = NULL,
		
		initialize = function(name = NULL, description = NULL, author = NULL){
		
			self$logbook <- data.table(
					cbind(id      = c(0.0),
						  action  = c("Initialize log"),
						  description = c(" "),
						  functioncall = c("cLogBook$initialize"),
					      checksum = c(NA),
					      recoverable = c(FALSE))
			) 
			
			(self$name        <- name        ) %ifnotnull% name
			(self$description <- description ) %ifnotnull% description
			(self$author      <- author      ) %ifnotnull% author
		    
		},
		
		append = function(action = NA, description = NA, functioncall = NA, checksum = NA, recoverable = NA){
   
			 dt.row <- data.table(
			 	id           = self$logbook[, max(as.numeric(id))+1],
			 	action       = action,
			 	description  = description,
			 	functioncall = functioncall %>% as.call %>% deparse,
			 	checksum     = checksum,
			 	recoverable  = recoverable
			 )  
			 self$logbook <- rbindlist(list(self$logbook, dt.row), use.names=T,fill=T) 
		},
		
		save = function(folder){
					
		},
		
		print = function(){
			self$logbook		
		}
	
	)
	
)