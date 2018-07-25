# --------------------------------------------------------------------------------------#
#                                                                                       #
#  frogdatahandling.R                                                                   #
#                                                                                       #
#  -------------------------------------------------------------------------------------#                                                                               
#                                                                                       #
#  Auteur: D.A. Macro                                                                   #
#           d.a.macro@minvenj.nl // davidmacro@gmail.com                                #
#                                                                                       #
#  -------------------------------------------------------------------------------------#
    


  
`%frapply%` <- function(dtfrog, dictionary){
	
	# Check if dtfrog is a frog object
	
	# Check if dictionary is a valid dictionary object
	
	# If (1) and (2), apply the dictionary to the frog data.table object and return the value. 


} 
 
 
save_dictionary = function(dtfrog, 
                           filename){
     saveRDS(dtfrog, file=filename)
     return(dtfrog)
}

load_dictionary = function(filename){
     return(readRDS(file=filename))
}
  
merge_synsets = function(dtfrog            = NULL, 
                         token.by.x        = c("lemma", "majorpos"),
                         token.by.y        = c("lemma", "majorpos"),
                         token.replacement = "lemma.replacement.2",
                         merge.database    = "lemma.synonyms.dutch"){
  
	# Assign synset database to local var  
	synsets <- get(merge.database) %>% copy
	
	stop("Invalid synset database") %if% (nrow(synsets) < 1)
	
	# Create merge with self (concatenate lemma to 'double' synsets)		    
	#synsets[, synset_combined := paste0(shQuote(sort(synset)), collapse=','), by = c(token.by.y)][, .(lemma, majorpos, synset_combined)] %>% unique
 
	# Merge frog
	dtfrog.merged <- merge(x    = dtfrog[, list(docid, sent, position, lemma, majorpos, parse1, word)] %>% unique, 
						   y    = synsets[, c(token.by.y, token.replacement), with=F], 
						   by.x = c(token.by.x),
	 					   by.y = c(token.by.y), all = F) 
 
		
	cols <- c("docid", "sent", "position")
				
	setcolorder(x = dtfrog.merged, neworder = c(c(cols), c(colnames(dtfrog.merged) %-% cols))) 
	
	setorderv(x = dtfrog.merged, cols) 
	
	return(dtfrog.merged %>% unique) 
}

merge_synset_properties = function(dtfrog = NULL, synset.properties = synset.properties.dutch){
	 
	 

} 
		
		
merge_hyperonymset  = function(dtfrog     = NULL, 
                               hyperonyms = synset.hypernyms.dutch){
	
	is.interactive <- TRUE	
						
	if(is.null(dtfrog)){ 
		is.interactive <- FALSE
		
		dtfrog <- self$dtfrog
		dtfrog <- dtfrog[, .(docid, word, lemma, sent, position, majorpos)] 
		
	}  else { 
		dtfrog <- dtfrog[, .(docid, word, lemma, sent, position, majorpos)] %>% self$fix_strings_as_factors  
	}

	# ------------------------------------------------------------------------------------#
	# Voorbewerken van hypernym data                                                      #
	#    -    Meerdere lemma kunnen naar hetzelfde hypernym verwijzen (herstructureren)   #
	#    -    Een lemma kunnen naar meerdere hypernymen  verwijzen (filteren)             #
                                                                                          #
	# ------------------------------------------------------------------------------------#
	 
	hypernyms           <- synset.hypernyms.dutch

    hypernyms.to.merge  <- tidyr:::separate_rows(hypernyms, lemmas_source, sep = ",")
	
	hypernyms.to.merge  <- synset.hypernyms[, lemma := lemmas_source][, lemmas_source := NULL][, relType := NULL][, relType := sourceid]
	 
	hypernyms.merged    <- merge(x  = dtfrog[, list(docid, sent, position, lemma)] %>% unique, 
                                 y  = hypernyms.to.merge ,  
                                 by = c("lemma"))
	 
	return(hypernyms.merged ) 
  
} 
 
 
#' Filter a frog object 
#'
#' @import data.table
#' @param dtfrog a frog data.table object
#' @param exclude a named list of vectors with elements to exclude; valid elements are 'docids', 'majorpos', 'words' and 'lemmas'. 
#' @return A filtered frog data.table object
#' @examples
#' \dontrun{
#' dtfrog.filtered <- filter_frog(dtfrog, exclude = list(majorpos=c("LET")))
#' }
filter_frog <- function(dtable, exclude) { 
   
	require(magrittr); require(data.table)
    
	dtable <- dtable %>% data.table %>% FixStringsFactors
	
	exclude_names <- names(exclude)
	
	for(i in exclude %>% seq_along){
	
		name <- exclude_names[[i]]
		
		if(name %in% colnames(dtable)){
			dtable <- dtable[!(get(exclude_names[[i]]) %in% exclude[[i]]),] 
		} else {
			cat("\r\nColumn name ", name , "does not exist. Filter not applied.\r\n")
		}
	} 
  
	return(dtable)	 
}			

#' Select items from a frog object 
#'
#' @import data.table
#' @param dtfrog a frog data.table object
#' @param select a named list of vectors with elements to exclude; valid elements are 'docids', 'majorpos', 'words' and 'lemmas'. 
#' @return A filtered frog data.table object
#' @examples
#' \dontrun{
#' dtfrog.filtered <- filter_frog(dtfrog, exclude = list(majorpos=c("LET")))
#' }
select_frog <- function(dtfrog, select=list(docids    = NULL, 
								            majorpos  = NULL, 
								            words     = NULL,  
								            lemmas    = NULL, logic = "OR")) { 
 
	
	dtfrog <- dtfrog %>% data.table %>% FixStringsFactors
  
 
	if(!is.null(exclude$docids)){
		if("docid" %in% colnames(dtfrog)){
			dtfrog <- dtfrog[(docid %in% as.numeric(exclude$docids)),]
		}
	}
		 
	if(!is.null(exclude$majorpos)){
		if("majorpos" %in% colnames(dtfrog)){
			dtfrog <- dtfrog[(majorpos %in% as.character(exclude$majorpos)),]
		}
	}	 
	
	if(!is.null(exclude$words)){
		if("word" %in% colnames(dtfrog)){
			dtfrog <- dtfrog[!(word %in% as.character(exclude$words)),] 	 
		} 
	}
	
	if(!is.null(exclude$lemmas)){
		if("lemma" %in% colnames(dtfrog)){
			dtfrog <- dtfrog[!(lemma %in% as.character(exclude$lemmas)),] 	 
		}
	}		
  
	return(dtfrog)	 
}			
 
  
