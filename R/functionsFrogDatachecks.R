# --------------------------------------------------------------------------------------#
#                                                                                       #
#  frogdatachecks.R                                                                     #
#                                                                                       #
#  -------------------------------------------------------------------------------------#                                                                               
#                                                                                       #
#  Auteur: D.A. Macro                                                                   #
#           d.a.macro@minvenj.nl // davidmacro@gmail.com                                #
#                                  
#  -------------------------------------------------------------------------------------#                                                                               
# 
#  Functions to perform checks on frog objects.  
# 
# 
#  -------------------------------------------------------------------------------------#

 
#' Check whether the object is valid frog data.table. 
#' @import data.table
#' @param dtfrog a suspected frog data.table object
#' @param exclude a named list of vectors with elements to exclude; valid elements are 'docids', 'majorpos', 'words' and 'lemmas'. 
#' @return A filtered frog data.table object
#' @examples
#' \dontrun{
#' dtfrog.filtered <- filter_frog(dtfrog, exclude = list(majorpos=c(\"LET\")))
#' }
is.frog <- function(dtfrog, require_all_original_columns = TRUE,
						    require_docid = TRUE,
						    require_sent = TRUE,
						    require_position = TRUE,
						    require_word = FALSE,
						    require_lemma = FALSE,
						    require_morph = FALSE, 
						    require_pos = FALSE,
						    require_prob = FALSE,
						    require_ner = FALSE,
						    require_chunk = FALSE,
						    require_parse1 = FALSE,
						    require_parse2 = FALSE,
						    require_majorpos = TRUE, 
						    require_unique_keys = TRUE,
						    allow_extra_columns = FALSE){

	
	require_docid 		<- ifelse(require_all_original_columns, TRUE, require_docid) 
	require_sent 		<- ifelse(require_all_original_columns, TRUE, require_sent) 
	require_position	<- ifelse(require_all_original_columns, TRUE, require_position) 
	require_word 		<- ifelse(require_all_original_columns, TRUE, require_word) 
	require_lemma 		<- ifelse(require_all_original_columns, TRUE, require_lemma) 
	require_morph 		<- ifelse(require_all_original_columns, TRUE, require_morph) 
	require_pos 		<- ifelse(require_all_original_columns, TRUE, require_pos) 
	require_prob 		<- ifelse(require_all_original_columns, TRUE, require_prob) 
	require_ner 		<- ifelse(require_all_original_columns, TRUE, require_ner) 
	require_chunk 		<- ifelse(require_all_original_columns, TRUE, require_chunk) 
	require_parse1 		<- ifelse(require_all_original_columns, TRUE, require_parse1) 
	require_parse2 		<- ifelse(require_all_original_columns, TRUE, require_parse2) 
	require_majorpos 	<- ifelse(require_all_original_columns, TRUE, require_majorpos)  
		
	# Check 1: is data.table object?
	ch1 <- ifelse(require_all_original_columns,(is.data.table(dtfrog) == TRUE), TRUE)
	
	(cat("Error. Input object is not a data.table object")) %if% (!ch1)
	
	# Check 2: are all required columns present? 	
	cols.to.check <-  c(ifelse(require_docid , "docid", NA),
						ifelse(require_sent , "sent", NA),
						ifelse(require_position, "position", NA),
                      	ifelse(require_word , "word", NA),
						ifelse(require_lemma , "lemma", NA),
						ifelse(require_morph , "morph", NA),
						ifelse(require_pos , "pos", NA),
						ifelse(require_prob , "prob", NA),
						ifelse(require_ner , "ner", NA),
						ifelse(require_chunk , "chunk", NA),
						ifelse(require_parse1 , "parse1", NA),
						ifelse(require_parse2 , "parse2", NA),
						ifelse(require_majorpos, "majorpos" ,NA)) %>% na.omit %>% as.vector
	 
	ch2 <- cols.to.check %in% (dtfrog %>% colnames) %>% all
	
	(cat("Error. Not all required columns are present. Please make sure that the data.table object contains at least:", cols.to.check  )) %if% (!ch2)
	
	# Check 3:  no duplicates?
	ch3 <-  ifelse(require_unique_keys,
				   dtfrog[, CHECK := length(.N) == 1, by=c("docid","sent","position")][, all(CHECK)],
				   TRUE)
		
	(cat("Error. Frob object contains duplicates.")) %if% (!ch3)
	
	
	# Check 4:  allow_extra_columns
	ch4 <-  ifelse(require_unique_keys,
				   dtfrog[, CHECK := length(.N) == 1, by=c("docid","sent","position")][, all(CHECK)],
				   TRUE)
		
	(cat("Error. Frob object contains extra columns, but this was not allowed. Consider setting the argument 'allow_extra_columns' to TRUE. ")) %if% (!ch4)
	
	return(ch1 & ch2 & ch3 & ch4)
}
 
#' @describeIn isfrog Check whether the provided data.table object is an original frog object. 
is.frog.original <- function(dtfrog){
	return(is.frog, require_all_original_columns = TRUE,
	                require_unique_keys = TRUE, 
				    allow_extra_columns = FALSE)
}                   

#' @describeIn isfrog Check whether the provided data.table object is a full frog object. 
is.frog.full <- function(dtfrog){
	return(is.frog, require_all_original_columns = TRUE,
	                require_unique_keys = TRUE, 
				    allow_extra_columns = TRUE)
}

#' @describeIn isfrog Check whether the provided data.table object is a minimal frog object. 
is.frog.minimal <- function(dtfrog){
	return(is.frog, require_all_original_columns = FALSE,
				    require_docid = TRUE,
				    require_sent = TRUE,
				    require_position = TRUE,	
	                require_unique_keys = TRUE, 
				    allow_extra_columns = TRUE)
}

