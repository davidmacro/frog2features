# --------------------------------------------------------------------------------------# 
#                                                                                       # 
#  Features :: dictionary_construction.R                                                # 
#                                                                                       # 
#  -------------------------------------------------------------------------------------#                                                                            
#                                                                                       # 
#  Auteur: D.A. Macro                                                                   # 
#           d.a.macro@minvenj.nl // davidmacro@gmail.com                                # 

dictionary.lemma.test <- data.table(
    lemma_id = c(1,2,3),
    lemma    = c("lopen", "computer", "fiets"), 
    p1       = c(10,20,33),
    stringsAsFactors = FALSE
) 

# ---------------------------------------------------------------------------------------------------------------- #
# A dictionary object has the following structure:                                                                 #
# ---------------------------------------------------------------------------------------------------------------- #
#                                                                                                                  #
#  Classes: 	‘data.table’, 'data.frame', 'dictionary'                                                           #
# ---------------------------------------------------------------------------------------------------------------- #			
#                                                                                                                  #
#  Content:      data.table containing the dictionary's information.                                               #
# ---------------------------------------------------------------------------------------------------------------- # 
#                                                                                                                  #
#    Mandatory columns:                                                                                            #
#                                                                                                                  #
#	  			  [entry]_id 			        # Numeric identifier (unique within the dictionary)                #
#                 [entry]_name        	        # Alphanumeric identifier (may be provided externally) 	           #
#                                                                                                                  #
#    Optional columns:                                                                                             #
#                                                                                                                  #
#                  - description*		        # Description per entry                                            #
#                  - properties{...}*           # Additional columns of properties                                 #
# ---------------------------------------------------------------------------------------------------------------- #
#                                                                                                                  #
#   Attributes                                                                                                     #
# ---------------------------------------------------------------------------------------------------------------- #
#     (2) Custom attributes                                                                                        #
#      - attr(*, "description")                 # Description of the dictionary									   #
#      - attr(*, "type")                        # Dictionary type ("word","lemma","majorpos")		               #
#      - attr(*, "join_column")                 # Dictionary type ("word","lemma","majorpos")                      # 
# ---------------------------------------------------------------------------------------------------------------- #
#	
# ---------------------------------------------------------------------------------------------------------------- #
	 
 

# create_dictionary <- function(x,  
# 							  exclude                    = NULL, 
# 							  prefix                     = NULL, 
# 							  feature.selection.methods	 = NULL, 
# 							  selection.methods.options  = "default",
# 							  dictionary.type            = "lemma",
# 							  dictionary.properties      = NULL)  {
#  	   					  
# 	(prefix <- "") %ifnull% prefix
# 	(prefix <- "") %ifnull% prefix						  
# 							  
#  	if(selection.methods.options == "default"){
# 	  	names(selection.methods.options) <- "selection_options_" %+% selection.methods   
# 	} else {
# 		# todo: check if for every element in selection method a (valid) options element in selection.methods.options exists
# 	}
#   
#     # Preprocesses frog data.table   
#  	dtfrog <- dtfrog %>%  FixStringsFactors 
# 
# 	# Exclude items from the data.table
# 	if(dictionary.type == "majorpos"){
# 		filter_frog(exclude=list(majorpos  = c("LET","SPEC","TW"))) 
# 	}
# 	 
# 	# (1) Filter method == incidence; filter featuers based on occurence 
# 	if("incidence" %chin% selection.methods){
# 	
# 		# Create feature selector\
# 		
# 		# Apply filter
# 	
# 		# Count the number of occurrences of a lemma in a document.          
# 		dtfrog[, tmp_entry_in_n_docs    := length(unique(docid)), by = c(dictionary.type) ]
# 		dtfrog[, tmp_entry_in_prop_docs := tmp_entry_in_n_docs/ length(unique(docid)) ]
# 	  
# 		# Selecteer een entry alleen als deze in meer dan 1% van alle docs voorkomt: 
# 		dtfrog.selected <- dtfrog[tmp_entry_in_prop_docs > ll & 
# 								  tmp_entry_in_prop_docs < ul,
# 								  c(dictionary.type,"tmp_entry_in_prop_docs"),with=F] %>% unique
# 
# 		# Selecteer een lemma vervolgens alleen als deze niet in de lijst met stopwoorden voorkomt: 
# 		if(!is.null(exclude)){
# 			dtfrog.selected <- dtfrog.selected[! (lemma %in% exclude), .(lemma,lemmainpdocs)]
# 		} 
# 		 
# 		# Sorteer op proportie
# 		dtfrog.selected <- dtfrog.selected[order(lemmainpdocs, decreasing=T)]
# 		 
# 	}
# 	 
# 	# (2) Filter method == filter; filter featuers based on association with outcome (univariate or multivariate) 
# 	if("filter" %chin% selection.methods){
# 	
# 	
# 	}	  
# 	
# 	if("wrapper" %chin% selection.methods){
# 	
# 	
# 	}
# 	
# 	if("embedded" %chin% selection.methods){
# 	
# 	
# 	} 
# 	
# 	if(do_debug) cat("\t# Maak een full factorial dt (documents x synsets)", "\r\n")
# 	dtfrog.fullfactorial <- expand.grid(docid  = sort(dtfrog$docid  %>% unique),
# 	                                    synset = sort(dtfrog.lemmaselected$lemma %>% unique),
# 	                                    stringsAsFactors = F) %>% data.table
#     
# 
#   	if(do_debug) cat("\t# Merge alle geobserveerde synset terug met de full factorial dt", "\r\n")
# 	dtfrog.synset.merged <- merge(x    = dtfrog.fullfactorial, 
# 	                              y    = dtfrog.synsetsselected,
# 	                             by    = c("docid","synset"),
# 	                             all.x = TRUE,
# 	                             all.y = TRUE )
# 	 
# 	if(do_debug) cat("\t# Stel nu de synsetfrequentie op 0 voor alle na's", "\r\n")
# 	dtfrog.synset.merged <- dtfrog.synset.merged[is.na(nsynset), nsynset:=0 ]
# 
# 	if(do_debug) cat("\t# Merge de uitkomstvariabelen terug", "\r\n")
# 	dtfrog.synset.merged <- merge(x = dtfrog.synset.merged, 
# 	                              y = dtfrog[, c("docid",screening_options$dependent,screening_options$controls),with=F] %>% unique,
# 	                             by = c("docid"),
# 	                             all.x = T,
# 	                             all.y = T)											
# 	 								  			
# 	if(do_debug) cat("\t# Selecteer een synset vervolgens alleen als deze niet in de lijst met stopwoorden voorkomt: ", "\r\n")
# 	if(!is.null(exclude)){
# 		dtfrog.synsetsselected <- dtfrog.synsetsselected[! (synset %in% exclude), .(lemma,synset,lemmainpdocs)]
# 	} 
# 	 
# 	if(do_debug) cat("\t# Maak een vector met kolomnamen waarop gefilterd moet worden.", "\r\n") 
#     t.col.names <- screening_options$dependents %+% screening_options$suffix 
# 	 
# 	
# 	if(do_debug) cat("\t# Maak een vector met kolomnamen waarop gefilterd moet worden.", "\r\n") 
#     t.col.names <- screening_options$dependents %+% screening_options$suffix 
# 	 
# 	# 
#     dtfrog.lemma.merged[ , (t.col.names) := feature_to_t_values(dtable     = .SD, 
# 														         feature    = screening_options$feature,
# 														         dependents = screening_options$dependents,
# 														         controls   = screening_options$controls,
# 														         family     = screening_options$family),  by=synset]
# 	# Genereer een uniek lemma id 
# 	dtfrog.lemmaselected[, lemmacode := prefix %+% .I] 
# 	  
# 	if(!is.null(limit)){
# 		if(limit < nrow(dtfrog.lemmaselected)){
# 			dtfrog.lemmaselected <- dtfrog.lemmaselected[1:limit,]
# 		}
# 	}
# 
# 
# 
# 
# 	attr(dictionary.majorpos, "description")           <- "Dictionary of the major part-of-speech categories returned by the FROG parser"
# 	attr(dictionary.majorpos, "type")                  <- "majorpos"
# 	attr(dictionary.majorpos, "join_column")           <- "majorpos"
# 	attr(dictionary.majorpos, "action")                <- "count" 
#     attr(dictionary.majorpos, "return.data.type")      <- "wide"
#     attr(dictionary.majorpos, "return.na.value")       <- 0
#     return(dictionary.majorpos)
# 
#  											
# }

 
create_dictionary_lemmas <- function(dtfrog, 
									ll		          =  0.02, 
									ul 	              =  0.90, 
									exclude           =  NULL, 
									prefix            =  "l_", 
									limit             =  NULL,
									screening_method  = "incremental",
									screening_options = list(dependents    = c("O1","O2","O3","O4", "O5","O6","O7","O8"),
																feature    = "nsynset",
																suffix     = "t",
																family     = binomial(link="logit"),
																controls   = NULL,
																nmax       = NULL)){

																
	do_debug <- TRUE																
																
    (cat("# ----------------------------------------------------------------------- ", "\r\n")) %if% (do_debug)   
    (cat("# Genereer lemma dictionary                                               ", "\r\n")) %if% (do_debug) 
    (cat("# ----------------------------------------------------------------------- ", "\r\n")) %if% (do_debug) 	 													
 	(cat("#     Opties:                                                             ", "\r\n")) %if% (do_debug)   			
	(cat("#     Opties:                                                             ", "\r\n")) %if% (do_debug)  				
    (cat("# ----------------------------------------------------------------------- ", "\r\n")) %if% (do_debug) 
	
	if(do_debug) cat("\t# Fix strings as factors", "\r\n")
	dtfrog <- dtfrog[, .(docid, lemma)] %>% FixStringsFactors 
	 
	# Verwijder leestekens
	dtfrog    <- dtfrog[! (lemma %in% special.lemmata.punctuation()), ] 
 		
	# Bereken in hoeveel documenten (n) een lemma voorkomt, door per lemma het aantal unieke docids te tellen. 
	dtfrog     <- dtfrog[, lemmainndocs := length(unique(docid)), by = c("lemma")] 

	# Bereken in welke proportie van alle documenten een lemma voorkomt.  
	dtfrog     <- dtfrog[, lemmainpdocs := lemmainndocs /length(unique(docid))] 
  
	# Selecteer een lemma alleen als deze in meer dan 1% van alle docs voorkomt: 
	dtfrog.lemmaselected <- unique(dtfrog[lemmainpdocs > ll & lemmainpdocs < ul,.(lemma,lemmainpdocs)])

	# Selecteer een lemma vervolgens alleen als deze niet in de lijst met stopwoorden voorkomt: 
	if(!is.null(exclude)){
		dtfrog.lemmaselected <- dtfrog.lemmaselected[! (lemma %in% exclude), .(lemma,lemmainpdocs)]
	} 
	 
	# Sorteer op proportie
	dtfrog.lemmaselected <- dtfrog.lemmaselected[order(lemmainpdocs, decreasing=T)]
	
	
	if(do_debug) cat("\t# Maak een full factorial dt (documents x synsets)", "\r\n")
	dtfrog.fullfactorial <- expand.grid(docid  = sort(dtfrog$docid  %>% unique),
	                                    synset = sort(dtfrog.lemmaselected$lemma %>% unique),
	                                    stringsAsFactors = F) %>% data.table
    

  	if(do_debug) cat("\t# Merge alle geobserveerde synset terug met de full factorial dt", "\r\n")
	dtfrog.synset.merged <- merge(x    = dtfrog.fullfactorial, 
	                              y    = dtfrog.synsetsselected,
	                             by    = c("docid","synset"),
	                             all.x = TRUE,
	                             all.y = TRUE )
	 
	if(do_debug) cat("\t# Stel nu de synsetfrequentie op 0 voor alle na's", "\r\n")
	dtfrog.synset.merged <- dtfrog.synset.merged[is.na(nsynset), nsynset:=0 ]

	if(do_debug) cat("\t# Merge de uitkomstvariabelen terug", "\r\n")
	dtfrog.synset.merged <- merge(x = dtfrog.synset.merged, 
	                              y = dtfrog[, c("docid",screening_options$dependent,screening_options$controls),with=F] %>% unique,
	                             by = c("docid"),
	                             all.x = T,
	                             all.y = T)											
	 								  			
	if(do_debug) cat("\t# Selecteer een synset vervolgens alleen als deze niet in de lijst met stopwoorden voorkomt: ", "\r\n")
	if(!is.null(exclude)){
		dtfrog.synsetsselected <- dtfrog.synsetsselected[! (synset %in% exclude), .(lemma,synset,lemmainpdocs)]
	} 
	 
	if(do_debug) cat("\t# Maak een vector met kolomnamen waarop gefilterd moet worden.", "\r\n") 
    t.col.names <- screening_options$dependents %+% screening_options$suffix 
	 
	
	if(do_debug) cat("\t# Maak een vector met kolomnamen waarop gefilterd moet worden.", "\r\n") 
    t.col.names <- screening_options$dependents %+% screening_options$suffix 
	 
	# 
    dtfrog.lemma.merged[ , (t.col.names) := feature_to_t_values(dtable     = .SD, 
														         feature    = screening_options$feature,
														         dependents = screening_options$dependents,
														         controls   = screening_options$controls,
														         family     = screening_options$family),  by=synset]
	# Genereer een uniek lemma id 
	dtfrog.lemmaselected[, lemmacode := prefix %+% .I] 
	  
	if(!is.null(limit)){
		if(limit < nrow(dtfrog.lemmaselected)){
			dtfrog.lemmaselected <- dtfrog.lemmaselected[1:limit,]
		}
	}
 
}
 				   
create_dictionary_ngrams <- function(dtfrog, 
										ll = 0.02, 
										ul = 0.90, 
										exclude = NULL, 
										ndegree=1, 
										limit = 500,
										prefix="ngram_"){

						    
	dtfrog <- dtfrog[, .(docid, lemma, sent, position, majorpos)] %>% FixStringsFactors 

	#------------------------------------------------------------------------------------------------------------------------------------------#
	# Databewerking voor het construeren van de ngrams:                                                                                        #
	#------------------------------------------------------------------------------------------------------------------------------------------#
	#	docid 	sent 	pos 	lemma	...   			  lemma0      lemma1	 lemma2              ngram                                     #
	#     1       1      1        ik              			ik		   lopen	  naar               "ik lopen naar"                           #
	#     1       1      2        lopen			==>         lopen       naar      ...       ==>      NA                                        #
	#     1       1      3        naar                      naar 	     ...      ...				 NA                                        #
	#------------------------------------------------------------------------------------------------------------------------------------------#
	#                                                                                                                                          #
	#	Kanttekeningen:                                                                                                                        #
	#                                                                                                                                          #
	#		-	Groepeer alleen woorden die direct na elkaar voorkomen; leestekens zijn hierbij natuurlijke afbakeningen van groepen woorden.  #
	#		-	Construeer geen n-grams wanneer deze leestekens, speciale aanduidingen (namen etc.) en telwoorden bevatten.                    #
	#		-	Construeer geen gedeeltelijke n-grams. 				                                                                           #
	#                                                                                                                                          #
	#------------------------------------------------------------------------------------------------------------------------------------------#
		
	
	# Verwijder leestekens, twelwoorden en aanduidingen (spec)
	dtfrog <- dtfrog[!(majorpos %in% c("LET","SPEC","TW")),] 

	# Stel een key in
	setkeyv(dtfrog, c("docid", "sent", "position"))
 
	# Verwijder stopwoorden uit de elementen van de ngrams
	if(!is.null(exclude)){
		dtfrog <- dtfrog[! (lemma %in% exclude), ]
	}  
	
	for(i in 0:ndegree){  
		dtfrog[position == shift(position, 1, type="lead") - 1 , 
			   eval("lemma" %+% i) := shift(lemma, n = i, type="lead"), by = c("docid", "sent") ] 
	} 
	
	lcols <- c("lemma" %+% c(0:ndegree))
	 
 
    dtfrog <- na.omit(dtfrog)
	dtfrog[, ngram:=do.call(paste, c(.SD, sep=" ")), by = c("docid", "sent"), .SDcols = lcols]
	
	dtfrog <- dtfrog[, .(docid = docid, lemma = ngram)]
 
	dictionary <- generatedictionary(dtfrog,ll = ll, ul = ul, exclude = exclude, prefix=prefix, limit = limit)
	
	return(dictionary[1:limit,])
 
}
 
generate_dictionary_synsets <- function(dtfrog, 
									   ll 				 = 0.02, 
									   ul 				 = 0.90, 
									   synset.properties = synset.properties.dutch,
									   exclude 			 = NULL, 
									   prefix            = "synset_", 
									   screening_method  = "incremental",
									   screening_options = list(dependents = c("O1","O2","O3","O4", "O5","O6","O7","O8"),
																feature    = "nsynset",
																suffix     = "t",
																family     = binomial(link="logit"),
																controls   = NULL,
																nmax       = NULL)){
	do_debug <- do_debug

    if(do_debug) cat("# ----------------------------------------------------------------------- ", "\r\n")
    if(do_debug) cat("# function: generate_synset_dictionary                                    ", "\r\n")
    if(do_debug) cat("# ----------------------------------------------------------------------- ", "\r\n\r\n")
	

	
    if(do_debug) cat("\t# Maak een dt geaggregeerd op lemma-niveau", "\r\n")
	lemma.summaries  <- synset.properties.dutch[, .(lemma,majorpos, synset )] %>% unique
    
	if(do_debug) cat("\t# Maak een dt geaggregeerd op synset-niveau", "\r\n")
	synset.summaries <- synset.properties.dutch[, .( lemmas = paste0(lemma, collapse=",")), by = synset]
 
	if(do_debug) cat("\t# Merge de lemma-summaries aan het frog-object", "\r\n")
	dtfrog.merged <- merge(x               = dtfrog, 
					       y               = lemma.summaries, 
					       by              = c("lemma","majorpos"), 
					       all.x           = T,
					       all.y           = F,
					       allow.cartesian = T)   						   
								
							   
	if(do_debug) cat("\t# Verwijder leestekens", "\r\n")
	dtfrog.merged    <- dtfrog.merged[! (lemma %in% special.lemmata.punctuation()), ] 
 		
	if(do_debug) cat("\t# Bereken het aantal documenten (n) waar de synset in voorkomt", "\r\n")
	dtfrog.merged     <- dtfrog.merged[, synsetinndocs := length(unique(docid)), by = c("synset")] 

	if(do_debug) cat("\t# Bereken in welke proportie van alle documenten een synset voorkomt.  ", "\r\n")
	dtfrog.merged     <- dtfrog.merged[, synsetinpdocs := synsetinndocs /length(unique(docid))] 
  
	if(do_debug) cat("\t# Filter 1: ", "\r\n")
	if(do_debug) cat("\t# Selecteer een synset alleen als deze in meer dan \"ll\" van alle docs voorkomt en in niet meer dan \"ul\":", "\r\n")
 
	dtfrog.synsetsselected <- dtfrog.merged[synsetinpdocs > ll & synsetinpdocs < ul, .(docid,lemma,synsetinpdocs, synset)] %>% unique
    dtfrog.synsetsselected <- dtfrog.synsetsselected[!is.na(synset)]
			
	if(do_debug) cat("\t# Bereken hoe vaak een synset voorkomt in een document", "\r\n")
	dtfrog.synsetsselected <- dtfrog.synsetsselected[, nsynset := .N, by=c("docid","synset")]
     			
	if(do_debug) cat("\t# Maak een full factorial dt (documents x synsets)", "\r\n")
	dtfrog.fullfactorial <- expand.grid(docid  = sort(dtfrog$docid  %>% unique),
	                                    synset = sort(dtfrog.synsetsselected$synset %>% unique),
	                                    stringsAsFactors = F) %>% data.table
    

  	if(do_debug) cat("\t# Merge alle geobserveerde synset terug met de full factorial dt", "\r\n")
	dtfrog.synset.merged <- merge(x    = dtfrog.fullfactorial, 
	                              y    = dtfrog.synsetsselected,
	                             by    = c("docid","synset"),
	                             all.x = TRUE,
	                             all.y = TRUE )
	 
	if(do_debug) cat("\t# Stel nu de synsetfrequentie op 0 voor alle na's", "\r\n")
	dtfrog.synset.merged <- dtfrog.synset.merged[is.na(nsynset), nsynset:=0 ]

	if(do_debug) cat("\t# Merge de uitkomstvariabelen terug", "\r\n")
	dtfrog.synset.merged <- merge(x = dtfrog.synset.merged, 
	                              y = dtfrog[, c("docid",screening_options$dependent,screening_options$controls),with=F] %>% unique,
	                             by = c("docid"),
	                             all.x = T,
	                             all.y = T)											
	 								  			
	if(do_debug) cat("\t# Selecteer een synset vervolgens alleen als deze niet in de lijst met stopwoorden voorkomt: ", "\r\n")
	if(!is.null(exclude)){
		dtfrog.synsetsselected <- dtfrog.synsetsselected[! (synset %in% exclude), .(lemma,synset,lemmainpdocs)]
	} 
	 
	if(do_debug) cat("\t# Maak een vector met kolomnamen waarop gefilterd moet worden.", "\r\n") 
    t.col.names <- screening_options$dependents %+% screening_options$suffix 
	 
	# 
    dtfrog.synset.merged[ , (t.col.names) := feature_to_t_values(dtable     = .SD, 
														         feature    = screening_options$feature,
														         dependents = screening_options$dependents,
														         controls   = screening_options$controls,
														         family     = screening_options$family),  by=synset]

	if(do_debug) cat("\t# Bepaalde minimale en maximale t-waarde ", "\r\n")														 
	dtfrog.synset.merged[, `:=`(tmax=rowMaxs(as.matrix(.SD)), 
	                            tmin=rowMins(as.matrix(.SD))), .SDcols = t.col.names]
    
	if(do_debug) cat("\t# Bereken het product van de minimale en maximale waarde en klap het teken om", "\r\n")	
	if(do_debug) cat("\t# -- Toelichting: als -(max(t_waarde) * min(t-waarde)) >> groot, dan ", "\r\n")	
	if(do_debug) cat("\t#                 is de feature enerzijds een sterk negatieve voorspeller voor 1 bepaalde uitkomst ", "\r\n")	
	if(do_debug) cat("\t#                 en een positieve voorspeller voor een andere uitkomst ", "\r\n")	
	
	
	dtfrog.synset.merged[, tmaxmin := -tmax * tmin ] 
	
    
    synset.evaluate <- dtfrog.synset.merged[, c("synset", "tmaxmin", t.col.names), with=F]
    synset.evaluate <- synset.evaluate[order(tmaxmin, decreasing = T)] %>% unique
    
    synset.evaluate <- merge(x    = synset.evaluate, 
							 y    = synset.summaries %>% unique, 
							by    = "synset", 
							all.x = T, 
							all.y = F)[order(tmaxmin, decreasing = T)] 
   
    synset.evaluate[, synsetcode := prefix %+% .I] 	
   
	if(!is.null(screening_options$nmax)){
		synset.evaluate <- synset.evaluate[1:screening_options$nmax,]
	} 
	
	return(synset.evaluate)
 							   
}


special.lemmata.punctuation.dividers <- function(){
 
	punctuation <- c(";", ")", "(", "{", "}", "[","]",":",",")
	return(punctuation)
	 
}


special.lemmata.punctuation <- function(){
 
	punctuation <- c(".",";", ")", "(", "{", "}", "[","]","/","\\", ":", "\"",",","!","!!","%","$","*")
	return(punctuation)
	 
}


stopwords.dutch.extended <- function(){
	require(tm); 
	stopwords.dutch.base  <- stopwords(kind="nl")
	
	stopwords.dutch.extra <- c("aan","af","al","alles","als","altijd","andere","ben",
								"bij","daar","dan","dat","de","der","deze","die","dit",
								"doch","doen","door","dus","een","eens","en","er","ge",
								"geen","geweest","haar","had","heb","hebben","heeft",
								"hem","het","hier","hij","hoe","hun","iemand","iets",
								"ik","in","is","ja","je","kan","kon","kunnen","maar",
								"me","meer","men","met","mij","mijn","moet","na","naar",
								"niet","niets","nog","nu","of","om","omdat","ons","ook",
								"op","over","reeds","te","tegen","toch","toen","tot","u",
								"uit","uw","van","veel","voor","want","waren","was","wat",
								"we","wel","werd","wezen","wie","wij","wil","worden","zal",
								"ze","zei","zelf","zich" ,"zij","zijn","zo","zonder","zou",
								"zullen","komen","weten")
  
	return(stopwords.dutch.base %union% stopwords.dutch.extra %>% sort)
 
} 
 


