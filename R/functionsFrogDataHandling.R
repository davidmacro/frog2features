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
   
create_dictionary <- function(dtfrog, token, prefix = "f_"){
     
    stop("The data-object does not contain a column named " %+% token) %if% (token %notin% (dtfrog %>% colnames))
 
    setkeyv(dtfrog, token)
    
    retval <- dtfrog[, .(count = .N), by=c(token)] 
    
    setorder(retval, -count)
   
    retval[, name := prefix %+% .I]
    setcolorder(retval, c("name", "count", token))
    
    return(retval)
    
}   
  
apply_tag <- function(dtfrog,  
                      where.token                 = "lemma",
                      where.data.table.expression = NULL,
                      where.regex                 = NULL,
                      where.list                  = NULL,
                      entities                    = NULL, 
                      keywords                    = NULL,
                      tag.column.name             = "is_tagged",  
                      casefold                    = 0,
                      tagged.value                = 1,
                      tagged.value.not            = 0,
                      tagged.value.expression     = NA,
                      tagged.value.expression.not = NA,
                      match.type                  = "match.expression",
                      set.not                     = 1,
                      type                        = "apply.tag"){
     
    cat("\r\n\t Apply tag to frog object:  ") 
    cat(match.type %+% ": " %+% where.regex)                 %if% (where.regex %>% is.null %>% not)
    cat(match.type %+% ": " %+% where.data.table.expression) %if% (where.data.table.expression %>% is.null %>% not)
    
    
    # First process the recursive case:  
    if(match.type == "match.entity.list"){
         
        fname         <- "apply_tag" 
        
        dt.tmp <- dtfrog %>% copy
     
        entities[, set.not := 0]
        entities[, group := .GRP, by = tag.column.name][, by = "group"]
        entities[, set.not :=  .SD[,.I == 1]  * 1.0 , by = "group"]
        
        for(i in 1:nrow(entities)){
        
            entity <- entities[i] %>% as.list %>% lookupArgs(fname, .)
            
            dt.tmp <- apply_tag(dtfrog                      =  dt.tmp,
                                where.token                 =  entity$where.token,
                                where.data.table.expression =  entity$where.data.table.expression,
                                where.regex                 =  entity$where.regex,
                                where.list                  =  entity$where.entities,
                                entities                    =  entity$entities, 
                                keywords                    =  entity$keywords,
                                tag.column.name             =  entity$tag.column.name,  
                                casefold                    =  entity$casefold,
                                tagged.value                =  entity$tagged.value,
                                tagged.value.not            =  entity$tagged.value.not,
                                tagged.value.expression     =  entity$tagged.value.expression,
                                tagged.value.expression.not =  entity$tagged.value.expression.not,
                                match.type                  =  entity$match.type,
                                set.not                     =  entity$set.not) 
        }  
        
        return(dt.tmp)
        
    }  
    
    if(match.type == "match.data.table.expression"){
        
          c1 <- (where.data.table.expression %>% is.null)
          c2 <- (tag.column.name             %>% is.null)
          c3 <- (tagged.value                %>% is.null)
          c4 <- (tagged.value.expression     %>% is.na)
          c5 <- (tagged.value.not            %>% is.null)
          c6 <- (tagged.value.expression.not %>% is.na)
         
          if(any(c1, c2, (c3 %xor% c4 ) %>% not, (c5 %xor% c6 ) %>% not)){  
              stop("Invalid argument(s). Match variant \"match.data.table.expression\" requires: \r\n 
                   \t \"where.data.table.expression\" \r\n
                   \t \"tag.column.name\" \r\n
                   \t \"tagged.value\" or \"tagged.value.not.expression\"\r\n
                   \t \"tagged.value.not\" or \"tagged.value.not.expression\"\r\n")

          } else {  
              
              if(set.not){
                  dtfrog[, (tag.column.name) := tagged.value.not]
              }  
              
             return(
                 dtfrog[
                    where.data.table.expression %>% parse(text = .) %>% eval, 
                        (tag.column.name) := tagged.value
                 ] %>% setorderv(c("docid", "sent", "position"))
             )
          } 
        
    }
    
    if(match.type == "match.list"){
  
         if(where.list %>% is.null){ 
             
             stop("Invalid argument(s). ")
             
         } else { 
             
            c1 <- (where.token %>% is.null)
            c2 <- (tag.column.name             %>% is.null)
            c3 <- (tagged.value                %>% is.null)
            c4 <- (tagged.value.expression     %>% is.na)
            c5 <- (tagged.value.not            %>% is.null)
            c6 <- (tagged.value.expression.not %>% is.na)
             
            if(any(c1, c2, (c3 %xor% c4 ) %>% not, (c5 %xor% c6 ) %>% not)){
             
            stop("Invalid argument(s). Match variant \"match.regular.expresion\" requires: \r\n 
                   \t \"where.regex\" \r\n
                   \t \"where.token\" \r\n
                   \t \"tag.column.name\" \r\n
                   \t \"tagged.value\" or \"tagged.value.not.expression\"\r\n
                   \t \"tagged.value.not\" or \"tagged.value.not.expression\"\r\n") 
                
            } else {
                if(where.list %>% is.list){ 
                    
                } else { 
                     if(where.list %>% is.character){ 
                        if(length(where.list) == 1){
                            
                            where.list <- where.list %>% call %>% eval 
                            
                            if(where.list %>% is.data.table){
                                where.list <- where.list[match.type == "match.regular.expression", where.regex , by = tag.column.name][,where.regex]
                            }
                            
                        } 
                     }
                 } 
            } 
        }
        
        if(set.not){
             dtfrog[, (tag.column.name) := tagged.value.not] 
        }
 
        return(dtfrog)
        
    }
     
    
    if(match.type == "match.regular.expression"){
        
        c1 <- (where.token %>% is.null)
        c2 <- (tag.column.name             %>% is.null)
        c3 <- (tagged.value                %>% is.null)
        c4 <- (tagged.value.expression     %>% is.na)
        c5 <- (tagged.value.not            %>% is.null)
        c6 <- (tagged.value.expression.not %>% is.na)
 
        
        if(any(c1, c2, (c3 %xor% c4 ) %>% not, 
                       (c5 %xor% c6 ) %>% not)){
             
            stop("Invalid argument(s). Match variant \"match.regular.expresion\" requires: \r\n 
                   \t \"where.regex\" \r\n
                   \t \"where.token\" \r\n
                   \t \"tag.column.name\" \r\n
                   \t \"tagged.value\" or \"tagged.value.not.expression\"\r\n
                   \t \"tagged.value.not\" or \"tagged.value.not.expression\"\r\n")
             
            
        } else
            
            if(set.not){
                dtfrog[, c(tag.column.name):= tagged.value.not ];    
            }
            
            if(casefold){
                dtfrog[casefold(get(where.token)) %like%  casefold(where.regex),  c(tag.column.name) := tagged.value]  
            } else {
                dtfrog[get(where.token) %like%  where.regex,  c(tag.column.name) := tagged.value]  
            }
        
            setorderv(dtfrog, c("docid", "sent", "position"))  
            
        return(dtfrog)
        
    }
  
      
} 

replace_in_frog <- function(dtfrog, where, where.list = NA, token = "lemma", value.expression = NA, value = NA, value.get = NA, prefix = "", suffix = ""){
     
    cat("\t\t\t\t\t\ Replace in frog: \r\n")
    cat("\t\t\t\t\t\t   where: " %+% where %+% "\r\n")
    cat("\t\t\t\t\t\t replace: " %+% token %+% "\r\n")
    cat("\t\t\t\t\t\t    with: " %+% value.expression %+% "\r\n") %ifnotna% value.expression
    cat("\t\t\t\t\t\t    with: " %+% prefix %+% value %+% suffix %+% "\r\n")            %ifnotna% value
 
    # Replace 
    if(where.list %>% is.na %>% all %>% not){
        
        dt.tmp <- dtfrog %>% copy
        
        for(where.item in where.list){
             
           cat("Replacing: ", where.item, "\r\n")     %if% (1==1)    
            
           where.string <- where %+% " %like% " %+%  "\"" %+% where.item %+% "\""
       
           dt.tmp <- replace_in_frog(dtfrog           = dt.tmp, 
                                     where            = where.string,
                                     token            = token,
                                     value.expression = value.expression,
                                     value            = value,
                                     value.get        = value.get,
                                     prefix           = prefix,
                                     suffix           = suffix)
        }
        
        return(dt.tmp)
    }
    
     
    if(value %>% is.na %>% not){ 
        dtfrog[ where %>% parse(text = .) %>% eval, lemma := prefix %+% value %+% suffix ] %>% 
            setorderv(c("docid", "sent", "position")) %>% 
            return
    } else { 
        if(value.get %>% is.na %>% not){ 
            dtfrog[ where %>% parse(text = .) %>% eval, lemma := prefix %+% (value.get %>% get) %+% suffix] %>% 
                setorderv(c("docid", "sent", "position")) %>% 
                return
        } else { 
            if(value.expression %>% is.na %>% not){ 
                dtfrog[ where %>% parse(text = .) %>% eval, lemma := prefix %+% ((value.expression %>% parse(text = .) %>% eval)) %+% suffix] %>%
                    setorderv(c("docid", "sent", "position")) %>% 
                    return
            } 
        } 
    } 
     
    
}

calculate_frequency_measures <- function(dtfrog, 
										 token       = "lemma", 
										 return.type = "all",
										 setkeys     = T){

	dtfrog <- dtfrog %>% data.table %>% FixStringsFactors
	
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
			return(dtfrog[, c(token,name1,name2), by = c(token), with=F] %>% unique)
		} else {
			return(FALSE)
		}	
	}   
}
   
exclude_on_token_content <- function(dtfrog, token, items, exclude.what = "inside", ...){
 
    
    stop("Error: the dtfrog does not contain a column named " %+% token) %if% (token        %notin% (dtfrog %>% colnames))
    stop("Error: exclude.what should be 'inside' or 'outside'.")         %if% (exclude.what %notin% c("inside","outside"))

    if(exclude.what == "inside"){
        {dtfrog %>% data.table %>% FixStringsFactors }[!(get(token) %chin% items), ] %>% return
        
    } else {
        {dtfrog %>% data.table %>% FixStringsFactors }[(get(token) %chin% items), ] %>% return    
    }
     
}  
    

exclude_on_token_prevalence  <- function(dtfrog , 
										 token                    =  "lemma",  
										 count.type               =  "frequency",
										 exclude.what             =  "outside", 
								         ll                       =  0,
										 ul                       =  +Inf,
										 external.calculation     =  FALSE,
										 setkeys                  =  FALSE,
										 between                  =  FALSE,
									     requireStrictFrog        =  TRUE){
     
    # Checks:		             
    stop("Error: remove.what should either be 'inside' or 'outside'.")   %if%  (!(exclude.what %in% c("inside","outside")))
 	stop("Error: count.type  should either be 'frequency' or 'ratio'.")  %if%  (!count.type %in% c("frequency","ratio"))
    stop("Error: the dtfrog does not contain a column named " %+% token) %if%  (!(token %in% (dtfrog %>% colnames)))
 
	# If we've made it here, do the actual work...
	# Determine whether to filter on frequencies or on ratios
	tmpdata <- dtfrog %>% data.table %>% copy %>% FixStringsFactors 

    name_n      <- token %+% "_in_ndocs"
    name_p      <- token %+% "_in_pdocs"
	name_tfid1  <- token %+% "_tfidf1"
    
	if(external.calculation){
	    
        tmpdata <- tmpdata %>% calculate_frequency_measures(token = token, return.type = "all",setkeys = setkeys )
        
	} else { 
        
	    setkeyv(tmpdata, token) %if% setkeys
	    
        ndocs  <- uniqueN(tmpdata$docid)  
 
	    tmpdata[, (name_n) := uniqueN(docid), by = c(token)]  	
		tmpdata[, (name_p) := get(name_n) / ndocs ,]   
		
		     
    	ids <- c("docid","sent","position")
        
    	setkeyv(tmpdata, ids) %if% setkeys
    	    
    	setcolorder(tmpdata,neworder = c(ids, colnames(tmpdata) %-% ids))
	} 
	
	
	if(between) { 
     	if(count.type == "frequency"){  
    		if(exclude.what == "inside"){
    			tmpdata[!between(get(token %+% "_in_ndocs"), ll, ul, incbounds=FALSE),] %>% return 
    		} else {
    			tmpdata[between(get(token %+% "_in_ndocs"),  ll, ul),] %>% return
    		}
    	} else {
    		if(exclude.what == "inside"){
    			tmpdata[!between(get(token %+% "_in_pdocs"), ll, ul, incbounds=FALSE),] %>% return
    		} else {
    			tmpdata[between(get(token %+% "_in_pdocs"),  ll, ul),] %>% return
    		}
    	}		 
    	    
	} else {
	    if(count.type == "frequency"){  
    		if(exclude.what == "inside"){
    			tmpdata[get(name_n) < ll | get(name_n) > ul,] %>% return 
    		} else {
    			tmpdata[get(name_n) > ll & get(name_n) < ul,] %>% return
    		}
    	} else {
        	if(exclude.what == "inside"){
    			tmpdata[get(name_p) < ll | get(name_p) > ul,] %>% return 
    		} else {
    			tmpdata[get(name_p) > ll & get(name_p) < ul,] %>% return
    		}
    	}	
	}
 }
  
`%frapply%` <- function(dtfrog, dictionary){
	
	# Check if dtfrog is a frog object
	
	# Check if dictionary is a valid dictionary object
	
	# If (1) and (2), apply the dictionary to the frog data.table object and return the value. 


} 

# create_semantics <- function(dtfrog            = NULL, 
#                              types             = c("synsets","hypernyms"),
#                              synset.properties = synset.properties.dutch,
#                              synset.hypernyms  = synset.hypernyms.dutch) {
#   
#     dtfrog <- dtfrog %>% data.table %>% FixStringsFactors
#   
# 	is.interactive <- TRUE	
# 					 	
# 	if(is.null(dtfrog)){ 
# 		is.interactive <- FALSE
# 		
# 		dtfrog <- self$dtfrog[,.(docid, word, lemma, sent, position, majorpos)] %>% copy
#  
# 		
# 	}  else { 
# 	    
# 		dtfrog <- dtfrog[, .(docid, word, lemma, sent, position, majorpos)] %>% self$fix_strings_as_factors  
# 		
# 	}
#  
# 	invisible(self)
#  
# }  

create_syntactic_graph <- function(dtfrog, docid, sent, return.type = "data.table"){
 
    dtfrog <- {dtfrog %>% data.table}[, .(docid, word, lemma, sent, position, pos, majorpos, parse1, parse2,  morph)] %>% FixStringsFactors
  
	docid.select  <- docid
	sent.select   <- sent
	 
	vertices.data <- dtfrog[docid == docid.select & sent == sent.select,]  
	
	left.part     <- vertices.data[, .( node                  = position, 
									    node.parent           = parse1,
									    reltype               = parse2,
									    node.pos              = pos, 
									    node.morph            = morph, 
									    node.majorpos         = majorpos,
									    node.word             = word)]      
	
	right.part <- vertices.data[position %in% left.part$node.parent ,
									.( node              = position, 
										parent.majorpos    = majorpos,
										parent.word        = word)]    
			
	merged <- merge(x     = left.part, 
					y     = right.part , 
					by.x  = "node.parent",
					by.y  = "node", 
					allow.carthesian = T,
					all.x = T,
					all.y = F )[order(node)]
					
	
	cols.first <- c("node", "node.parent", "reltype", "node.word", "parent.word")
	cols.order <- c(cols.first, c(colnames(merged) %-% cols.first) %>% sort)
	
	retval <- FALSE
	
	if(return.type == "data.table"){
		retval <- merged
	}   
	
	if(return.type %in% c("igraph", "igraph.plot")){
	
		# Stel de loops goed in
		merged[node.parent == 0, node.parent:=node] 
	
		s.graph <- merged[, .(node.parent,node, reltype)] %>% 
							graph.data.frame(vertices = merged[, .(node,node.word)]) %>% 
							simplify(remove.multiple = F, remove.loops = T)
		
	
		if(return.type == "igraph.plot"){
		
			retval <- plot(s.graph, layout = layout_(s.graph, as_tree()),
									vertex.shape       = "none" ,
									vertex.label.cex   = 1.2,
									vertex.label.color = "darkblue",
									edge.arrow.size    = 0.2,   
									vertex.label       = V(s.graph)$node.word,
									edge.label         = E(s.graph)$reltype, 
									edge.label.cex     = 0.8,
									edge.label.color   = "darkred")
	
			title(main = "Syntactic Dependency Graph",   
				  sub = merged[, node.word %>% paste0(collapse = ' ')]) 
					
		}  else {   
			return(s.graph)  
		}	 
	
		retval <- merged[, .(node, node.parent)]
	}   
			  
	return(merged) 
		 
} 
   
calculate_count <- function(dtfrog         = NULL, 
                            dictionary     = NULL,
                            token.x        = "lemma",
                            token.y        = "lemma",
                            return.type    = "wide",
                            return.measure = "count"){
  
     
    cat("# Create full factorial frame\r\n")
    dt.ff <- CJ(docid = dtfrog$docid %>% unique %>% sort, 
                token = dictionary[[token.y]] %>% unique %>% sort, unique = TRUE) 
    
    cat("# Merge dictionaryon token.x = " %+% token.x %+% 
        "                     token.y = " %+% token.y)
    
    dt.ff <- merge(
        x   = dt.ff, 
        y   = {dictionary %>% copy}[,count:=NULL], 
        by.x=c("token"), 
        by.y=c(token.y)
    ) 
    
    cat("# Make aggregated dtfrog\r\n")
    dtfrog.aggregated <- dtfrog[, c("docid", token.x), with=F] %>% copy
     
    # (tf) calculate the term frequency by counting the number of times the term occurs in the document 
    dtfrog.aggregated <- dtfrog.aggregated[, token_in_doc_n:=.N, by = c("docid", token.x)] %>% unique
     
    cat("# Make merged dtfrog\r\n") 
    dt.merged <- merge(x = dt.ff,
                       y = dtfrog.aggregated,
                       by.x = c("docid","token"), 
                       by.y = c("docid",token.x),
                       all.x = T,
                       all.y = F)[token_in_doc_n %>% is.na, token_in_doc_n := 0] 
      
    # (idf) calculate the number of documents in the corpus: 
    dt.merged[, ndocs := uniqueN(docid)] 
     
    # calculate the number of documents in which the term can be found 
    dt.merged[, token_in_n_doc := sum(token_in_doc_n > 0) + 1L , by = c("token")]
    
    # calculate the number of documents in which the term can be found 
    dt.merged[, tfidf := token_in_doc_n * log(ndocs/token_in_n_doc)]
       
    if(return.type == "wide"){
        
        return.formula <- as.formula("docid ~ name" )  
      	
        if(return.measure == "count"){
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


merge_syntactic <- function(dtfrog, 
                            token, 
                            exclude 			= NULL, 
                            exclude.frequency 	= NULL,
                            include 			= NULL, 
                            select 				= NULL){
     
	dtfrog <- dtfrog %>% data.table %>% FixStringsFactors
	 
 
	dtfrog.merged <-  merge(x        = dtfrog,
							y        = dtfrog,
							by.x     = c("docid","sent","parse1"), 
							by.y     = c("docid","sent","position"), 
							suffixes = c("_from","_to"))[order(docid, sent, position)]
	
	
	dtfrog.merged <- dtfrog.merged[, ngram := get(token %+% "_from") %+% " " %+%get(token %+% "_to")]
	dtfrog.merged <- dtfrog.merged[,.(docid, sent, position, ngram, syntrel = parse2_from, majorpos_from, majorpos_to)]
	
    if(!is.null(exclude)){
        dtfrog.merged <- dtfrog.merged[!(ngram %chin% exclude$ngram),]
    }   
	return(dtfrog.merged) 
}  
 
 
create_ngram_fast <- function(dtfrog,  
                              degree              = 1, 
                              tokens              = c("lemma","lemma"), 
                              filters             = list(majorpos_1 = list(default.filter.rule("exclude.punctuation")),
                                                         majorpos_2 = list(default.filter.rule("exclude.punctuation")),
                                                            lemma_1 = list(default.filter.rule("exclude.dutch.stopwords")),
                                                            lemma_2 = list(default.filter.rule("exclude.dutch.stopwords"))),
                             proximity.criterium = "adjacency",
                             verbose             = F,
                             return.type         = "call"){
	
    dtfrog.tmp <- dtfrog %>% data.table %>% FixStringsFactors
	
    stop("No dtfrog provided") %if% ("dtfrog" %>% exists %>% not)
    
    cat("\t\t\t\t\t Create n-gram with: \r\n")
    cat("\t\t\t\t\t\t degree: " %+% degree %+% "\r\n")
    
    for(token in tokens){
        cat("\t\t\t\t\t\t tokens: " %+% token %+% "\r\n") 
    }
    
    cat("\t\t\t\t\t\t proximity.criterium: " %+% proximity.criterium %+% "\r\n")
    
	# Length = degree + 1  
	ngram_length <- degree +1 
	
	dtfrog.tmp[, ngram_valid := 0]
	  
	cols.to.lag  <- list()
	filter.names <- filters %>% names
     
	
	for(i in 1:ngram_length){ 
	   cols.to.lag[[i]] <- c(tokens[i], "position",c(str_subset(filter.names, pattern = "_" %+% i) %>% str_replace(pattern = "_" %+% i, ""))) %>% unique
	}
	 
	if(proximity.criterium == "adjacency"){
    	    
        # Set the order of dtfrog; we cannot assume that the provided object is given in correct order
 
	    setorderv(dtfrog.tmp, cols =  c("docid","sent", "position"))
	    
        for(i in 1:ngram_length){ 
             
	        cols.lagged <- cols.to.lag[[i]] %+% "_" %+% i
	    
	        dtfrog.tmp[, c(cols.lagged) := shift(.SD, i-1, type="lead"),  by = c("docid", "sent"), .SDcols = cols.to.lag[[i]]]
	            
	        dtfrog.tmp[cols.lagged[[i]] %>% get %>% is.na %>% not, ngram_valid := i]
	        
	        if(i > 1){
	            dtfrog.tmp <- dtfrog.tmp[get("position_" %+% (i-1)) != (get("position_" %+% (i)) + 1),]     
	        }
	        
	    }  
	}
	 
	if(proximity.criterium == "syntactic"){
	    
	    # Note: currently only syntactic bygrams
	     
	    cols.pos <- which(colnames(dtfrog.tmp) %in% c((colnames(dtfrog.tmp) %-% c("docid", "sent","position")))) 
        cols.val <- colnames(dtfrog.tmp)[cols.pos]
	  
        for(i in 1:1){  
	    
	         dtfrog_l <- dtfrog.tmp 
             dtfrog_r <- dtfrog.tmp %>% copy
	         
             col.pos.l <- which(colnames(dtfrog_l) %in% c((colnames(dtfrog_l) %-% c("docid", "sent","position")))) 
             col.pos.r <- which(colnames(dtfrog_r) %in% c((colnames(dtfrog_r) %-% c("docid", "sent","position")))) 
              
	         colnames(dtfrog_l)[col.pos.l] %<>% paste0("_" %+% i)
             colnames(dtfrog_r)[col.pos.r] %<>% paste0("_" %+% (i+1))
           
             dtfrog_l[, position := get("parse1_" %+% (i))]  
              
	         dtfrog.tmp <-  merge(x        = dtfrog_l, 
                                  y        = dtfrog_r,  
                                  by.x     = c("docid","sent","position"),
                                  by.y     = c("docid","sent","position"), all = T)
	          
	         dtfrog.tmp[(("word_" %+% (i))   %>% get %>% is.na %>% not &
	                     ("word_" %+% (i+1)) %>% get %>% is.na %>% not), 
	                     ngram_valid := (i+1)]
	     } 
   
	} 
	
	cols.to.glue  <- tokens %+% "_" %+% 1:ngram_length
	    
    # Filter all incomplete ngrams:   
    dtfrog.tmp <- dtfrog.tmp[ngram_valid == ngram_length,]
	     
    # Apply all filters as defined in filters: 
    for(token.name in (filters %>% names)){   
          for(filter.rule in filters[[token.name]]) {   
              
              filter.rule$token <- token.name
              
              dtfrog.tmp <- getCallFromRule(rule = filter.rule, data.input = dtfrog.tmp, verbose = F) 
          }  
    } 
     
    dtfrog.tmp$ngram <- do.call(paste, c(dtfrog.tmp[,..cols.to.glue], sep = " - ")) 
	  
    return(dtfrog.tmp)
	 
}
  
 
save_dictionary = function(dtfrog, 
                           filename){
     saveRDS(dtfrog, file=filename)
     return(dtfrog)
}

load_dictionary = function(filename){
     return(readRDS(file=filename))
}
 
apply_dictionary = function(){
    
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
 
## Obsolete functions: 
frog2lemmacloud <- function(dtfrog, sparsity = .90, max.words = 120, stopwords = stopwords.dutch.extended()){

	# Maak een wordcloud van een frog object 
	# require(data.table); require(magrittr); require(tm)

	cat("\r\n")
	cat("    #----------------------------------------------------------------------------\r\n")
	cat("    # Functie: Poldata_wordcloud() \r\n")
	cat("    #----------------------------------------------------------------------------\r\n")
	cat("    #  [-] Haal de frogobjecten op uit het argument en wijs toe: \r\n")
	cat("    #----------------------------------------------------------------------------\r\n")
	
	dtfrog %>% FixStringsFactors -> dtfrog

	cat("    #  [-] Sorteer het frogobject o.b.v. docid, sent en position (positie van woord binnen zin) \r\n")
	setkeyv(dtfrog, c("docid", "sent", "position"))

	dtfrog <- dtfrog[, .(docid, sent, position, lemma)]

	cat("    #  [-] Maak van het frogobject weer zinnen (plak per docid e.e.a. weer aan elkaar) \r\n")
	dtfrog.sentencelevel <- dtfrog[, zin := paste0(lemma, collapse=' '), by=docid][, .(docid,zin)] %>% 
		unique() %>%
		data.frame()

	cat("    #  [-] Definieer een ds object om met de tm library te gebruiken \r\n")
	ds <- DataframeSource(dtfrog.sentencelevel)

	cat("    #  [-] Maak het corpus aan:")
	corpus <- VCorpus(ds)
 
 	cat("    #  [-] Pas datatransformaties toe")
	corpus = tm_map(corpus, content_transformer(tolower))
	corpus = tm_map(corpus, removeNumbers)
	corpus = tm_map(corpus, removeWords, stopwords)
	corpus = tm_map(corpus, removePunctuation)

	cat("    #  [-] Maak een document term matrix")
	dtm <- DocumentTermMatrix(corpus)

	cat("    #  [-] Maak een document term matrix")
	dtm <- removeSparseTerms(dtm, sparsity)

	cat("    #  [-] Maak een document term matrix")
	freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))

	cat("    #  [-] Maak een wordcloud")
	wordcloud(rownames(freq), freq[,1], max.words=max.words, colors=brewer.pal(1, "Dark2"))

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
 
  
