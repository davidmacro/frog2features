# ---------------------------------------------------------------------------------------# 
#                                                                                        # 
#  Features :: 1 :: Meta-tekstuele kenmerken :: 1 :: Tekstlengtematen                    #
#                                                                                        #
# ---------------------------------------------------------------------------------------#                                                                                        
#                                                                                        #
#  Auteur: D.A. Macro                                                                    #
#		   d.a.macro@minvenj.nl // davidmacro@gmail.com		                             #
#                                                                                        #
# ---------------------------------------------------------------------------------------# 
#                                                                                        #
#	Input: 		Frog data-object                                                         #
#                                                                                        #
#	Output:		Aantal woorden per document                                              #
#			    Aantal zinnen per document                                               #
#				Aantal woorden per zin                                                   #
#				Aantal punctuaties                                                       #
#	                                                                                     #
# ---------------------------------------------------------------------------------------# 

f_meta_length_generic <- function(dtfrog,   
                                  normaliseer       =   FALSE, 
                                  calculation       =   "log1p",
                                  prefix            =   "fmeta_length"){

	require(data.table); require(magrittr)
	
	# Aantal woorden per document (f1)
	# Aantal zinnen per document (f2)
	# Aantal woorden per zin (f3)
	# Aantal punctuaties (f4)
	 
	dtfrog <- dtfrog %>% data.table %>% FixStringsFactors 
  
	# Als prefix niet gedefinieerd is, zet hem dan als "" 
	(prefix <- "") %if% (is.na(prefix) | is.null(prefix))
 
	# (1) Aantal woorden per document; filter hiervoor in het frog object alle leestekens eruit (majorpos != "LET")
	f1a.name  <- prefix %+% "nwords_doc"
	f1a.cols  <- c("docid", f1a.name) 
    
    f1a       <- dtfrog[, (f1a.name) := .N %>% log1p, by=c("docid")][, ..f1a.cols] %>% unique 
  
    # (1) Aantal unieke woorden per document; filter hiervoor in het frog object alle leestekens eruit (majorpos != "LET")
    f1b.name  <- prefix %+% "nwords_unique_doc"
	f1b.cols  <- c("docid", f1b.name) 
    f1b       <- dtfrog[, (f1b.name) := uniqueN(word)%>% log1p, by=c("docid")][!is.na(f1b.name), ..f1b.cols] %>% unique 
 
	# (2) Aantal zinnen per document
	f2.name <- prefix %+% "nsentence_doc"
	f2.cols <- c("docid", f2.name) 
	f2      <- dtfrog[, (f2.name) := uniqueN(sent)%>% log1p, by=c("docid")][, ..f2.cols] %>% unique 
 
	# (3) Gemiddeld aantal woorden per zin
    f3.name <- prefix %+% "nwords_sent"
    f3.cols <- c("docid", f3.name)
    
	dtfrog[, tmp_nwords_sent := sum(majorpos != "LET"), by=c("docid","sent")]
	
	f3      <- dtfrog[, .(docid,sent,tmp_nwords_sent)] %>% unique
  
	#  (-) het gemiddelde aantal woorden per zin binnen een document
	f3[,  (f3.name) := mean(tmp_nwords_sent), by=c("docid")]
	f3 <- f3[, ..f3.cols] %>% unique 
 
    
	#  (-) bereken het aantal keer dat er punctuatietekens ("majorpos == "LET") voorkomt
	f4.name <- prefix %+% "_npunct_doc"
	f4.cols <- c("docid", f4.name)
	f4      <- dtfrog[,  (f4.name) := length(majorpos != "LET"), by=c("docid")][, ..f4.cols]
	f4 %<>% unique
	
	return(f1a %frogmerge% f1b %frogmerge% f2 %frogmerge% f3 %frogmerge% f4)
	 
}
 