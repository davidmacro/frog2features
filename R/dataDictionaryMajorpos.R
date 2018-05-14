dictionary.majorpos <-function(){

	dictionary.majorpos <- data.table(majorpos         = c("ADJ", "BW", "LET", "LID","N","SPEC","TSW","TW","VG","VNW","VZ","WW"),
					                  description      = c("Adjectief", "Bijwoord (adverb)", "Leesteken", "Lidwoord", "Naamwoord",
									  "Speciaal",  "TSW", "Telwoord", "Voegwoord", "Voornaamwoord",  "Voorzetsel","Werkwoord"),
					                  stringsAsFactors = FALSE)
									  
	class(dictionary.majorpos) <- append(class(dictionary.majorpos), "dictionary")
	
	attr(dictionary.majorpos, "description")           <- "Dictionary of the major part-of-speech categories returned by the FROG parser"
	attr(dictionary.majorpos, "type")                  <- "majorpos"
	attr(dictionary.majorpos, "join_column")           <- "majorpos"
	attr(dictionary.majorpos, "action")                <- "count" 
	attr(dictionary.majorpos, "return.data.type")      <- "wide"
	attr(dictionary.majorpos, "return.na.value")       <- 0
	return(dictionary.majorpos)
}
	
	
	
	
	
	
	
	