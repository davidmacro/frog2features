# ---------------------------------------------------------------------------------------#
#                                                                                        #
#  Features :: 1 :: Meta-tekstuele kenmerken :: 2 :: Lengte/verhoudingsmaten             #
#                                                                                        #
# ---------------------------------------------------------------------------------------#                                                                                    
#                                                                                        #
#  Auteur: D.A. Macro                                                                    #
#           d.a.macro@minvenj.nl // davidmacro@gmail.com                                 #
#                                                                                        #
# ---------------------------------------------------------------------------------------#
#                                                                                        #
#    Input:         Frog data-object                                                     #
#                                                                                        # 
#    Output: data.table met features geaggregeerd op documentniveau                      #
#                                                                                        #
#   --------------------------------------------------------------------------           #
#    Structuur outputobject:                                                             #
#   --------------------------------------------------------------------------           #
#                                                                                        #
#        --------------------------------------------------------------------------      # 
#        docid         		   document-id                                               #
#        --------------------------------------------------------------------------      #
#                                                                                        #
#        (f1)  [prefix]lc_avg     -  gemiddeld aantal tekens per zin in het document     #
#        (f2)  [prefix]lc_max     -  aantal tekens in langste zin in het document        #
#        (f3)  [prefix]lc_min     -  aantal tekens in kortste zin in het document        #
#                                 -                                                      #
#        (f4)  [prefix]lc_top10   -  gemiddeld aantal tekens in 10% langste zinnen       #
#        (f5)  [prefix]lc_top20   -  gemiddeld aantal tekens in 20% langste zinnen       #
#        (f6)  [prefix]lc_top80   -  gemiddeld aantal tekens in 80% langste zinnen       #
#        (f7)  [prefix]lc_top90   -  gemiddeld aantal tekens in 90% langste zinnen       #
#        (f8)  [prefix]lc_bot10   -  gemiddeld aantal tekens in 10% kortste zinnen       #
#        (f9)  [prefix]lc_bot20   -  gemiddeld aantal tekens in 20% kortste zinnen       #
#        (f10) [prefix]lc_bot80   -  gemiddeld aantal tekens in 80% kortste zinnen       #
#        (f11) [prefix]lc_bot90   -  gemiddeld aantal tekens in 90% kortste zinnen       #
#                                 -                                                      #
#        (f12) [prefix]lw_avg     -  gemiddeld aantal woorden per zin in het document    #
#        (f13) [prefix]lw_max     -  aantal woorden in langste zin in het document       #
#        (f14) [prefix]lw_min     -  aantal woorden in kortste zin in het document       #
#                                 -                                                      #
#        (f15) [prefix]lw_top10   -  gemiddeld aantal woorden in 10% langste zinnen      #
#        (f16) [prefix]lw_top20   -  gemiddeld aantal woorden in 20% langste zinnen      #
#        (f17) [prefix]lw_top80   -  gemiddeld aantal woorden in 80% langste zinnen      #
#        (f18) [prefix]lw_top90   -  gemiddeld aantal woorden in 90% langste zinnen      #
#        (f19) [prefix]lw_bot10   -  gemiddeld aantal woorden in 10% kortste zinnen      #
#        (f20) [prefix]lw_bot20   -  gemiddeld aantal woorden in 20% kortste zinnen      #
#        (f21) [prefix]lw_bot80   -  gemiddeld aantal woorden in 80% kortste zinnen      #
#        (f22) [prefix]lw_bot90   -  gemiddeld aantal woorden in 90% kortste zinnen      #
#                                                                                        #
# ---------------------------------------------------------------------------------------#
 
# ---------------------------------------------------------------------------------------#
# Referenties:                                                                           #
# ---------------------------------------------------------------------------------------#
#                                                                                        #
#     Zhang, C., Wu, X., Niu, Z., & Ding, W. (2014). Authorship identification from      #
#                        unstructured texts. Knowledge-Based Systems, 66, 99-111.        #
# ---------------------------------------------------------------------------------------#
  

f_meta_zhang2014 <- function(dtfrog, prefix = "f_meta_zhang2014_"){

    require(data.table); require(magrittr)
     
     
    dtfrog %>% FixStringsFactors -> dtfrog
    
    # -------------------------------------------------------------------------------------# 
    #  Onderstaande lengte- en verhoudingsmaten zijn ontleend aan:                         #
    #       Zhang, Wu, Niu, & Ding (2014)                                                  #
    # -------------------------------------------------------------------------------------#
                                                                                           #
    # -------------------------------------------------------------------------------------#    
    #  Structurele features (wij scharen dit onder meta features om al te veel subtypen    #
    #  te voorkomen; er is op dit vlak volgens mij nog geen algemeen aanvaarde typologie   #
    #  of terminologie.                                                                    #
    # -------------------------------------------------------------------------------------#
                                                                                           #
    # -------------------------------------------------------------------------------------#
    #  De structurele features van Zhang et al (2014) zijn combinaties van lengte-         #
    #  en lengteverhoudingsmaten, waarbij zowel op woord- als letterniveau wordt geteld.   #
    #                                                                                      #
    #  De terminologie van Zhang et al (2014) volgend:                                     #
    #                                                                                      #
    #  Hoofddtypen:                                                                        #
    #     lc = sentence character length  (aantal tekens per zin)                          #
    #     lw = sentence word length (aantal woorden per  zin)                              #
    #                                                                                      #
    #  Subtypen:                                                                           #
    #     _avg  = gemiddelde         _topXX = gemiddelde binnen top XX procent             #
    #     _max  = hoogste            _botXX = gemiddelde binnen top XX procent             #
    #     _min  = laagste                                                                  #
    #                                                                                      #
    #  E.e.a. is met data.table eenvoudig na te bouwen vanuit de FROG objecten:            #
    # -------------------------------------------------------------------------------------#
    
    # Aggregeer van woord-niveau naar zin-niveau; gebruik woorden (NIET de lemmata)
    dtfrog.sentencelevel <- dtfrog[, zin := paste0(word, collapse=' '), by=c("docid","sent")]
    
    # Selecteer alleen docid en zin; maak vervolgens per rij uniek. 
    dtfrog.sentencelevel <- dtfrog.sentencelevel[, .(docid,sent, zin)] %>% unique 

    # Fix het probleem dat aan het einde van de zin een " ." staat; deze moet "." worden. 
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " .", replacement = ".", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " ,", replacement = ",", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " ;", replacement = ";", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " :", replacement = ":", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " [", replacement = "[", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " ]", replacement = "]", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " {", replacement = "{", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " }", replacement = "}", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " !", replacement = "!", fixed=T, perl=T)]
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " ?", replacement = "?", fixed=T, perl=T)]    
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " /", replacement = "/", fixed=T, perl=T)]    
    dtfrog.sentencelevel[, zin := gsub(x=zin, pattern = " \\", replacement = "\\", fixed=T, perl=T)]     

    # Als prefix niet gedefinieerd is, zet hem dan als "" 
    (prefix <- "") %if% (is.na(prefix) | is.null(prefix))
    
    # -------------------------------------------------------------------------------------#    
    # (1) LC - Maten gebaseerd op het aantal tekens per zin                                #
    # -------------------------------------------------------------------------------------#
     
    # Bereken het aantal karakters per zin. 
    dtfrog.sentencelevel[, ncharacters := nchar(zin)]
 
    # Bereken per document of een zin qua lengte in het 1e, 2e, ..e, of 10e decile valt
    dtfrog.sentencelevel[, ntiles := .bincode(x              = ncharacters,
                                              breaks         = quantile(ncharacters, prob = c(0:10)/10),
                                              include.lowest = T,
                                              right          = T), by=c("docid")]
    
    
    dtfrog.sentencelevel <- dtfrog.sentencelevel[, .(docid,sent, ncharacters,ntiles )] %>% unique
    
    # LC: maten gebaseerd op de lengte van het aantal tekens in een zin
    
    scols <- c(prefix %&% "lc_avg",
               prefix %&% "lc_max",
               prefix %&% "lc_min",
               prefix %&% "lc_top10",  
               prefix %&% "lc_top20",   
               prefix %&% "lc_top80",   
               prefix %&% "lc_top90",
               prefix %&% "lc_bot10",
               prefix %&% "lc_bot20",
               prefix %&% "lc_bot80",
               prefix %&% "lc_bot90")
               
    setkeyv(dtfrog.sentencelevel, "docid")
    
    dtfrog.sentencelevel[,  (scols) := .(mean(ncharacters, na.rm = TRUE),
                                          max(ncharacters, na.rm = TRUE),
                                          min(ncharacters, na.rm = TRUE),
                                          .SD[ntiles >  9, mean(ncharacters, na.rm = TRUE)],
                                          .SD[ntiles >  8, mean(ncharacters, na.rm = TRUE)],
                                          .SD[ntiles >  2, mean(ncharacters, na.rm = TRUE)],
                                          .SD[ntiles >  1, mean(ncharacters, na.rm = TRUE)],
                                          .SD[ntiles <= 1, mean(ncharacters, na.rm = TRUE)],
                                          .SD[ntiles <= 2, mean(ncharacters, na.rm = TRUE)],
                                          .SD[ntiles <= 8, mean(ncharacters, na.rm = TRUE)],
                                          .SD[ntiles <= 9, mean(ncharacters, na.rm = TRUE)]),
                                        by=c("docid"), .SDcols=c("ntiles","ncharacters")]
 
	# Fix missings op de n-tiles
 
 	# --> Voor de missings op de 'top ntiles' maten is het eerst beschikbare ntile het maximale voorkomende ntile binnen het document
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_top10")), (prefix %&% "lc_top10") := .SD[ntiles == max(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")] 
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_top20")), (prefix %&% "lc_top20") := .SD[ntiles == max(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_top80")), (prefix %&% "lc_top80") := .SD[ntiles == max(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_top90")), (prefix %&% "lc_top90") := .SD[ntiles == max(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")]
	
	# --> Voor de missings op de 'bottom ntiles' maten is het eerst beschikbare ntile het minimale voorkomende ntile binnen het document
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_bot10")), (prefix %&% "lc_bot10") := .SD[ntiles == min(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_bot20")), (prefix %&% "lc_bot20") := .SD[ntiles == min(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_bot80")), (prefix %&% "lc_bot80") := .SD[ntiles == min(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencelevel[is.na(get(prefix %&% "lc_bot90")), (prefix %&% "lc_bot90") := .SD[ntiles == min(ntiles), mean(ncharacters, na.rm = TRUE)],by=c("docid")]	
  
    dtfrog.sentencelevel <- dtfrog.sentencelevel[, c("docid", scols), with=F] %>% unique
    
    # -------------------------------------------------------------------------------------#    
    # (2) LW - Maten gebaseerd op het aantal woorden in een zin                            #
    # -------------------------------------------------------------------------------------#
     
    # Bereken het aantal karakters per zin. 
    
	# -------------------------------------------------------------------------------------#
    # Negeer 'woorden' die in feite neerkomen op een leesteken of een nummer; als alternatief
    # hadden we ervoor kunnen kiezen om alles 'lengte > 1' te gebruiken; maar dan mis je
    # woorden zoals 'u', uitroepen zoals 'o' en 'a', etc. 
    # -------------------------------------------------------------------------------------#

    dtfrog.sentencewords <- dtfrog[, .(docid, sent, word)]

    filtercharacters <- c(".",",",";",":","[","]","{","}","!","?","/","\\",
                          "1","2","3","4","5","6","7","8","9" )

    dtfrog.sentencewords <- dtfrog.sentencewords[!(word %in% filtercharacters),] 

    dtfrog.sentencewords <- dtfrog.sentencewords[, nwords := .N, by=c("docid","sent")][ , .(docid, sent, nwords)]


    dtfrog.sentencewords[, ntiles := .bincode(x              = nwords,
                                              breaks         = quantile(nwords, prob = c(0:10)/10),
                                              include.lowest = T,
                                              right          = T), by=c("docid")]
 
    
    wcols <- c(prefix %&% "lw_avg",
               prefix %&% "lw_max",
               prefix %&% "lw_min",
               prefix %&% "lw_top10",  
               prefix %&% "lw_top20",   
               prefix %&% "lw_top80",   
               prefix %&% "lw_top90",
               prefix %&% "lw_bot10",
               prefix %&% "lw_bot20",
               prefix %&% "lw_bot80",
               prefix %&% "lw_bot90")
    
    dtfrog.sentencewords[,  (wcols) := .(mean(nwords, na.rm = TRUE),
                                         max(nwords,  na.rm = TRUE),
                                         min(nwords,  na.rm = TRUE),
                                         .SD[ntiles >  9, mean(nwords, na.rm = TRUE)],
                                         .SD[ntiles >  8, mean(nwords, na.rm = TRUE)],
                                         .SD[ntiles >  2, mean(nwords, na.rm = TRUE)],
                                         .SD[ntiles >  1, mean(nwords, na.rm = TRUE)],
                                         .SD[ntiles <= 1, mean(nwords, na.rm = TRUE)],
                                         .SD[ntiles <= 2, mean(nwords, na.rm = TRUE)],
                                         .SD[ntiles <= 8, mean(nwords, na.rm = TRUE)],
                                         .SD[ntiles <= 9, mean(nwords, na.rm = TRUE)]),
                                         by=c("docid"), .SDcols=c("ntiles","nwords")]
    
	# Fix missings; vervang door eerst volgend beschikbare ntile
	
	# --> Voor de missings op de 'top ntiles' maten is het eerst beschikbare ntile het maximale voorkomende ntile binnen het document
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_top10")), (prefix %&% "lw_top10") := .SD[ntiles == max(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")] 
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_top20")), (prefix %&% "lw_top20") := .SD[ntiles == max(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_top80")), (prefix %&% "lw_top80") := .SD[ntiles == max(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_top90")), (prefix %&% "lw_top90") := .SD[ntiles == max(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")]
	
	# --> Voor de missings op de 'bottom ntiles' maten is het eerst beschikbare ntile het minimale voorkomende ntile binnen het document
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_bot10")), (prefix %&% "lw_bot10") := .SD[ntiles == min(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_bot20")), (prefix %&% "lw_bot20") := .SD[ntiles == min(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_bot80")), (prefix %&% "lw_bot80") := .SD[ntiles == min(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")]
	dtfrog.sentencewords[is.na(get(prefix %&% "lw_bot90")), (prefix %&% "lw_bot90") := .SD[ntiles == min(ntiles), mean(nwords, na.rm = TRUE)],by=c("docid")]	
		
    dtfrog.sentencewords <- dtfrog.sentencewords[, c("docid", wcols), with=F]%>% unique
    
    return(dtfrog.sentencelevel %frogmerge% dtfrog.sentencewords) 
     
}

 
 

