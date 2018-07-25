#' Describe FROG
#'
#' @param verbose (default = TRUE)
#'
#' @return Printed text with information on FROG.
#' @export
#'
#' @examples
#' \notrun{
#' describe.frog()
#' 
#' }
describe.frog <- function(verbose = T){
    
	
	print.message = "
# ------------------------------------------------------------------------------------------------------------- #
# FROG                                                                                                          #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   webite: http://languagemachines.github.io/frog/                                                             #
#                                                                                                               #
# citation: Van den Bosch, A., Busser, G.J., Daelemans, W., and Canisius, S. (2007). An efficient memory-based  #
#               morphosyntactic tagger and parser for Dutch, In F. van Eynde, P. Dirix, I. Schuurman, and V.    #
#               Vandeghinste (Eds.), Selected Papers of the 17th Computational Linguistics in the Netherlands   #
#               Meeting, Leuven, Belgium, pp. 99-114                                                            #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
# Excerpt from FROG website:                                                                                    #
#                                                                                                               #
#  'Frog is an integration of memory-based natural language processing (NLP) modules developed for Dutch.       #
#   All NLP modules are based on Timbl, the Tilburg memory-based learning software package.                     #
#                                                                                                               #
#                                                                                                               #
#   Most modules were created in the 1990s at the ILK Research Group (Tilburg University, the Netherlands)      #
#   and the CLiPS Research Centre (University of Antwerp, Belgium). Over the years they have been integrated    #
#   into a single text processing tool, which is currently maintained and developed by the Language Machines    #
#   Research Group and the Centre for Language and Speech Technology at Radboud University Nijmegen.            #
#                                                                                                               #
#                                                                                                               #
#   A dependency parser, a base phrase chunker, and a named-entity recognizer module were added more recently.  #
#   Where possible, Frog makes use of multi-processor support to run subtasks in parallel.                      #
#                                                                                                               #
#                                                                                                               #
#   Various (re)programming rounds have been made possible through funding by NWO, the Netherlands Organisation #
#   for Scientific Research, particularly under the CGN project, the IMIX programme, the Implicit Linguistics   #
#   project, the CLARIN-NL programme and the CLARIAH programme.'                                                #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- # 
#                                                                                                               # 
#   Other useful references:                                                                                    # 
#                                                                                                               #
#       Oostdijk, N., Reynaert, M., Hoste, V., & Schuurman, I. (2013). The construction of a 500-million-word   #
#           reference corpus of contemporary written Dutch. In Essential speech and language technology for     #
#           Dutch (pp. 219-247). Springer, Berlin, Heidelberg.                                                  #
#                                                                                                               #
#       Verberne, S., Boves, L., Oostdijk, N., & Coppen, P. A. (2010). What is not in the Bag of Words          #
#           for Why-QA?. Computational Linguistics, 36(2), 229-245.                                             # 
#           https://www.mitpressjournals.org/doi/pdf/10.1162/coli.09-032-R1-08-034                              #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Usefull commands:                                                                                           #
#                                                                                                               #
#      describe.frog()                 Describe FROG                                                            #
#      describe.frog.columns()         Describe the columns in a FROG output file                               # 
#      describe.majorpos.types()       Describe the main Part-of-Speech types (major) as identified by FROG     # 
#      describe.describe.ner.types()   Describe the main Named Entitites recognized by FROG                     # 
#      describe.chunk.types()          Describe the chunking                                                    # 
#      describe.syndep.labels()        Describe the types of syntactic dependencies FROG can output.            # 
# ------------------------------------------------------------------------------------------------------------- #

"
	
	if(verbose){
		print.message %>% cat
	}
}

#' Describe FROG Columns
#'
#' @param verbose (default = TRUE) 
#' @return A data.table with a description of each column.  
#'         printed text with information on the FROG columns 
#'         (side effect) 
#' @export
#' @seealso 
#'  \code{\link{describe.frog}}                 
#'            \code{\link{describe.frog.columns}}         
#'  \code{\link{describe.majorpos.types}}      
#'  \code{\link{describe.describe.ner.types}} 
#'  \code{\link{describe.chunk.types()}}        
#'  \code{\link{describe.syndep.labels()}}
#' @examples
describe.frog.columns <- function(verbose=TRUE){

	print.message <- "
# ------------------------------------------------------------------------------------------------------------- #
# Columns in the FROG output                                                                                    #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   webite: http://languagemachines.github.io/frog/                                                             #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Excerpt from the FROG website                                                                               #
#                                                                                                               #
#     Frog's current version will tokenize, tag, lemmatize, and morphologically segment word tokens in          #
#     Dutch text files, will assign a dependency graph to each sentence, will identify the base phrase chunks   #
#     in the sentence, and will attempt to find and label all named entities.                                   #
#                                                                                                               #
#                                                                                                               # 
#   Usefull commands:                                                                                           #
#      describe.frog()                 Describe frog                                                            #
#      describe.frog.columns()         Describe the columns in a FROG output file                               # 
#      describe.majorpos.types()       Describe the main Part-of-Speech types (major) as identified by FROG     # 
#      describe.describe.ner.types()   Describe the main Named Entitites recognized by FROG                     # 
#      describe.chunk.types()          Describe the chunking                                                    # 
#      describe.syndep.labels()        Describe the types of syntactic dependencies FROG can output.            # 
# ------------------------------------------------------------------------------------------------------------- # "
	if(verbose){
		print.message %>% cat
	}
	
	frog.columns <- data.table(
		columns = c(
			"docid","sent","position","word","lemma","morph","pos","majorpos","prob","ner","chunk","parse1","parse2"
		),
		description = c(
			"unique document identifier", 
			"within document sentence identifier",
			"position of word within sentence",
			"word or phrase as found in the original document (minimally processed)",
			"Lemma (according to MBLEM)",
			"Morphological segmentation (according to MBMA)",
			"Part-of-speech (POS) tag (CGN tagset; according to MBT)", 
			"Major categories of POS", 
			"Confidence in the POS tag, a number between 0 and 1",
			"Named entity type (ner) using a BIO (or IOB2) encoding", 
			"Base (non-embedded) phrase chunk in BIO encoding",
			"Token number of head word in dependency graph (according to CSI-DP)",
			"Type of dependency relation with head word"
		),
		stringsAsFactors = FALSE
	) 
	 
	return(frog.columns)
}	
 
describe.majorpos.types <- function(verbose=TRUE){
 
	print.message <- "
# ------------------------------------------------------------------------------------------------------------- #
# Part-of-Speech tags (Major types)                                                                             #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
# Excerpt from the FROG manual                                                                                  #
#                                                                                                               #
#    'The Part-of-Speech tagger uses the tag set of Corpus Gesproken Nederlands (CNG)' (p. 10)                  #
#                                                                                                               #
#       (...)                                                                                                   #
#                                                                                                               #
#    'The PoS tagger in Frog is based on MBT, a memory-based tagger-generator and tagger2(?) trained on a       #
#     large Dutch corpus of 10,975,324 tokens in 933,891 sentences. This corpus is a mix of several manually    #
#     annotated corpora but about 90% of the data comes from the transcribed Spoken Dutch Corpus of about nine  #
#     million tokens (?). The other ten precent of the training data comes from the ILK corpus (46K tokens),    #
#     the D-Coi corpus (330K tokens) and the Eindhoven corpus (75K tokens) citeuit den Boogaart 1975 that were  #
#     re-tagged with CGN tag set. The tag set consists of 12 coarse grained PoS-tags and 280 fine-grained PoS   #
#     tag labels. Note that the chosen main categories (shown in table 4.1) are well in line with a universal   #
#     PoS tag set as proposed by (?) which has almost the same tags. The universal set has a particles tag for  #
#     function words that signify negation, mood or tense while CGN has an interjection tag to label words like #
#     'aha' and 'oke' that are typically used in spoken utterances.' (p. 18)                                    # 
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #                                                                                                         #
#   Other useful references:                                                                                    #
#                                                                                                               #
#      Aminian, M., Avontuur, T., Azar, Z., Balemans, I., Elshof, L., Newell, R., ... & van Zaanen,             #
#           M. (2012). Assigning part-of-speech to Dutch tweets. In Workshop 'NLP can u tag                     #
#           user-generated-content (pp. 9-14).                                                                  #
#                                                                                                               #
#       Oostdijk, N. H. J. (2001). Het Corpus Gesproken Nederlands. Lettergrepen. Nieuwsbrief van               #
#           de Faculteit der Letteren KUN.                                                                      #
#                                                                                                               #
#       Van Eynde, F. (2004). Part of speech tagging en lemmatisering van het Corpus Gesproken                  #
#           Nederlands. KU Leuven.                                                                              #
#                                                                                                               #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #

"
 	
	if(verbose){
		print.message %>% cat
	}
	 
	dictionary.majorpos <- data.table(
		majorpos = c(
			"ADJ", "BW", "LET", "LID","N","SPEC","TSW","TW","VG","VNW","VZ","WW"
		),
		description = c(
			"Adjectief (bijvoeglijk naamwoord)", 
			"Adverb (bijwoord)", 
			"Leesteken", 
			"Lidwoord", 
			"Zelfstandig naamwoord", 
			"Speciaal",  
			"TSW",
			"Telwoord",
			"Voegwoord", 
			"Voornaamwoord",
			"Voorzetsel",
			"Werkwoord"
		),
		stringsAsFactors = FALSE
	)
									  
 	dictionary.majorpos  %<>% format.data.frame(justify = "left") 
	return(dictionary.majorpos )  
}
	

	
describe.ner.types <- function(verbose=TRUE){

print.message <- "
# ------------------------------------------------------------------------------------------------------------- #
# Named entity recognition types                                                                                #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Website: http://languagemachines.github.io/frog/                                                            #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Excerpt from the FROG manual                                                                                # 
#                                                                                                               # 
#      'The Named Entity Recognizer (NER) detects names in the text and labels them as location (LOC),          #
#       person (PER), organization (ORG), product (PRO), event (EVE) or miscellaneous (MISC).                   #
#                                                                                                               #
#       Internally and in Frog's columned output, the tags use a so-called BIO paradigm where B stands          #
#       for the beginning of the name, I signifies Inside the name, and O outside the name.'                    #
#                                                                                                               #
#      (...)                                                                                                    #
#                                                                                                               #
#     'The Named Entity Recognizer (NER) is an MBT classifier (?) trained on the SoNar 1 million word           #
#       corpus labeled with manually verified NER labels. The annotation is flat and in case of nested names,   #
#       the longest name is annotated.                                                                          #
#                                                                                                               #
#       For example a phrase like 'het Gentse Stadsbestuur' is labeled as het[Gentsestadsbestuur]ORG.           #
#       'Gentse' also refers to a location but the overarching phrase is the name of an organization and        #
#       this label takes precedence. Dutch determiners are never included as part of the name. Details about    #
#       the annotation of the training data can be found in the Sonar NE annotation guidelines. '               #  
#                                                                                                               # 
# ------------------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #
# Table of recognized entities (n.b., this table can be saved to a variable by invoking the command:            #
#                                                                                                               # 
#    dt <- describe.ner.types()                                                                                 #
# ------------------------------------------------------------------------------------------------------------- #

" 
 
	if(verbose){
		print.message %>% cat
	} 
	
	ner.types <- data.table(
		nertype = c(
			"PER","ORG","LOC","PRO","EVE","MISC"
		),
		description = c(
			"person",
			"organization",
			"location",
			"product",
			"event",
			"miscellaneous"
		),
		stringsAsFactors = FALSE
	)  
	
	ner.types %<>% format.data.frame(justify = "left") 
	return(ner.types)
}	
	
describe.chunk.types <- function(verbose=TRUE){

print.message <- "
# ------------------------------------------------------------------------------------------------------------- #
# Phrase Chunker                                                                                                #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Website: http://languagemachines.github.io/frog/                                                            #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Excerpt from the FROG manual                                                                                #
#                                                                                                               # 
#      'The phrase chunker represents an intermediate step between part-of-speech tagging and full parsing      #
#       as it produces a non-recursive, non-overlapping flat structure of recognized phrases in the text and    #
#       classifies them with their grammatical function such as adverbial phrase (ADVP), verb phrase (VP) or    #
#       noun phrase (NP). The tag labels produced by the chunker use the same type of BIO-tags (Beginning-      #
#       Inside-Outside) as the named entity recognizer.' (p.18)                                                 #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #
# Table of chunk types (n.b., this table can be saved to a variable by invoking the command:                    #
#                                                                                                               # 
#    dt <- describe.chunk.types()                                                                               #
# ------------------------------------------------------------------------------------------------------------- #

" 
    if(verbose){
		print.message %>% cat
	}
	 
	chunk.types <- data.table(
		chunktype = c(
			"NP","VP","ADJP","PP","O","DETP", "SBAR", "CONJ", "ADVP", "VAST"
		),
		description = c(
			"Noun phrase",
			"Verb phrase",
			"Adjective phrase",
			"Prepositional phrase",
			"O",
			"DETP",
			"SBAR",
			"Conjunctive phrase",
			"Adverbial phrase",
			"VAST"
		),
		stringsAsFactors = FALSE	
	)  
	
	chunk.types %<>% format.data.frame(justify = "left") 
	return(chunk.types)
}		
	
describe.syndep.labels <- function(verbose=TRUE){

print.message <- "
# ------------------------------------------------------------------------------------------------------------- #
# Syntactic dependencies                                                                                        #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Website: http://languagemachines.github.io/frog/                                                            #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #
#                                                                                                               #
#   Excerpt from the FROG manual                                                                                #
#                                                                                                               #
#        'The Constraint-satisfaction inference-based dependency parser (CSI-DP) (?) predicts grammatical       #
#         relations between pairs of tokens. In each token pair relation, one token is the head and the other   #
#        is the dependent. Together these relations represent the syntactic tree of the sentence. One token,    #
#        usually the main verb in he sentence, forms the root of the tree and the other tokens depend on the    #
#        root in a direct or indirect relation. CSI-DP is trained on    the Alpino treebank (?) for Dutch and   #
#        uses the Alpino syntactic labels listed in appendix A.                                                 #
#                                                                                                               #
#        In the plain text output of Frog (example 4.1) the dependency information is presented in the          #
#        last two columns. The one-but-last column shows number of the token number of the head word of the     #
#        dependency relation and the last column shows the grammatical relation type.                           #
#                                                                                                               #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #  

# ------------------------------------------------------------------------------------------------------------- #
# Table of syntactic dependency types (n.b., this table can be saved to a variable by invoking the command:     #
#                                                                                                               # 
#    dt <- describe.syndep.labels()                                                                             #
#                                                                                                               #
# ------------------------------------------------------------------------------------------------------------- #

" 
	if(verbose){
		print.message %>% cat
	}
	 
	syndep.types <- data.table(
		syndeplabel = c(
			 "APP","BODY","CMP","CNJ","CRD","DET","DLINK","DP","HD",
			 "HDF","LD","ME","MOD","MWP","NUCL","OBCOMP","OBJ1","OBJ2",
			 "PC","POBJ1","PREDC","PREDM","RHD","ROOT","SAT","SE","SU",
			 "SUP","SVP","TAG","VC","WHD"
		),
		description = c(
			"appositie, bijstelling",
			"romp (bij complementizer))",
			"complementizer",
			"lid van nevenschikking",
			"nevenschikker (als hoofd van conjunctie)",
			"determinator",
			"discourse-link",
			"discourse-part",
			"hoofd",
			"afsluitend element van circumpositie",
			"locatief of directioneel complement",
			"maat (duur, gewicht, ... ) complement",
			"bijwoordelijke bepaling",
			"deel van een multi-word-unit",
			"kernzin",
			"vergelijkingscomplement",
			"direct object, lijdend voorwerp",
			"secundair object (meewerkend, belanghebbend, ondervindend)",
			"voorzetselvoorwerp",
			"voorlopig direct object",
			"predicatief complement",
			"bepaling van gesteldheid 'tijdens de handeling'",
			"hoofd van een relatieve zin",
			"startpunt",
			"satelliet; aan- of uitloop",
			"verplicht reflexief object",
			"subject, onderwerp",
			"voorlopig subject",
			"scheidbaar deel van werkwoord",
			"aanhangsel, tussenvoegsel",
			"verbaal complement",
			"hoofd van een vraagzin"
		),
		stringsAsFactors = FALSE
	)
	
	syndep.types %<>% format.data.frame(justify = "left")  
	
	return(syndep.types) 
}
	
describe.rule.types <- function(verbose=T){

	



}
	
	