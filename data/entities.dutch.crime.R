 
entities.dutch.crime.property.diefstal <- function(){ 
  c("achterhouden","achteroverdrukken", "diefstal", "dief",
                   "afnemen","benemen","gappen","inpikken",
                "jatten","kapen","leegstelen","ontfutselen","ontnemen",
               "ontvreemden","pijlen","pikken", "plunderen","roven","schachten","snaaien","stropen","toeëigenen","verdonkeremanen","verdonkeren","verduisteren",
                "vervreemden","wegfutselen","weggraaien","wegkapen","wegnemen","wegpikken") %>% return
   
}  
entities.dutch.crime.property.fraude  <- function(){ 
    c("bedrog","bedriegen","malversatie","malverseren", "onregelmatigheden",
        "ontvreemding","ontvreemden", "verdonkeremaning","verdonkeremanen",
        "frauderen",  "frauduleus","verduisteren","verduistering",
      "zwendelen","zwendel")  %>% return

}
 
 
entities.dutch.crime.violent <- function(){
    
    c("slaan", "schoppen", "vermoorden", "mishandelen", "schieten","pistool", "neermaaien") %>% return
     
}

entities.dutch.crime.hacking <- function(){
     
	 c("hacken", "cracken", "kraken") %>% return
} 

entities.dutch.crime.property <- function(x){
    
    entities <- list()  
    
    entities[[1]] <- data.table(
        
        casefold        = 1,
        class           = "dutch.crime",  
        subclass        = "dutch.crime.diefstal", 
        tag.column.name = "is_diefstal",
        tag.value       = 1,
        tag.value.not   = 0,
        set.not         = 1,
        match.type      = "match.list", 
        where.token     = "lemma",
        where.list      = "entities.dutch.crime.property.diefstal"
        
    )   

    entities[[2]] <- data.table(
        
        casefold        = 1,
        class           = "dutch.crime", 
        subclass        = "dutch.crime.fraude", 
        tag.column.name = "is_fraude", 
        tag.value       = 1,
        tag.value.not   = 0,
        set.not         = 1,
        match.type      = "match.list", 
        where.token     = "lemma",
        where.list      = "entities.dutch.crime.property.fraude"
        
    )   
 
    entities <- entities %>% rbindlist(fill=T)
    
} 


#dtfrog.small[({sapply(c("stelen", "pik"), function(x){(lemma %like% x)*1.0 %>% return})} == 1) %>% which,  ]

#dtfrog.small[ lemma %like% "stelen",]


