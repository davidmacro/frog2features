 
entities.dutch.encryption <- function(){
    
    require(data.table);    require(magrittr);
    
    entities <- list() 
    
    entities[[1]] <- list(where.token     = "lemma",
                          class           = "encryption", 
                          subclass        = "encryption_main", 
                          where.regex     = "versleut", 
                          match.type      = "match.regular.expression",
                          tag.column.name = "is_encryption",
                          casefold        = 1 ) 
    
    entities[[2]] <- list(where.token     = "lemma", 
                          class           = "encryption", 
                          subclass        = "encryption_main", 
                          where.regex     = "ncrypt", 
                          match.type      = "match.regular.expression",
                          casefold        = 1,
                          tag.column.name = "is_encryption" )
    
    entities[[3]] <- list(where.token = "lemma", 
                          class        = "encryption", 
                          subclass     = "encryption_main", 
                          where.regex  = "crypto", 
                          match.type   = "match.regular.expression",
                          tag.column.name = "is_encryption",
                          casefold     =  1 )  
    
    entities[[4]] <- list(where.token = "lemma", 
                          class        = "encryption", 
                          subclass     = "encryption_main", 
                          where.regex  = "AES", 
                          match.type   = "match.regular.expression",
                          tag.column.name = "is_encryption",
                          casefold     =  0 ) 
    
    entities[[5]] <- list(where.token = "lemma", 
                          class        = "encryption", 
                          subclass     = "encryption_main", 
                          where.regex  = "MD5", 
                          match.type   = "match.regular.expression",
                          tag.column.name = "is_encryption",
                          casefold     =  0)  
     
    entities[[6]] <- list(where.token = "lemma", 
                          class        = "encryption", 
                          subclass     = "encryption_main", 
                          where.regex  = "SHA1", 
                          match.type   = "match.regular.expression",
                          tag.column.name = "is_encryption",
                          casefold     =  0) 
      
    entities[[7]] <- list(where.token     = "lemma", 
                          class           = "encryption", 
                          subclass        = "encryption_main", 
                          where.regex     = "locker", 
                          match.type      = "match.regular.expression",
                          tag.column.name = "is_encryption",
                          casefold     =  1)  
    
    entities[[8]] <- list(where.token     = "lemma", 
                          class           = "encryption", 
                          subclass        = "encryption_main", 
                          where.regex     = "vergrendel", 
                          match.type      = "match.regular.expression",
                          tag.column.name = "is_encryption",
                          casefold     =  1) 
      
    return(data.table:::rbindlist(entities, fill=T))
} 

