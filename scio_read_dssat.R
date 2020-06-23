scio.read.dssat = function(filepath) {
  
  
  # returns string w/o leading whitespace
  xtrim.leading <- function (x)  sub("^\\s+", "", x)
  
  # returns string w/o trailing whitespace
  xtrim.trailing <- function (x) sub("\\s+$", "", x)
  
  # returns string w/o leading or trailing whitespace
  xtrim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
  con = file(filepath, "r")
  
  #================================= INIT MAIN VARIABLES
  
  ALL_VARIABLES_ARE_DOUBLE = TRUE
  #---------------------------       
  dssat_fileNames <- character()
  dssat_sections  <- character()
  dssat_fields    <- list()
  dssat_tables    <- list()
  #---------------------------     
  fields_name     <- character()
  fields_size     <- double()
  fields_text     <- logical()
  fields_decimals <- double()
  #---------------------------     
  tix <- 0
  is_First_Line_After_Variables = FALSE
  is_Data_Line = FALSE
  
  #================================= READ File line by line
  
  while ( TRUE ) {
    file_line = readLines(con, n = 1)
    
    if ( length(file_line) == 0 ) { break }
    
    #------------ CHECK first CHAR
    s_char = substr(file_line, 1, 1)
    
    #------------ IF first CHAR != *
    if(s_char != "*"){
      
      if(is_First_Line_After_Variables == TRUE){
        space_counter = 0
        other_counterL = 0
        other_counterR = 0
        new_variable = TRUE
        in_decimals = FALSE
        
        #---------------------------     
        
        for(i in 1:nchar(file_line)){
          cur_char = substr(file_line,i, i)
          
          if(new_variable == TRUE){
            
            if(cur_char == " ") { space_counter = space_counter + 1}
            else{
              other_counterL = other_counterL + 1;
              new_variable = FALSE
            }
            
          }else{
            
            
            if(cur_char !=" "){
              
              if(cur_char == '.'){ in_decimals = TRUE }
              
              
              if(in_decimals == TRUE){
                other_counterR = other_counterR + 1
              }else{
                other_counterL = other_counterL + 1;
              }
              
            }
            
            else{
              size = space_counter + other_counterL + other_counterR
              fields_size = c(fields_size,size)
              
              if(other_counterR > 1){ decimals = other_counterR - 1 
              }else{ decimals = 0 }
              
              fields_decimals = c(fields_decimals,decimals)
              
              artifact = substr(file_line,i-other_counterL-other_counterR,i-1)
              
              is_neg = 1
              if(substr(artifact,1,1) == '-'){is_neg = 2}
              
              is_text = FALSE
              partL = substr(artifact,is_neg,other_counterL)
              
              if(grepl("\\D", partL)){is_text = TRUE}
              
              if(decimals > 0){
                partR = substr(artifact,nchar(artifact)-decimals+1,nchar(artifact))
                
                if(grepl("\\D", partR)){is_text = TRUE}
              }
              
              fields_text = c(fields_text,is_text)
              if (is_text == TRUE) { ALL_VARIABLES_ARE_DOUBLE = FALSE }
              
              #----------------------
              space_counter = 1
              other_counterL = 0
              other_counterR = 0
              in_decimals = FALSE
              new_variable = TRUE
            }
          }
        }
        
        #---------------------------     
        
        size = space_counter + other_counterL + other_counterR
        fields_size = c(fields_size,size)
        
        if(other_counterR > 1){ decimals = other_counterR - 1 
        }else{ decimals = 0 }
        
        fields_decimals = c(fields_decimals,decimals)
        
        artifact = substr(file_line,nchar(file_line)-other_counterL-other_counterR+1,nchar(file_line))
        
        is_neg = 1
        if(substr(artifact,1,1) == '-'){is_neg = 2}
        
        is_text = FALSE
        
        partL = substr(artifact,is_neg,other_counterL)
        if(grepl("\\D", partL)){is_text = TRUE}
        
        if(decimals > 0){
          partR = substr(artifact,nchar(artifact)-decimals+1,nchar(artifact))
          
          if(grepl("\\D", partR)){is_text = TRUE}
        }
        
        fields_text = c(fields_text,is_text)        
        if (is_text == TRUE) { ALL_VARIABLES_ARE_DOUBLE = FALSE }
        
        #---------------------------     
        
        dssat_data = matrix(nrow=1,ncol=length(fields_name))
        
        #---------------------------     
        
        is_Data_Line = TRUE
      }
      
      #---------------------------     
      if(is_Data_Line == TRUE){
        
        if (nchar(file_line)>0) {
          
          cleaned_line <- xtrim.leading(file_line)
          
          
          data_list <- strsplit(cleaned_line, "\\s+")
          
          
          if (ALL_VARIABLES_ARE_DOUBLE == TRUE) {
            
            data <- as.double(unlist(data_list))
            
          }else{ data <- unlist(data_list) }
          
          
          if (is_First_Line_After_Variables == TRUE) {
            
            dssat_data[nrow(dssat_data),] <- data
            is_First_Line_After_Variables = FALSE 
            
          }else { dssat_data = rbind(dssat_data,data) }
          
          colnames(dssat_data) <- fields_name
          rownames(dssat_data) <- c()
          
        }
      }
      #------------ IF first CHAR == *  
    }else{  
      
      if (is_Data_Line == TRUE) {
        
        if (ALL_VARIABLES_ARE_DOUBLE == TRUE) {
          
          dssat_tables[[tix]] <- as.data.frame(dssat_data)
          
        }else{   
          
          xdssat_data <- data.frame(dssat_data, stringsAsFactors=FALSE)
          
          k <- sapply(fields_text, isFALSE)
          xdssat_data[,k] <- lapply(xdssat_data[,k], as.double)
          
          dssat_tables[[tix]] <- xdssat_data
        }  
        
        is_Data_Line = FALSE
      }
      
      #---------------------------     
      
      if(substr(file_line, 1, 4) == "*RUN"){
        
        if(tix>0){
          dssat_fields[[tix]] <- list(name = fields_name, size = fields_size, text = fields_text, decimals = fields_decimals)
        }
        
        #---------------------------     
        tix <- tix + 1  
        
        #---------------------------     
        dssat_fileNames[[tix]]<- filepath    
        dssat_sections[[tix]] <- file_line
      }
      
      if(substr(file_line, 1, 11) == "*EVALUATION"){
        
        if(tix>0){
          dssat_fields[[tix]] <- list(name = fields_name, size = fields_size, text = fields_text, decimals = fields_decimals)
        }
        
        #---------------------------     
        tix <- tix + 1  
        
        #---------------------------     
        dssat_fileNames[[tix]]<- filepath    
        dssat_sections[[tix]] <- xtrim.trailing(substr(file_line, 1,regexpr('DSSAT',file_line)-1))
      }
      
    }
    #------------ IF first CHAR == @
    if(s_char == "@"){
      #---------------------------     
      fields_name     <- character()
      fields_size     <- double()
      fields_text     <- logical()
      fields_decimals <- double()
      #---------------------------     
      cleaned_line <- substr(file_line,2,nchar(file_line))
      fields_name <- unlist(strsplit(cleaned_line, "\\s+"))
      #---------------------------     
      is_First_Line_After_Variables = TRUE
    }
    
  }
  
  #-------------------------------------------
  dssat_fields[[tix]] <- list(name = fields_name, size = fields_size, text = fields_text, decimals = fields_decimals)
  
  #-------------------------------------------
  
  if (ALL_VARIABLES_ARE_DOUBLE == TRUE) {
    
    dssat_tables[[tix]] <- as.data.frame(dssat_data)
    
  }else{   
    
    xdssat_data <- data.frame(dssat_data, stringsAsFactors=FALSE)
    
    k <- sapply(fields_text, isFALSE)
    xdssat_data[,k] <- lapply(xdssat_data[,k], as.double)
    
    dssat_tables[[tix]] <- xdssat_data
  }
  
  #-------------------------------------------
  close(con)
  
  return( new(Class = "Dasst", fileNames = dssat_fileNames, sections = dssat_sections, fields = dssat_fields, tables = dssat_tables))
}
