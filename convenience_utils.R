### File handling utilities

read_csv_folder <- function(dirpath){
  files <- list.files(dirpath)
  csv_files <- files[grepl('.csv', files)]
  print('All files found:')
  print(files)
  print('CSV files found:')
  print(csv_files)
  print('loading...')
  out <- vector(mode = 'list', length = length(csv_files))
  for(i in seq(1, length(csv_files))){
    filepath = paste0(dirpath, csv_files[i])
    paste0(filepath)
    out[[i]] <- read_csv(filepath)
  }
  return(out)
}


#return the top n columns in data.frame form so all columns print
peekdf <- function(df, n = 5){
  return(head(df, n) %>%
           as.data.frame)
}


#reorder a factor based on other columns
refactor_col <- function(df, factor_col, sort_col, ...){
  args <- list(...)
  print(args)
  print(sort_col)
  cols_needed <- c(as.character(sort_col), unlist(args))
  print(cols_needed)
  print('hi')
  
  newfac <- factor(df$factor_col, levels = df$sort_col)
  
  return(newfac)
  
}



### Wrap text using <br> tags
wrap_text <- function(longstring, linelength){
  #print(longstring)
  allwords <- str_split(longstring, ' ') %>% unlist
  wordcount <- length(allwords)
  #print('got here')
  #make final and tmp output strings
  output <- ''
  tmpline <- ''
  wordnum = 1
  #keep running until we're out of words from original
  while(wordnum <= length(allwords)){
    #print(c('wordnum', wordnum, 'length allwords', length(allwords)))
    #if next word will exceed line length, transfer tmp to body before proceeding
    if(nchar(tmpline) + nchar(allwords[wordnum]) >= linelength){
      #add break character, add to output, and clear tmp
      tmpline <- paste0(tmpline, '<br>')
      output <- paste0(output, tmpline)
      tmpline <- ''
    }
    #then add word to tmpline and interate
    #if tmpline has just been reset, just use word directly to avoid adding space
    tmpline <- ifelse(nchar(tmpline) == 0, allwords[wordnum], paste(tmpline, allwords[wordnum]))
    wordnum <- wordnum + 1
    #add words to line unless it's longer than to 
    #print(output)
  }
  output <- paste0(output, tmpline)  
  #cat(paste(output, '\n\n'))
  return(output)
}
#wrap_text(longstring, 20)

