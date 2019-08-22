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