read_raw_data <- function(file_names) {
  
  # strip filenames down to subject ids
  rename_subj <- function(subj) {
    subj <- sub("-eye_data Samples_fixed.txt","",subj)
    subj <- paste0(strsplit(subj,"_")[[1]][1:2],collapse="_")
  }
  
  read_data <- function(x) {
    ##read the header from the file to paste back into the new file
    header <- scan(paste0(ET_DATA_DIR, x),
                         what = character(), nlines = MAX_HEADER_ROWS, 
                   sep="\n", quiet=TRUE)
    
    #find last header row
    header_rows <- sum(sapply(1:length(header), 
                              function(x) substring(header[x], 1, 1) == "#"))
    
    
    # Deal with idiosyncacies caused by different number of calib points
    result <- fread(paste0(ET_DATA_DIR, x), skip = header_rows,
                                    integer64 = "numeric") %>%
      mutate(subj = rename_subj(x))
    
    # Deal with Timestamps greater than R's max 32bit int
    if(max(result$Time) > .Machine$integer.max){
      result %<>% 
        mutate(Time = Time - min(Time) + 1,
               Time = as.integer(Time))
    }
    
    return(result)
  }
  
  average_eyes <- function(both_eyes) {
    eye_sum <- rowSums(both_eyes, na.rm = T)
    eye_present <- rowSums(both_eyes > 0, na.rm = T)
    
    mean_eye = eye_sum/eye_present
      
  }
  
  #read all files and concatenate
  all_raw_data <- bind_rows(lapply(file_names,read_data)) %>%
    mutate(Stimulus = tolower(Stimulus)) %>%
    filter(!Stimulus %in% EXCLUDED_STIMS)
  
  # Make each x/y estimate the average of left and right eyes
  xs <- average_eyes(all_raw_data[,c("L POR X [px]", "R POR X [px]")])
  ys <- average_eyes(all_raw_data[,c("L POR Y [px]", "R POR Y [px]")])
  
  all_raw_data$x = xs
  all_raw_data$y = ys
  
  # Drop points outside the screen area
  all_results <- all_raw_data %>%
    select(Time, Stimulus, subj, x, y) %>%
    mutate(x = ifelse(x == 0 | x >= X_MAX | y == 0 | y >= Y_MAX, NA, x),
           y = ifelse(x == 0 | x >= X_MAX | y == 0 | y >= Y_MAX, NA, y))
  
  return(all_results)
}
