#' RM2C2dev
#' @name generate_codebooks_from_dir
#' @export
#' @param dir class: string
#' @param codebook_type class: string
#' @import tidyverse
#' @import readr
#' @examples
#' generate_codebooks_from_dir('data/')

generate_codebooks_from_dir <- function(dir, codebook_type="simple", write_file = T) {
  
  # generate output timestamp ----
  st=format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  # possible codebook types ----
  possible_codebook_types = c("full", "simple")
  
  # check if valid cookbook type ----
  if(codebook_type %in% possible_codebook_types) {
    valid_codebook_type = T
    
    # create codebooks directory
    if(!dir.exists("codebooks")) {
      dir.create("codebooks")
    }
    
    print("Current directory...")
    print(getwd())

  } else {
    stop("Please enter a valid codebook type (i.e., `full` , `simple, `both`")
  }
  
  # proceed if valid codebook type -----
  if(valid_codebook_type) {
    
    # list files in data dir
    data_files = list.files(dir, full.names = T)
    
    # load all files into list
    all_dfs = lapply(data_files, read_delim, "|", escape_double = FALSE, trim_ws = TRUE)
    
    # for each file
    file_count = 1
    all_codebooks = tibble()
    for(df in all_dfs){
      # get pack name
      pack_name = data_files[file_count]
      pack_name_clean = gsub("/", "", gsub(".txt","", gsub(dir, "", pack_name)))
      
      # init blank codebook for pack
      pack_codebook <- tibble()
      
      if(codebook_type == "simple") {
        # for each column name
        col_index = 1
        for (i in names(df)) {
          # get single row
          sample_data_for_col = df %>% 
            select(!!enquo(i)) %>% 
            filter(!!enquo(i) != "." & !is.na(!!enquo(i))) %>% 
            sample_n(size=1)
          
          sample_data_str =  sample_data_for_col[[1]][1]
          
          # create column codebook
          et <- tibble(pack_id = pack_name_clean,
                       column_index = col_index,
                       column = i,  
                       column_type = class(i), 
                       column_example_data = toString(sample_data_str),
                       codebook_generation_timestamp = Sys.time())
          
          # merge with pack codebook
          pack_codebook <- bind_rows(pack_codebook, et)
          col_index = col_index + 1
        }
        # append to full codebook table
        all_codebooks = bind_rows(all_codebooks, pack_codebook)
        
      } else {
        # create full `dataMaid` codebook
        dataMaid::makeCodebook(df, file=paste0("codebooks/", pack_name_clean, ".Rmd"))
      }
    
      # increment pack counter
      file_count = file_count + 1
    }
  }
  if(codebook_type == "simple") {
    # export each pack codebook
    codebooks_fn = paste0("codebooks/all_codebooks-",pack_name_clean,"-",st,".csv")
    
    # if requested file write, save csv
    if(write_file) {
      write_csv(all_codebooks, codebooks_fn)
    }
    
    # return tibble of packs
    return(all_codebooks)
  } else {
    return("Codebooks created with the dataMaid package.")
  }

}