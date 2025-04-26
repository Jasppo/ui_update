# Load files function ================================================================================================================

# Environments to cache loaded objects
cache <- new.env(parent = emptyenv())

# Generic lazy loader function for data
lazy_load <- function(filepath, sheet = NULL, table_index = 1) {
  # Extract filename without extension
  filename <- tools::file_path_sans_ext(basename(filepath))
  
  # Check if data already exists in the environment
  if (!exists(filename, envir = cache)) {
    # print(paste0(filepath, ": Does not exist"))
    # Determine the file extension and convert to lowercase
    file_extension <- tolower(tools::file_ext(filepath))
    
    # Load the data based on file type
    switch(
      file_extension,
      "rds" = {
        cache[[filename]] <- readRDS(filepath)
      },
      "csv" = {
        cache[[filename]] <- read_csv(filepath)
      },
      "xlsx" = {
        cache[[filename]] <- read_excel(filepath, sheet = sheet)
      },
      "rdata" = {
        e <- new.env()
        load(filepath, envir = e)
        for (obj_name in ls(e)) {
          cache[[obj_name]] <- get(obj_name, envir = e)
        }
      },
      "shp" = {
        cache[[filename]] <- st_read(filepath, quiet = TRUE)
      },
      "docx" = {
        doc <- read_docx(filepath)
        cache[[filename]] <- docx_extract_tbl(doc, table_index)
      },
      stop("Unsupported file type")
    )
  } else {
    #  print(paste0(filepath, ": Exists"))
  }
  
  # Return the data from the cache
  cache[[filename]]
}

# Function to clear cache if needed
clear_cache <- function() {
  rm(list = ls(envir = cache), envir = cache)
}