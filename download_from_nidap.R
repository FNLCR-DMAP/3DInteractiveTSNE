downloa_dataset_from_nidap -> function(dataset_rid, token, branch) {

    list_files_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",dataset_rid,"/files?branchId=", branch)
    print(paste("making request to ", list_files_url))

    response <- GET(list_files_url, httr::add_headers(Authorization = paste("Bearer", auth_token)))
    data_content <- content(response, as="text")
    parsed_json <- fromJSON(data_content)
    
    files <- parsed_json$data$path
    files <- files[!file_ext(files) %in% c("log", "")] #filter out log and spark success files
    if (length(files) == 0 ){
        stop("Error, zero files found in dataset")
    }
    print("reading through files")
    df = data.frame()
    for (file in files) {
      print(paste("getting data from", file))
      file <- url_encode(file)
      get_file_content_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",dataset_rid,"/files/",file,"/content?branchId=", branch)
      response2 <- GET(get_file_content_url, httr::add_headers(Authorization = paste("Bearer", auth_token)))
      
      if (file_ext(file) == "csv") {
        raw <- content(response2, as="text")
        dataset <- read.csv(text = raw)
        dataset <- data.frame(dataset)
        df <- rbind(df, dataset)
      } else if (file_ext(file) == "parquet") {
        print("reading parquet file")
        print(file)
        raw = content(response2, as="raw")
        dataset = read_parquet(raw)
        dataset = data.frame(dataset)
        
        dataset$name <- file
        df <- rbind(df, dataset)
      } else {
        dataset = generate_random_sample_data(100)
        dataset$name <- "else"
        df <- rbind(df, dataset)
      }
    }
    # df = df %>% filter(!is.na(pk))
    print("successfully read in all data")
    print(head(df, 5))
    return(df)
}