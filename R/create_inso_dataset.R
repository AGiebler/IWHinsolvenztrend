#' @title Load Insolvency Data from Raw Files
#' @description This function loads the insolvency raw data from parquet files from specified raw data path
#' @param data_path The path to the directory containing the raw data files.
#' @return A data frame containing the loaded insolvency data.
load_raw_data <- function(data_path = "data-raw/inso") {
    # Load the raw data from the specified path
    if (!dir.exists(data_path)) {
        stop("Der angegebene Pfad existiert nicht: ", data_path)
    }
    
    # Load the dataset using arrow
    dataset <- arrow::open_dataset(data_path, format = "parquet")
    
    # Convert to a data frame
    df <- as.data.frame(dataset)
    
    return(df)
}

