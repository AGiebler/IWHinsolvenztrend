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


identify_glaeubigerausschuss <- function(text) {
    # Check if the text contains information about the Gläubigerausschuss
    pattern <- "vorläufige[n|r]* Gläubigerausschu.+eingesetzt"

    return(grepl(pattern, text, ignore.case = TRUE))
}


identify_vorl_insolvenzverwalter <- function(text) {
    # Check if the text contains information about the preliminary insolvency administrator
    pattern <- "vorläufig.+Insolvenzverwalter.+bestell"

    return(grepl(pattern, text, ignore.case = TRUE))
}


identify_sachwalter <- function(text) {
    # Check if the text contains information about the Sachwalter
    pattern <- "zu.+Sachwalter.+bestell"

    return(grepl(pattern, text, ignore.case = TRUE))
}


identify_eroeffnung <- function(text) {
    # Check if the text contains information about the opening of the insolvency proceedings
    pattern <- "Insolvenzverfahren.+eröffnet"

    return(grepl(pattern, text, ignore.case = TRUE))
}


identify_abweisung <- function(text) {
    # Check if the text contains information about the dismissal of the insolvency proceedings
    # there are diferent pattarns for dismissal
    # e.g. "Insolvenzverfahren.+abgewiesen" or "Insolvenzverfahren.+nicht eröffnet"
    # or "Insolvenzverfahren.+nicht eröffnet"

    pattern <- paste(
        "mangels.+masse.+((ab|zurück)gewiesen|ab.+lehn|eingestellt)",
        "antrag.+abgewiesen.+keine.+masse",
        "antrag.+mangels.+masse",
        sep = "|"
    ) 

    return(grepl(pattern, text, ignore.case = TRUE))
}