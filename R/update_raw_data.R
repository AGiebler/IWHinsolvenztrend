#' @title Get Session ID
#' @description Extracts the 'jakarta.faces.ViewState' (session ID) from an HTML response.
#' @param res An HTTP response object (from `httr::GET` or `httr::POST`).
#' @return The 'jakarta.faces.ViewState' value.
get_session_id <- function(res) {
    # More robustly find the ViewState value by targeting its specific input field name
    session_id <- tryCatch({
        res %>%
            rvest::read_html() %>%
            rvest::html_node("input[name='jakarta.faces.ViewState']") %>%
            rvest::html_attr("value")
    }, error = function(e) {
        warning(paste("Could not extract session ID:", e$message))
        return(NULL) # Return NULL on error
    })
    
    return(session_id)
}


#' @title Load Existing Insolvency Data
#' @description Loads existing insolvency data from a Parquet dataset.
#' @param data_path The file path to the Parquet dataset directory.
#' @return A data frame containing the existing insolvency data.
load_existing_data <- function(data_path) {
    if (dir.exists(data_path)) {
        df_inso_crawl <- tryCatch({
            arrow::open_dataset(data_path) |>
                dplyr::collect() |>
                dplyr::mutate(datum = as.Date(datum))
        }, error = function(e) {
            stop(paste0("Fehler beim Laden des Datensatzes unter '", data_path, "': ", e$message))
        })
    } else {
        # This stop is already in place and serves as a good initial check.
        stop(paste0("Fehler: Kein bestehender Datensatz unter '", data_path, "' gefunden."))
    }
    return(df_inso_crawl)
}


#' @title Create Dates to Crawl
#' @description Generates a sequence of dates to crawl, starting from the last crawled date up to today.
#' @param df A data frame containing a 'datum' column (last crawled date).
#' @return A vector of dates to be crawled.
create_dates_to_crawl <- function(df) {
    # latest bankruptcy date in monthly dataset
    last_crawl <- max(df$datum)

    # create all dates between latest crawl and today
    dates_to_crawl <- seq(as.Date(last_crawl), as.Date(Sys.Date()), by = "days")
    return(dates_to_crawl)
}


#' @title Get Month List
#' @description Extracts unique month strings (YYYY-MM) from a vector of dates.
#' @param dates A vector of date objects or date strings.
#' @return A vector of unique month strings.
get_month_list <- function(dates) {
    # extract affected months from dates_to_crawl
    month_list <- unique(stringr::str_sub(dates, 1, 7))
    return(month_list)
}


#' @title Split Date
#' @description Splits a date object or string into year, month, and day components as strings.
#' @param date A date object or a date string (e.g., "YYYY-MM-DD").
#' @return A list of year, month, and day components as strings.
split_date <- function(date) {
    # Using format() is more robust and idiomatic than string splitting,
    # especially since the input is a Date object from the crawl sequence.
    # as.Date() ensures it works for both Date objects and character strings.
    date_obj <- as.Date(date)
    return(list(year = format(date_obj, "%Y"), month = format(date_obj, "%m"), day = format(date_obj, "%d")))
}


#' @title Initiate Session
#' @description Initiates a new web session by making a GET request to the search URL and extracting the session ID.
#' @param url The URL of the search page.
#' @return The session ID obtained from the GET request.
initiate_session <- function(url) {
    # call page to initiate a session for every date and item
    tryCatch({
        primer <- httr::GET(url)
        if (httr::http_error(primer)) {
            stop(paste("HTTP error during session initiation:", httr::http_status(primer)$reason))
        }
        session_id <- get_session_id(primer)
        return(session_id)
    }, error = function(e) {
        stop(paste("Failed to initiate session for URL", url, ":", e$message))
    })
}


#' @title Build Search Parameters
#' @description Constructs the form parameters required for the initial search POST request.
#' @param year The year for the search date.
#' @param month The month for the search date.
#' @param day The day for the search date.
#' @param item The 'gegenstand' item code (e.g., "0", "1", "2").
#' @param session_id The current session ID (jakarta.faces.ViewState).
#' @return A named list of search parameters for the POST request.
build_search_param <- function(year, month, day, item, session_id) {
    search_param <- list(
            "frm_suche" = "frm_suche",
            "frm_suche:lsom_bundesland:lsom" = "--+Alle+Bundesländer+--",
            "frm_suche:ldi_datumVon:datumHtml5" = paste0(year, "-", month, "-", day),
            "frm_suche:ldi_datumBis:datumHtml5" = paste0(year, "-", month, "-", day),
            "frm_suche:lsom_wildcard:lsom" = "0",
            "frm_suche:litx_firmaNachName:text" = "",
            "frm_suche:litx_vorname:text" = "",
            "frm_suche:litx_sitzWohnsitz:text" = "",
            "frm_suche:iaz_aktenzeichen:itx_abteilung" = "",
            "frm_suche:iaz_aktenzeichen:som_registerzeichen" = "--",
            "frm_suche:iaz_aktenzeichen:itx_lfdNr" = "",
            "frm_suche:iaz_aktenzeichen:som_jahr" = "--",
            "frm_suche:lsom_gegenstand:lsom" = item,
            "frm_suche:ireg_registereintrag:som_registergericht" = "--",
            "frm_suche:ireg_registereintrag:som_registerart" = "",
            "frm_suche:ireg_registereintrag:itx_registernummer" = "",
            "frm_suche:ireg_registereintrag:ihd_validator" = "true",
            "frm_suche:cbt_suchen" = "Suchen",
            "jakarta.faces.ViewState" = session_id)

    return(search_param)
}


#' @title Build AJAX Parameters for Text
#' @description Constructs the AJAX request parameters to fetch the detailed text for a specific insolvency entry.
#' @param session_id_results The session ID from the results page.
#' @param idx The 0-based index of the row in the results table.
#' @param form_detail_j_idt The dynamic 'j_idt' identifier for the detail form.
#' @return A list of AJAX request parameters.
build_ajax_params_for_text <- function(session_id_results, idx, form_detail_j_idt) {
    # This parameter name is dynamic and its value is the same as its name.
    # It identifies which row's details are being requested via AJAX.
    # e.g., "tbl_ergebnis:0:frm_detail" = "tbl_ergebnis:0:frm_detail"
    dynamic_param_name <- paste0("tbl_ergebnis:", idx, ":frm_detail")
    dynamic_param <- stats::setNames(list(dynamic_param_name), dynamic_param_name)

    # The rest of the parameters for the AJAX request
    static_params <- list(
        "jakarta.faces.ViewState" = session_id_results,
        "jakarta.faces.source" = form_detail_j_idt,
        "jakarta.faces.partial.event" = "click",
        "jakarta.faces.partial.execute" = form_detail_j_idt,
        "jakarta.faces.partial.render" = "msgs frm_text:ihd_text",
        "jakarta.faces.behavior.event" = "click",
        "jakarta.faces.partial.ajax" = "true"
    )

    return(c(dynamic_param, static_params))
}



#' @title Fetch Row Text
#' @description Fetches the detailed text content for a specific insolvency entry using an AJAX request.
#' @param row_index The 0-based index of the row in the results table.
#' @param initial_post_response The HTTP response object from the initial search POST request.
#' @param session_id_results The session ID obtained from the results page.
#' @return The text content of the insolvency entry, or NA if not found.
fetch_row_text <- function(row_index, initial_post_response, session_id_results, result_url) {
    # 1. Extract the specific j_idt for this row from the initial page content
    str_tbl_ergebnis <- paste0("tbl_ergebnis:", row_index, ":frm_detail")
    page_content <- httr::content(initial_post_response, as = "text", encoding = "UTF-8")
    
    form_detail_j_idt <- stringr::str_extract(page_content, pattern = paste0(str_tbl_ergebnis, ":j_idt[0-9]{3,4}"))

    if (is.na(form_detail_j_idt)) {
        warning(paste("Could not find 'form_detail_j_idt' for row index:", row_index))
        return(NA_character_)
    }

    # 2. Build the parameters for the AJAX request
    ajax_params <- build_ajax_params_for_text(session_id_results, row_index, form_detail_j_idt)

    # 3. Make the AJAX request
    Sys.sleep(1) # Be polite to the server and avoid rate limiting
    
    # Add error handling for the POST request itself
    ajax_response <- tryCatch({
    ajax_response <- httr::POST(url = result_url, body = ajax_params, encode = "form")

    # 4. Parse the XML response and extract the text value from the hidden input
    tryCatch({
        ajax_response %>%
            httr::content("text", encoding = "UTF-8") %>%
            xml2::read_xml() %>%
            xml2::xml_find_first("//update[@id='frm_text:ihd_text']") %>%
            xml2::xml_text() %>%
            rvest::read_html() %>%
            rvest::html_node("input") %>%
            rvest::html_attr("value")
    }, error = function(e) {
        warning(paste("Failed to fetch or parse text for row index:", row_index, "Error:", e$message))
        NA_character_ # Return NA on parsing failure
    })
    }, error = function(e) {
        warning(paste("HTTP POST request failed for row index:", row_index, "Error:", e$message))
        return(NA_character_) # Return NA if the POST request itself fails
    })
}

#' @title Scrape Insolvency Table
#' @description Scrapes the insolvency table for a given date and item, including fetching detailed text for each entry.
#' @param url The URL for the initial search POST request.
#' @param date The date string being crawled (for logging).
#' @param item The 'gegenstand' item code.
#' @param search_param The search parameters for the initial POST request.
#' @return A data frame containing the scraped data.
scrape_inso_table <- function(url, date, item, search_param, url_result) {
    # Initial POST to get the results table
    p <- tryCatch({
        res <- httr::POST(url = url, body = search_param, encode = 'form')
        if (httr::http_error(res)) {
            stop(paste("HTTP error during initial search POST:", httr::http_status(res)$reason))
        }
        res
    }, error = function(e) {
        warning(paste("Failed initial search POST for date", date, "item", item, ":", e$message))
        return(data.frame()) # Return empty dataframe on error
    })
    if (is.data.frame(p) && nrow(p) == 0) return(data.frame()) # If POST failed and returned empty df

    # Check if there are any hits
    page_content <- httr::content(p, as = "text", encoding = "UTF-8")
    if (stringr::str_detect(page_content, "Keine Treffer")) {
        return(data.frame())
    }
    
    results_table <- tryCatch({
        page_content %>% rvest::read_html() %>% rvest::html_node(xpath = "//*[@id='tbl_ergebnis']") %>% rvest::html_table(fill = TRUE)
    }, error = function(e) {
        warning(paste("Failed to parse results table for date", date, "item", item, ":", e$message))
        return(data.frame()) # Return empty dataframe on parsing error
    })
    if (is.null(results_table) || nrow(results_table) == 0) return(data.frame())
    
    colnames(results_table) <- c("datum", "inso_kennzeichen", "gericht", "name", "sitz", "register", "text")
    results_table$gegenstand <- as.integer(item)

    # Extract the session ID for the results page once
    session_id_results <- get_session_id(p)

    # Loop through each row to fetch the detailed text using the new helper function
    detailed_texts <- vapply(1:nrow(results_table), function(k) {
        cat(date, ": item:", item, ":", k, "of", nrow(results_table), "\n")
        idx <- k - 1 # Website index starts at 0
        fetch_row_text(idx, p, session_id_results, url_result)
    }, FUN.VALUE = character(1)) # Specify return type for vapply for robustness

    results_table$text <- detailed_texts
    results_table$datum <- as.Date(results_table$datum, tryFormats = c("%d.%m.%Y"))
    results_table$month <- stringr::str_sub(results_table$datum, 1, 7)

    return(results_table)
}


#' @title Clean Results
#' @description Cleans and standardizes the scraped insolvency data, including text manipulation and duplicate removal.
#' @param df A data frame of scraped insolvency results.
#' @return A cleaned and standardized data frame.
clean_results <- function(df) {
    # Use dplyr::mutate with across for more efficient and readable cleaning.
    df <- df %>%
        dplyr::mutate(
            # Squish whitespace and convert to uppercase for specified columns
            dplyr::across(c(inso_kennzeichen, name, sitz), ~stringr::str_to_upper(stringr::str_squish(.))),
            # Squish whitespace for other text columns
            dplyr::across(c(gericht, register, text), stringr::str_squish),
            # Remove quotes and backslashes from specified columns in one go
            dplyr::across(c(name, sitz, text), ~stringr::str_remove_all(., pattern = '[\\"]'))
        )

    # Convert gegenstand to integer after all data is collected
    df$gegenstand <- as.integer(df$gegenstand)

    # Remove duplicates based on key fields
    df <- df |>
        dplyr::distinct(datum, inso_kennzeichen, gericht, name, sitz, register, gegenstand, .keep_all = TRUE)

    return(df)
}


#' @title Check for Empty Text
#' @description Checks if any scraped insolvency entries have empty text fields and stops execution if found.
#' @param df A data frame of cleaned insolvency results.
check_for_empty_text <- function(df) {
    empty_text <- df |>
        dplyr::filter(text == "") |>
        nrow()
    
    if (empty_text > 0) {
        print(paste0(empty_text, " bankruptcies with empty text."))
        stop("Please check the website for changes.")

    }
}


#' @title Update Raw Insolvency Data
#' @description Orchestrates the entire data crawling process: loads existing data, determines dates to crawl,
#' performs scraping, cleans data, and saves updated results.
#' @param data_path The file path to the Parquet dataset directory.
#' @export
#' @importFrom dplyr collect mutate filter distinct bind_rows across
#' @importFrom stringr str_sub str_extract str_detect str_to_upper str_squish str_remove_all
#' @importFrom rvest read_html html_node html_attr html_table
#' @importFrom httr GET POST http_error http_status content
#' @importFrom arrow open_dataset write_dataset
#' @importFrom xml2 read_xml xml_find_first xml_text
#' @importFrom stats setNames
update_raw_data <- function(data_path = "data-raw") {
    # gegenstand: 0 - Sicherungsmassnahmen, 1 - Abweisungen, 2 - Eröffnungen
    item <- c("0", "1", "2")
    url <- "https://neu.insolvenzbekanntmachungen.de/ap/suche.jsf"
    url_result <- "https://neu.insolvenzbekanntmachungen.de/ap/ergebnis.jsf"

    df_inso_crawl <- load_existing_data(data_path)
    dates_to_crawl <- create_dates_to_crawl(df_inso_crawl)

    if (length(dates_to_crawl) <= 1) {
        print("No new dates to crawl. Data is up to date.")
        return(invisible(NULL))
    }

    # Create a data frame of all combinations of dates and items to crawl
    crawl_plan <- expand.grid(date = dates_to_crawl, item = item, stringsAsFactors = FALSE)

    # Use a functional approach (lapply) to collect results in a list.
    # This avoids the highly inefficient rbind() in a loop.
    results_list <- lapply(1:nrow(crawl_plan), function(i) {
        current_date <- crawl_plan$date[i]
        current_item <- crawl_plan$item[i]
        
        # Wrap each iteration in a tryCatch to allow the loop to continue even if one fails
        tryCatch({
            crawl_date_parts <- split_date(current_date)
            cat(as.character(current_date), ", item:", current_item, "\n")

            session_id <- initiate_session(url)
            if (is.null(session_id)) { # Check if session_id was successfully obtained
                stop("Session ID could not be obtained.")
            }
            search_param <- build_search_param(year = crawl_date_parts$year,
                                               month = crawl_date_parts$month,
                                               day = crawl_date_parts$day,
                                               item = current_item,
                                               session_id = session_id)

            scrape_inso_table(url, as.character(current_date), current_item, search_param, url_result)
        }, error = function(e) {
            warning(paste("Error crawling date", as.character(current_date), "item", current_item, ":", e$message))
            return(data.frame()) # Return an empty data frame for failed iterations
        })
    })

    # Combine all results from the list into a single data frame
    new_data <- dplyr::bind_rows(results_list) # bind_rows handles empty data frames gracefully

    # Combine with existing data, clean, and remove duplicates
    month_list <- get_month_list(dates_to_crawl)
    df_inso_crawl <- df_inso_crawl %>% dplyr::filter(month %in% month_list) %>% dplyr::bind_rows(new_data) %>% clean_results()

    check_for_empty_text(df_inso_crawl)
    arrow::write_dataset(df_inso_crawl, data_path, partitioning = "month", format = "parquet")
}
