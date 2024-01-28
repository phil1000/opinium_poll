.helper_loaded <- TRUE # to avoid loading this multiple times

# add CRAN libraries here - note tidyverse is the coding standard 
# used at DHSC
librarian::shelf(tidyverse, devtools, yaml, stringr, tidyr, 
                 lubridate, dplyr, httr, readxl, R6, futile.logger, 
                 jsonlite, openxlsx, janitor, purrr)

#' Converts the value field to a numeric and sets the is_na flag if it is a null value or a zero value but should not be.
#'
#' @param my_df A tidy dataframe containing the value field to be interrogated
#' @param non_zero_metrics a vector containing the names of fields that should not be zero
#'
#' @return the updated dataframe with value converted to a numeric and is_na flag set to True or False
convert_values <- function(my_df, non_zero_metrics){
  my_df <- my_df %>%
    mutate(value = as.numeric(value),
           is_na = is.na(value),
           value = if_else(is_na, 0, value),
           is_na = as.character(is_na)
    ) %>%
    mutate(is_na = case_when(value == 0 & metric %in% non_zero_metrics ~ "TRUE",
                             TRUE ~ is_na))
  
  return(my_df)
}

#' This function is for internal use only and is used by other functions to open and read a csv file
#'
#' @param input_file the csv file to be read
#' @param header_flag (optional) set to FALSE if the file doesn't contain a header row
#' @param my_col_names (optional) to specify own column names if there isn't a header row
#'
#' @return a vector containing a tibble with the contents of the file, and an error code and messages if errors encountered
get_csv_data <- function(input_file, 
                         header_flag=TRUE,
                         my_col_names = c("dummy")) {
  
  return_list <- list("ret_code" = -1, "data" = NA, "error_msg" = NA)
  
  if (!file.exists(input_file)) {
    return_list$error_msg <- sprintf("Couldn't open file: %s", input_file)
    return(return_list)
  }
  
  tryCatch(
    {
      if (header_flag) {
        csv_data <- read_csv(input_file,
                             show_col_types = FALSE)
      } else {
        csv_data <- read_csv(input_file,
                             col_names = my_col_names,
                             show_col_types = FALSE)
      }
      
      return_list$ret_code <- 0
      return_list$data <- csv_data
    },
    error = function(e){
      # note the use of <<- to assign to the parent namespace
      return_list$error_msg <<- sprintf("Error reading the file %s, error%s", 
                                        input_file, 
                                        e)
    }
  )
  
  return(return_list)
}


#' This function is for internal use only and is used by other functions to open and read an excel file
#'
#' @param input_file the excel file to be read
#' @param sheet_name the relevant sheet in the excel file
#' @param icb_range (optional) if a range of cells is to be read
#' @param skiprows (optional) number of rows to skip
#'
#' @return a vector containing a tibble with the contents of the file, and an error code and messages if errors encountered
get_excel_data <- function(input_file, 
                           sheet_name = NULL, 
                           icb_range = NULL,
                           skiprows = 0) {
  
  return_list <- list("ret_code" = -1, "data" = NA, "error_msg" = NA)
  
  if (!file.exists(input_file)) {
    return_list$error_msg <- sprintf("Couldn't open file: %s", input_file)
    return(return_list)
  }
  
  tryCatch(
    {
      excel_data <- read_excel(input_file, 
                               sheet=sheet_name,
                               range=icb_range,
                               skip = skiprows)
      
      return_list$ret_code <- 0
      return_list$data <- excel_data
    },
    error = function(e){
      return_list$error_msg <<- sprintf("Error reading the file %s, error%s", 
                                        input_file, 
                                        e)
    }
  )
  
  return(return_list)
}

get_icb_xref <- function(url) {
  # load the trust xref file as this is used both in the transformation 
  # plus also the cleaning of trust data
  
  # first download the file from the url
  trust_xref_list <- get_excel_from_url(url)
  
  if (trust_xref_list$ret_code != 0) {
    logger$error(trust_xref_list$error_msg)
    return(NULL)
  }
  
  # now read the downloaded file
  trust_xref_list <- helper_functions$get_excel_data(trust_xref_list$data,
                                                     sheet_name="ICB Mapping",
                                                     skiprows = 7)
  if (trust_xref_list$ret_code != 0) {
    logger$error(trust_xref_list$error_msg)
    return(NULL)
  }
  
  # clean the icb_name and region name fields
  trust_xref <- trust_xref_list$data %>%
    mutate(`ICB Name` = gsub("^NHS", "", `ICB Name`)) %>%
    mutate(`ICB Name` = gsub("Integrated Care Board", "ICB", `ICB Name`)) %>%
    mutate(`Region Name` = gsub("Commissioning Region", "", `Region Name`)) %>%
    rename(trust_code = Code,
           trust_name = Name,
           icb_code = `ICB Code`,
           icb_name = `ICB Name`,
           region_name = `Region Name`)
  
  return(trust_xref)
}

get_icb_populations <- function(url, icb_codes) {
  # first download the file from the url
  browser()
  icb_pop_list <- get_excel_from_url(url)
  
  if (icb_pop_list$ret_code != 0) {
    logger$error(icb_pop_list$error_msg)
    return(NULL)
  }
  
  # now read the downloaded file
  icb_pop_list <- helper_functions$get_excel_data(icb_pop_list$data,
                                                  sheet_name="ICB weighted population",
                                                  skiprows = 3)
  if (icb_pop_list$ret_code != 0) {
    logger$error(icb_pop_list$error_msg)
    return(NULL)
  }
  
  # clean the data
  icb_populations <- icb_pop_list$data %>%
    rename(icb_code = ICB22,
           weighted_population = `Overall ICB  weighted population`) %>%
    select(icb_code, weighted_population) 
  
  # Now join with the icb data to clear out the noise
  icb_populations <- icb_populations %>%
    filter(icb_code %in% icb_codes)
  
  return(icb_populations)
}

get_excel_from_url <- function(my_url) {
  
  return_list <- list("ret_code" = -1, "data" = NA, "error_msg" = NA)
  
  if (grepl("\\.xlsx$", my_url)) excel_file_type <- ".xlsx"
  else excel_file_type <- ".xls"
  
  tf <- tempfile(fileext = excel_file_type)
  
  tryCatch(
    {
      ret_code <- download.file(my_url, tf, mode="wb")
      return_list$ret_code <- 0
      return_list$data <- tf
      
      },
    error = function(e){
      return_list$error_msg <<- sprintf("Error reading the file %s, error%s", 
                                        my_url, 
                                        e)
    }
  )
  
  return(return_list)
  
}

#' This function is for internal use only and is used by other functions to download a file from a URL and unzip it
#'
#' @param my_url the url where the zip file is located
#'
#' @return an unzipped excel file
get_and_unzip_csv_file <- function(my_url) {
  
  return_list <- list("ret_code" = -1, "data" = NA, "error_msg" = NA)
  
  temp <- tempfile()
  
  tryCatch(
    {
      download.file(my_url, 
                    temp,
                    quiet=TRUE)
      
      con <- unzip(temp)
      
      return_list$data <- con[grepl(x = con, pattern = "csv")]
      
      return_list$ret_code <- 0
      
    },
    error = function(e){
      return_list$error_msg <<- sprintf("Error reading from url %s, error%s", 
                                        my_url, 
                                        e)
    }
  )
  
  if (return_list$ret_code != 0) {
    return(return_list)
  }
  
  if (!file.exists(return_list$data)) {
    return_list$ret_code <- -1
    return_list$error_msg <- sprintf("Couldn't open file: %s", return_list$data)
    return(return_list)
  }
  
  return(return_list)
}

#' Rank all metrics by value within groups, which have been grouped by current data and metric name
#'
#' @param df a tibble containing, as a minimum, an icb_code, current_date, metric, value and is_na flag
#' @param reverse_rank_metrics metrics that need to be ranked in reverse order
#'
#' @return the ranking value appended to the passed tibble
derive_ranked <- function(df, reverse_rank_metrics){
  
  # Ranking logic - a low number means high performing, and a high number
  # means low performing.NA values will always be given a ranking=-99.
  
  # This function will be passed a list of which metrics need to be ranked in ascending or 
  # descending order. 
  
  # give NA values a ranking = -99 and just set the ranking = value for others
  df <- df %>%
    mutate(ranking = case_when(is_na == TRUE ~ -99,
                               location_id == 'National' ~ -99,
                               TRUE ~ value))
  
  # I only want to rank within rows where value != -99 and so first we filter
  # and then we'll outer join back to the original df_ranked dataframe
  # Note that it would be better practice to use if_else rather than ifelse
  # because it type checks. Unfortunately this causes this code a problem because the
  # ranking is either a double, if valid, or an integer (-99) if null. I therefore
  # use the less sophisticated ifelse statement instead.
  df_ranked_subset <- df %>%
    filter(ranking != -99) %>%
    arrange(location_type, current_date, metric, value) %>%
    group_by(location_type, current_date, metric) %>%
    mutate(ranking = ifelse(metric %in% reverse_rank_metrics, rank(-value, 
                                                                   ties.method = "max"), 
                            rank(value, 
                                 ties.method = "min"))) %>%
    select(-c(value))
  
  merged_df <- df %>%
    left_join(df_ranked_subset,
              by = c("location_type",
                     "location_id",
                     "location_name",
                     "current_date",
                     "category",
                     "sub_category",
                     "metric",
                     "is_na"
              )) %>%
    rename(ranking = ranking.x) %>%
    mutate(ranking = ifelse(ranking==-99, ranking, ranking.y)) %>%
    select(-c(ranking.y))
  
  return(merged_df)
}

#' Get delta from the mean within each current date/metric combination, where is_na = FALSE and icb_code != National
#'
#' @param df a tibble containing, as a minimum, an icb_code, current_date, metric, value and is_na flag
#'
#' @return the delta_from_mean value appended to the passed tibble
get_mean_delta <- function(df){
  
  # Get delta from the mean for all metrics where is_na = FALSE and
  # icb_code != National
  df <- df %>%
    mutate(delta_from_mean = case_when(is_na == TRUE ~ -99,
                                       location_id == 'National' ~ -99,
                                       TRUE ~ value))
  
  # I only want to derive means for rows where value != -99 and so first we filter
  # and then we'll outer join back to the original dataframe
  df_means_subset <- df %>%
    filter(delta_from_mean != -99) %>%
    arrange(location_type, current_date, metric, value) %>%
    group_by(location_type, current_date, metric) %>%
    summarise(mean = mean(value, na.rm = TRUE))
  
  # Note below that while I want to exclude National values when calculating the
  # mean, I still want to calculate the delta from mean for National values. First
  # I need to replicate the nhs_stp mean values for national because otherwise
  # my join will fail because there aren't any rows with location_id = 'nhs_country'
  df_means_national <- df_means_subset %>%
    filter(location_type == 'nhs-stp') %>%
    mutate(location_type = 'nhs-country')
  
  # Now concatenate
  df_means_subset <- rbind(df_means_subset,
                           df_means_national)
  
  merged_df <- df %>%
    left_join(df_means_subset,
              by = c("location_type" = "location_type",
                     "current_date" = "current_date",
                     "metric" = "metric"
              )) %>%
    mutate(delta_from_mean = case_when(is.na(mean) == TRUE ~ -99,
                                       delta_from_mean==-99 & location_id != 'National' ~ -99,
                                       delta_from_mean==-99 & location_id == 'National' ~ value - mean,
                                       TRUE ~ value - mean)) %>%
    select(-c(mean))
  
  return(merged_df)
}

#' Derive the latest date within each category/metric combination
#'
#' @param df a tibble containing, as a minimum, an icb_code, category, sub_category, current_date, metric
#'
#' @return an is_latest_date flag set to True or False, and appended to the passed tibble
assign_latest_date <- function(df) {
  
  # Derive the latest date within each category/metric combination
  df_latest_subset <- df %>%
    group_by(location_type, category, metric) %>%
    slice(which.max(current_date)) %>%
    select(location_type, current_date, category, sub_category, metric) %>%
    mutate(is_latest_date = "TRUE")
  
  merged_df <- df %>%
    left_join(df_latest_subset,
              by = c("location_type",
                     "current_date",
                     "category",
                     "sub_category",
                     "metric"
              )) %>%
    mutate(is_latest_date = ifelse(is.na(is_latest_date), "FALSE",
                                   is_latest_date))
  
  return(merged_df)
  
}

#' Write key stats about the data being loaded to the log file for information purposes
#'
#' @param description the descriptive header to be given to these stats
#' @param df the dataframe containing the data
#'
#' @return nothing is returned
write_stats <- function(description, df) {
  
  row_count <- nrow(df)
  
  icb_count <- df %>% 
    filter(location_type == 'nhs-stp') %>%
    distinct(location_id) %>% 
    pull() %>%
    length()
  
  trust_count <- df %>% 
    filter(location_type == 'nhs-trust') %>%
    distinct(location_id) %>% 
    pull() %>%
    length()
  
  date_count <- df %>% 
    distinct(current_date) %>%
    pull() %>%
    length()
  
  null_count_icbs <- nrow(filter(df, location_type == 'nhs-stp' & is_na == "TRUE"))
  null_count_trusts <- nrow(filter(df, location_type == 'nhs-trust' & is_na == "TRUE"))
  
  logger$info("%s: tidy row count = %i",
              description,
              row_count)
  
  logger$info("%s: number of null values, ICB = %i, number of null values, Trusts = %i",
              description,
              null_count_icbs,
              null_count_trusts)
  
  logger$info("%s: number of distinct icbs = %i, number of distinct trusts = %i, number of distinct dates = %i",
              description,
              icb_count,
              trust_count,
              date_count)
  
  max_date_icbs <- df %>% 
    filter(location_type == 'nhs-stp') %>%
    summarise_at(c("current_date"), max, na.rm = TRUE) %>% 
    pull()
  
  max_date_trusts <- df %>% 
    filter(location_type == 'nhs-trust') %>%
    summarise_at(c("current_date"), max, na.rm = TRUE) %>% 
    pull()
  
  logger$info("%s: maximum date ICBs = %s, maximum date ICBs = %s",
              description,
              as.character(max_date_icbs),
              as.character(max_date_trusts))
  
}

#' Export the passed data to specified file
#'
#' @param df A tibble containing the data to be written
#' @param outfile the name of the output file
#' @param output_dir the directory in which the output file is to be written
#' @param description used to inform informational or error logging
#'
#' @return 0 if data was written to file, -1 if the write failed.
write_transformed_data <- function(df, 
                                   outfile, 
                                   output_dir,
                                   description) {
  
  # Export data to specified file - first need to get to the constituent parts
  # of the directory path
  
  my_str_comp <- unlist(str_split(output_dir, "/"))
  full_path <- append(my_str_comp, outfile)
  
  results_csv <- do.call(file.path, as.list(full_path))
  
  # Save the output files	- return an error if either write fails as something
  # has gone wrong
  
  ret_code <- tryCatch(
    {
      write_csv(df, results_csv)
      
      logger$info("Number of rows exported for %s data was %i", 
                  description, 
                  nrow(df))
      
      ret_code <- 0
    },
    error = function(e){
      error_msg <- sprintf("Couldn't write to file: %s", results_csv)
      logger$error(error_msg)
      message(error_msg, e)
      
      return(-1)
    }
  )
  
  return(ret_code)
}

#' Reads the trust_icb xref from a given public URL
#'
#' @param URL location of the file 
#'
#' @return a cross reference of trust codes to icb_codes
get_trust_icb_xref <- function(URL) {
  
  return_list <- list("ret_code" = -1, "data" = NA, "error_msg" = NA)
  
  # get the csv file from the passed URL
  unzipped_xref <- get_and_unzip_csv_file(URL)
  
  if (unzipped_xref$ret_code != 0) {
    return_list$error_msg = unzipped_xref$error_msg
    return(return_list)
  }
  
  xref_file <- unzipped_xref$data
  
  # now read the csv file - it doesn't have column names so give names
  # - most are dummy names because we don't need them
  my_columns <- c("location_id", "location_name", "chaff", "icb_code",
                  "_5", "_6", "_7", "_8", "_9", "_10", "_11", "_12", "_13",
                  "_14", "_15", "_16", "_17", "_18", "_19", "_20", "_21", "_22",
                  "_23", "_24", "_25", "_26", "_27")
  
  xref_data <- get_csv_data(xref_file, 
                            FALSE,
                            my_columns)
  
  if (xref_data$ret_code != 0) {
    return_list$error_msg = xref_data$error_msg
    return(return_list)
  }
  
  # extract the fields I need
  xref <- xref_data$data
  
  xref <- xref %>%
    select(c(location_id,
             location_name,
             icb_code))
  
  return_list$data <- xref
  return_list$ret_code <- 0
  
  return(return_list)
}

#' Parses the dataframe looking for NA values. Logs any found and return an error.
#'
#' @param df dataframe to be parsed
#' @param file_name the name of the file containing the data 
#' @param sheet_name (optional) the name of the excel sheet containing the data 
#'
#' @return either an error code or the original dataframe
check_for_nulls <- function(df,
                            file_name,
                            sheet_name=NULL) {
  
  return_code <- 0
  
  # check for null values
  if (sum(is.na(df)) > 0) {
    return_code <- -1
    
    if (is.null(sheet_name)) {
      error_msg <- sprintf("Null values found in file %s, this file will be discarded", file_name)
    } else {
      error_msg <- sprintf("Null values found in file %s, tab %s, this file will be discarded", 
                           file_name,
                           sheet_name)
    }
    
    logger$error(error_msg)
    
    null_columns <- df %>% 
      select_if(function(x) any(is.na(x))) %>%
      summarise_all(funs(sum(is.na(.))))
    
    logger$error(knitr::kable(null_columns, caption = "Null values", "simple"))
  }
  
  return(return_code)
}

#' Assigns ranks, delta from mean for every row in the dataframe and latest date by category/metric grouping
#'
#' @param df dataframe to be enriched
#' @param non_zero_metrics metrics that should be treated as NA if they are zero
#' @param reverse_rank_metrics metrics that should be ranking in reverse order
#'
#' @return an enriched dataframe
enrich_data <- function(df, 
                        non_zero_metrics,
                        reverse_rank_metrics) {
  
  # coerce all values to numeric and set NA values = 0 and flag them using the
  # is_na flag. Also, set is_na flag if a value is zero but shouldn't be
  df <- convert_values(df, non_zero_metrics)
  
  # adding rankings
  df_ranked <- derive_ranked(df,
                             reverse_rank_metrics)
  
  # add delta from mean
  df_mean_delta <- get_mean_delta(df_ranked)
  
  # assign latest dates
  df_latest_date <- assign_latest_date(df_mean_delta)
  
  # format nicely
  df_latest_date <- df_latest_date %>%
    select(location_type, 
           location_id,
           location_name,
           current_date,
           category,
           sub_category,
           metric,
           value,
           ranking,
           delta_from_mean,
           is_na,
           is_latest_date)
  
  return(df_latest_date)
}

#' check for null values in location_id, current_date and metric, and also check for and remove duplicates
#'
#' @param df dataframe
#' @param file_name the file containing this data
#' @param sheet_name (optional) the excel sheet containing this data
#'
#' @return either an error code or the original dataframe
source_data_error_checks <- function(df,
                                     file_name,
                                     sheet_name=NULL) {
  
  return_list <- list("ret_code" = -1, "data" = NA)
  
  temp_df <- df %>%
    select(location_id,
           current_date,
           metric)
  
  return_code <- check_for_nulls(temp_df, 
                                 file_name,
                                 sheet_name)
  
  if (return_code!=0)
    return(return_list)
  
  # Now check for duplicates - write an error message and remove them
  # but carry on processing
  
  df <- check_for_duplicates(df)
  
  return_list$data <- df
  return_list$ret_code <- 0
  
  return(return_list)
}

#' Check for duplicates and, if found, write an error to the error log and remove the rows
#' @param df the dataframe to be parsed
#'
#' @return the dataframe stripped of duplicates if there were any
check_for_duplicates <- function(df) {
  # 
  # 
  duplicate_rows <- df %>%
    group_by_all() %>%
    filter(n()>1) %>%
    ungroup()
  
  if (nrow(duplicate_rows) > 0) {
    
    logger$error(knitr::kable(duplicate_rows, caption = "Duplicate rows found", "simple"))
    
    df <- df %>% 
      distinct(across(everything()))
  }
  
  return(df)
}


#' Assign ICB code for trust and rank each trust within its ICB parent
#'
#' @param df A dataframe containing trust data
#' @param xref A cross reference between trusts and ICBs
#' @param non_zero_metrics metrics that should be treated as NA if they are zero
#' @param reverse_rank_metrics metrics that should be ranking in reverse order
#'
#' @return The dataframe with trust and rank_in_icb appended
enrich_trust_data <- function(df, 
                              xref,
                              non_zero_metrics,
                              reverse_rank_metrics) {
  
  df_enriched <- enrich_data(df, 
                             non_zero_metrics,
                             reverse_rank_metrics)
  
  # use the xref to get icb_code, but first remove location_name as this 
  # will get dealt with later
  xref <- xref %>%
    select(-location_name)
  
  df_merged <- df_enriched %>%
    inner_join(xref,
               by = c("location_id" = "location_id")) %>%
    filter(is_latest_date == "TRUE")
  
  ## Now rank the trust within its parent ICB
  df_final <- get_rank_in_icb(df_merged)
  
  return(df_final)
}

#' Rank trusts within their parent ICB
#'
#' @param df_merged a datframe containing all trusts, already ranked for each metric across all trusts
#'
#' @return enriched dataframe with the rank_on_icb added
get_rank_in_icb <- function(df_merged) {
  
  # give NA values a rank_in_icb = -99 and just set the rank_in_icb = ranking for others
  df_merged <- df_merged %>%
    mutate(rank_in_icb = case_when(ranking == -99 ~ -99,
                                   TRUE ~ ranking))
  
  # group and rank in groups
  df_ranked_subset <- df_merged %>%
    filter(ranking != -99) %>%
    arrange(location_type, icb_code, metric, ranking) %>%
    group_by(location_type, icb_code, metric) %>%
    mutate(rank_in_icb = rank(ranking, ties.method = "min")) %>%
    select(-c(ranking,
              value,
              delta_from_mean,
              is_latest_date,
              is_na))
  
  df_final <- df_merged %>%
    left_join(df_ranked_subset,
              by = c("location_type",
                     "location_id",
                     "location_name",
                     "icb_code",
                     "current_date",
                     "category",
                     "sub_category",
                     "metric"
              )) %>%
    rename(rank_in_icb = rank_in_icb.x) %>%
    mutate(rank_in_icb = ifelse(rank_in_icb==-99, rank_in_icb, rank_in_icb.y)) %>%
    select(-c(rank_in_icb.y))
  
  # smarten the data before returning
  df_final <- df_final %>%
    select(icb_code,
           location_type, 
           location_id,
           location_name,
           current_date,
           category,
           sub_category,
           metric,
           value,
           ranking,
           rank_in_icb,
           delta_from_mean,
           is_na,
           is_latest_date)
  
  return(df_final)
}