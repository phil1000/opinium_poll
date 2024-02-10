# Load required packages --------------------------------------
library(tidyverse)
library(tidyxl)
library(unpivotr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(magrittr)
library(openxlsx)

# source other scripts
helper_functions <- new.env(); source("./R/helper_functions.R", 
                                      local = helper_functions)

run_analysis <- function() {
  
  config_path <- file.path("input", "config.yml")
  config <- yaml::read_yaml((config_path))
  
  run_opinium(config)
  run_survation(config)

}

run_opinium <- function(config) {
  
  logger$info("---------- Downloading data from Opinium URL ------------ ")
  
  file_path <- file.path("input", basename(config$opinium_url))
  download.file(config$opinium_url, file_path, mode = "wb")
  
  df_untidy <- tidyxl::xlsx_cells(
    file_path,
    include_blank_cells = TRUE
  )
  
  front_sheet <- df_untidy %>%
    filter(sheet == 'FRONT PAGE')
  
  target_cell <- front_sheet %>%
    filter(str_detect(character, "FIELD DATES"))
  
  survey_dates <- front_sheet %>%
    filter(
      row == (target_cell$row),
      col == (target_cell$col + 3)
    ) 
  
  survey_date <- survey_dates %>% 
    select(character) %>% 
    pull()
  
  survey_date <- as.character(survey_date)
  
  logger$info(paste0("Survey dates: ", survey_date))
  
  df_sheet <- df_untidy %>%
    filter(sheet == 'HeadlineVI')
  
  target_cell <- df_sheet %>%
    filter(str_detect(character, "Headline Voting Intention"))
  
  df <- df_sheet %>%
    filter(
      row >= (target_cell$row)
    )
  
  df <- behead(df, "left", party)
  df <- df %>% mutate_at(c('party'), ~na_if(., '')) %>%
    fill(party, .direction = "down")
  
  df <- behead(df, "left", voting_intention)
  
  df <- behead(df, "N", category)
  df <- df %>% mutate_at(c('category'), ~na_if(., '')) %>%
    fill(category, .direction = "down")
  
  df <- behead(df, "N", sub_category)
  
  df <- df %>%
    rename(value = content)
  
  df_totals <- df %>%
    select(party, voting_intention) %>%
    mutate(voting_intention = as.numeric(voting_intention)) %>%
    filter(voting_intention < 1.001) %>%
    group_by(party) %>%
    summarise(voting_intention = mean(voting_intention)) %>%
    ungroup() %>%
    mutate(category = "Voting Intention",
           sub_category = "Voting Intention") %>%
    rename(value = voting_intention)
  
  categories <- c("Age", "Region", "Gender")
  exclude_parties <- c("Plaid Cymru", "SNP", "Other")
  exclude_regions <- c("Wales", "Scotland")
  
  df_breakdowns <- df %>%
    select(category, sub_category, party, value) %>%
    mutate(value = as.numeric(value)) %>%
    filter(value < 1.001,
           category %in% categories)
  
  df_all <- rbind(df_breakdowns, df_totals) %>%
    arrange(category, sub_category, -value) %>%
    rename(percent = value,
           demog_level_1 = category,
           demog_level_2 = sub_category) %>%
    filter(!party %in% exclude_parties,
           !demog_level_2 %in% exclude_regions)

  outfile <- file.path(config$output_dir,
                       paste0(Sys.Date(),
                              "_opinium_voting_data.png"))
  
  df_final <- df_all
  
  demog_level_1_list <- df_final %>% distinct(demog_level_1) %>% pull()
  
  plot_list <- list()
  i = 1
  
  for (item in demog_level_1_list) {
    
    df <- df_final %>% filter(demog_level_1 == item)
    
    demog_count <- n_distinct(df$party)
    calc_width <- demog_count * 0.1
    
    p <- ggplot(df, aes(fill=party,
                        y=reorder(party, percent),
                        x=percent))
    
    p <- p + geom_bar(position="dodge",
                      stat="identity",
                      width = calc_width)
    
    p <- p + geom_text(aes(label = scales::percent(round(percent, 2))), 
                       hjust=1, size=5, color="white")
    
    p <- p + scale_fill_manual(breaks = c("Lab", 
                                          "Con", 
                                          "Lib Dem",
                                          "Reform", 
                                          "Green"),
                               values = c("#E4003B", 
                                          "#0087DC", 
                                          "#FAA61A",
                                          "#12B6CF", 
                                          "#02A95B"))
    
    p <- p + scale_x_continuous(labels = scales::percent)
    
    p <- p + theme(axis.title = element_blank())
    
    p <- p + theme(text = element_text(size=12))
    
    p <- p + theme(legend.position="none")
    
    p <- p + ggtitle(item) +
      theme(plot.title = element_text(hjust = 0.5, size=8))
    
    p <- p + facet_wrap(~demog_level_2)
    
    plot_list[[i]] = p
    i <- i + 1
  }
  
  figure <- ggarrange(plotlist=plot_list)
  
  fig_title <- paste0("Opinium: ", survey_date)
  
  figure <- annotate_figure(figure,
                            top = text_grob(fig_title, 
                                            color = "red", 
                                            face = "bold", 
                                            size = 14))
  
  ggsave(outfile, figure, width = 20, height = 20)
  
  x <- 1
  
}

run_survation <- function(config) {
  
  logger$info("---------- Downloading data from Survation URL ------------ ")
  
  file_path <- file.path("input", basename(config$survation_url))
  download.file(config$survation_url, file_path, mode = "wb")
  
  df_untidy <- tidyxl::xlsx_cells(
    file_path,
    include_blank_cells = TRUE
  )
  
  survey_dates <- get_poll_dates(df_untidy %>%
                                   filter(sheet == 'Cover Sheet and Methodology'))
  
  df_sheet <- df_untidy %>%
    filter(sheet == 'Tables')
  
  outfile <- file.path(config$output_dir,
                       paste0(Sys.Date(),
                              "_surv_likelihood_to_vote.png"))
  
  title_str <- paste0("Survation (", survey_dates, "): Likelihood to vote (%)")
  
  voting_likelihood_plots(df_sheet,
                          "Table_V1.",
                          outfile,
                          title_str)
  
  outfile <- file.path(config$output_dir,
                       paste0(Sys.Date(),
                              "_surv_voting_preference.png"))
  
  title_str <- paste0("Survation (", survey_dates,
                      "): Who will you vote for? (% of those likely to vote)")
  
  voting_preference_plots(df_sheet,
                          "Table_V2.2",
                          outfile,
                          title_str)
}

get_poll_dates <- function(df_sheet) {
  target_cell <- df_sheet %>%
    filter(str_detect(character, "Fieldwork Dates"))
  
  survey_dates <- df_sheet %>%
    filter(
      row == (target_cell$row + 1),
      col == (target_cell$col)
    ) 
  
  survey_date <- survey_dates %>% 
    select(character) %>% 
    pull()
  
  survey_date <- as.character(survey_date)
  
  logger$info(paste0("Survey dates: ", survey_date))
  
  return(survey_date)
}

voting_preference_plots  <- function(df_sheet,
                                     find_text,
                                     plot_file,
                                     plot_header) {
  
  logger$info("-------- Transforming and plotting voting preference --------- ")
  
  df <- get_demog_headers(df_sheet,
                          find_text)
  
  # use the column to the left of the current cell as the location names
  df <- behead(df, "left", party)
  
  # fill the voting likelihood column for the percentage values
  df <- df %>% mutate_at(c('party'), ~na_if(., '')) %>%
    fill(party, .direction = "down")
  
  df <- df %>%
    filter(!is.na(demog_level_2)) %>%
    mutate(frequency = numeric) %>%
    select(party,
           demog_level_1,
           demog_level_2,
           frequency)
  
  df_freq <- df %>%
    filter(frequency >= 1)
  
  df_pct <- df %>%
    filter(frequency < 1) %>%
    rename(percent = frequency)
  
  df_final <- df_freq %>%
    left_join(df_pct, by=c('party',
                           'demog_level_1',
                           'demog_level_2')) %>%
    mutate(percent = if_else(is.na(percent), 1, percent)) %>%
    mutate(frequency = as.numeric(frequency),
           percent = as.numeric(percent))
  
  # remove totals
  totals <- c("Unweighted Total", "Weighted Total")
  df_final <- df_final %>%
    filter(!(party %in% totals)) 
  
  # remove unwanted categories
  del_categories <- c("Voting Intention", "Party Identity", "EU16 Vote")
  df_final <- df_final %>%
    filter(!(demog_level_1 %in% del_categories)) 
  
  demog_level_1_list <- df_final %>% distinct(demog_level_1) %>% pull()
  
  plot_list <- list()
  i = 1
  
  for (item in demog_level_1_list) {
    
    df <- df_final %>% filter(demog_level_1 == item)
    
    demog_count <- n_distinct(df$party)
    calc_width <- demog_count * 0.0625
    
    p <- ggplot(df, aes(fill=party,
                        y=reorder(party, percent),
                        x=percent))
    
    p <- p + geom_bar(position="dodge",
                      stat="identity",
                      width = calc_width)
    
    p <- p + geom_text(aes(label = scales::percent(round(percent, 2))), 
                       hjust=1, size=2.5, color="white")
    
    p <- p + scale_fill_manual(breaks = c("Labour", 
                                          "Conservative", 
                                          "Liberal Democrats",
                                          "Reform UK", 
                                          "Scottish National Party",
                                          "Green Party", 
                                          "Plaid Cymru",
                                          "Reclaim Party", 
                                          "UKIP",
                                          "Other",
                                          "Undecided",
                                          "Refused"),
                               values = c("#E4003B", 
                                          "#0087DC", 
                                          "#FAA61A",
                                          "#12B6CF", 
                                          "#FDF38E",
                                          "#02A95B",
                                          "#005B54",
                                          "#C03F31",
                                          "#6D3177",
                                          "#d3d3d3",
                                          "#d3d3d3",
                                          "#d3d3d3"))
    
    p <- p + scale_x_continuous(labels = scales::percent)
    
    p <- p + theme(axis.title = element_blank())
    
    p <- p + theme(text = element_text(size=8))
    
    p <- p + theme(legend.position="none")
    
    p <- p + ggtitle(item) +
      theme(plot.title = element_text(hjust = 0.5, size=8))
    
    p <- p + facet_wrap(~demog_level_2)
    
    plot_list[[i]] = p
    i <- i + 1
  }
  
  figure <- ggarrange(plotlist=plot_list)
  
  figure <- annotate_figure(figure,
                            top = text_grob(plot_header, 
                                            color = "red", 
                                            face = "bold", 
                                            size = 14))
  
  ggsave(plot_file, figure, width = 20, height = 20)
  
  #write_csv(df_final, "./output/aa_tempado_df.csv")
}

voting_likelihood_plots  <- function(df_sheet,
                                     find_text,
                                     plot_file,
                                     plot_header) {
  
  logger$info("-------- Transforming and plotting voting likelihood --------- ")
  
  df <- get_demog_headers(df_sheet,
                          find_text)
  
  # use the column to the left of the current cell as the location names
  df <- behead(df, "left", voting_likelihood)
  
  # fill the voting likelihood column for the percentage values
  df <- df %>% mutate_at(c('voting_likelihood'), ~na_if(., '')) %>%
    fill(voting_likelihood, .direction = "down")
  
  df <- df %>%
    filter(!is.na(demog_level_2)) %>%
    mutate(frequency = numeric) %>%
    select(voting_likelihood,
           demog_level_1,
           demog_level_2,
           frequency)
  
  df_freq <- df %>%
    filter(frequency >= 1)
  
  df_pct <- df %>%
    filter(frequency < 1) %>%
    rename(percent = frequency)
  
  df_final <- df_freq %>%
    left_join(df_pct, by=c('voting_likelihood',
                           'demog_level_1',
                           'demog_level_2')) %>%
    mutate(percent = if_else(is.na(percent), 1, percent)) %>%
    mutate(frequency = as.numeric(frequency),
           percent = as.numeric(percent))
  
  # plot only those certain to vote
  certain_to_vote <- df_final %>%
    filter(voting_likelihood == "10 - certain to vote") %>%
    filter(demog_level_1 != "Total")
  
  demog_level1_list <- certain_to_vote %>% distinct(demog_level_1) %>% pull()
  
  plot_list <- list()
  i = 1
  
  for (item in demog_level1_list) {
    
    df <- certain_to_vote %>% filter(demog_level_1 == item)
    
    demog_count <- n_distinct(df$demog_level_2)
    calc_width <- demog_count * 0.0625
    
    p <- ggplot(df, aes(y=reorder(demog_level_2, percent),
                        x=percent))
    
    p <- p + geom_bar(fill= 'firebrick',
                      alpha=0.6,
                      position="dodge",
                      stat="identity",
                      width = calc_width)
    
    p <- p + scale_x_continuous(labels = scales::percent)
    
    p <- p + theme(axis.title = element_blank())
    
    p <- p + theme(text = element_text(size=8))
    
    p <- p + ggtitle(item) +
      theme(plot.title = element_text(hjust = 0.5, size=8))
    
    plot_list[[i]] = p
    i <- i + 1
  }
  
  figure <- ggarrange(plotlist=plot_list)
  
  figure <- annotate_figure(figure,
                            top = text_grob(plot_header, color = "red", face = "bold", size = 14))
  
  ggsave(plot_file, figure, width = 10, height = 6)
  
}

get_demog_headers <- function(df_sheet,
                              find_text) {
  
  target_cell <- df_sheet %>%
    filter(str_detect(character, find_text)) %>%
    filter(row_number()==1)
  
  df <- df_sheet %>%
    filter(
      row >= (target_cell$row + 3),
      col >= (target_cell$col)
    )
  
  # finding the first completely empty row, identify the end of the table
  # and removing everything below including notes
  table_end_row <- df %>%
    
    # skip the first column
    filter(
      col >= (target_cell$col + 1)
    ) %>%
    
    group_by(row) %>%
    filter(sum(is_blank) == length(is_blank)) %>%
    ungroup() %>%
    filter(row == min(row)) %>%
    pull(row) %>%
    unique()
  
  # finding the first completely empty column, identify the end of the table
  # and removing everything east
  table_end_col <- df %>%
    group_by(col) %>%
    filter(sum(is_blank) == length(is_blank)) %>%
    ungroup() %>%
    filter(col == min(col)) %>%
    pull(col) %>%
    unique()
  
  if (identical(table_end_row, integer(0))) # this means I have a null value
    table_end_row <- max(df$row) + 1
  
  if (identical(table_end_col, integer(0))) # this means I have a null value
    table_end_col <- max(df$col) + 1
  
  df <- df %>%
    filter(row < table_end_row) %>%
    filter(col < table_end_col)
  
  # the row above contains the current_date so use these as the dates
  df <- behead(df, "N", demog_level_1)
  
  df <- df %>% 
    fill(demog_level_1, .direction = "down")
  
  # use the next row as metric names
  df <- behead(df, "N", demog_level_2)
  
  return(df)
}
