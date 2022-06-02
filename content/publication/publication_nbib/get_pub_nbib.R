rm(list = ls())
library(readr)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)

get_pub <- function(filename, type = "nbib") {


  loc_dir <- "content/publication/"
  # data <- read_file(paste0("content/publication/publication_nbib/", filename))
  data <- read_lines(paste0(loc_dir, "publication_nbib/", filename))

  if (type == "nbib") {
    title_num <- which(str_detect(data, "^TI"))
    title <- str_remove(data[title_num], "TI\\s+-\\s+")
    for (i in (title_num + 1):length(data)) {
      if (str_detect(data[i], "^\\s{2,}")) {
        title <- paste0(title, str_remove(data[i], "^\\s+"))
      } else {
        break
      }
    }
    # Remove period if needed
    title <- str_remove(title, ".$")

    lid_num <- which(str_detect(data, "^LID"))
    doi <- str_remove(data[lid_num], "LID\\s+-\\s+")
    for (i in (lid_num + 1):length(data)) {
      if (str_detect(data[i], "^\\s{2,}")) {
        doi <- paste0(doi, str_remove(data[i], "^\\s+"))
      } else {
        break
      }
    }

    ab_num <- which(str_detect(data, "^AB"))
    abstract <- str_remove(data[ab_num], "AB\\s+-\\s+")
    for (i in (ab_num + 1):length(data)) {
      if (str_detect(data[i], "^\\s{2,}")) {
        abstract <- paste0(abstract, str_remove(data[i], "^\\s+"))
      } else {
        break
      }
    }
    # Format headers in abstract
    # if (str_detect(abstract, "[A-Z]{2,}")) {
    #   browser()
    # }


    # Authors
    fau_nums <- which(str_detect(data, "^FAU"))
    authors <- c()
    first_author <- ""
    for (i in fau_nums) {
      tmp <- str_remove(data[i], "^FAU\\s+-\\s+")
      # swap what is before and after the comma
      tmp <- str_split(tmp, ",", n = 2)
      if (is.null(authors)) {
        if (length(str_split(str_trim(tmp[[1]][1]), "-")[[1]]) == 1) {
          first_author <- str_trim(tmp[[1]][1])
        } else {
          first_author <- str_split(str_trim(tmp[[1]][1]), "-")[[1]][2]
        }

      }
      authors <- c(authors, paste0(str_trim(tmp[[1]][2]), " ",
                                   str_trim(tmp[[1]][1])))

    }

    date_num <- which(str_detect(data, "^DP"))
    date <- ymd(str_remove(data[date_num], "^DP\\s+-\\s+"))

    type_num <- which(str_detect(data, "^PT"))
    type <- NA
    if (str_remove(data[type_num], "^PT\\s+-\\s+") == "Journal Article") {
      type <- '["2"]'
    }

    ta_num <- which(str_detect(data, "^TA"))
    journal <- str_remove(data[ta_num], "^TA\\s+-\\s+")

    folder_name <- paste0(first_author, "-", year(date), "-",
                          str_to_lower(str_extract(title, "^[A-Za-z]+")))

    # tmp <- tempfile()
    # Create new folder if it doesn't already exist
    # browser()
    ck_dir <- which(list.files(loc_dir) == folder_name)
    if (length(ck_dir) == 0) dir.create(paste0(loc_dir, folder_name))
    tmp <- file(paste0(loc_dir, folder_name, "/index.md"), "w")
    write_lines("---", tmp)
    write_lines(paste0("title: ", '"', title, '"'), tmp)
    write_lines(paste0("date: ", date), tmp, append = TRUE)
    write_lines(paste0("authors: [", paste0('"', authors, '"',
                                            collapse = ", "), "]"),
                tmp, append = TRUE)
    if (!is.na(type)) write_lines(paste0("publication_types: ", type),
                                  tmp, append = TRUE)
    write_lines(paste0("abstract: ", '"', abstract, '"'), tmp, append = TRUE)
    write_lines(paste0("featured: false"), tmp, append = TRUE)
    write_lines(paste0("publication: \"*", journal, "*\""), tmp, append = TRUE)
    write_lines(paste0("doi: ", '"', doi, '"'), tmp, append = TRUE)
    write_lines("---", tmp)
    close(tmp)
  } else if (type == "bibtex") {
    # Find title line
    title_num <- which(str_detect(data, "^\\s+title"))
    title <- str_split(str_remove(str_remove_all(str_remove_all(data[title_num],
                                                                "^\\s+"),
                                                 "[{}]"), ",$"), "=")
    title <- paste0(title[[1]][1], ': "', str_to_sentence(title[[1]][2]), '"')

    author_num <- which(str_detect(data, "^\\s+author"))
    tmp <- str_extract_all(data[author_num], "[A-Za-z]+,\\s[A-Za-z]")
    authors <- c()
    for (i in 1:length(tmp[[1]])) {
      tmp_name <- str_split(tmp[[1]][i], ",")
      authors <- c(authors, str_trim(paste0(tmp_name[[1]][2], " ",
                                            tmp_name[[1]][1])))
    }

    journal_num <- which(str_detect(data, "^\\s+journal"))
    journal <- str_split(str_remove(str_remove_all(str_remove_all(data[journal_num],
                                                                "^\\s+"),
                                                 "[{}]"), ",$"), "=")
    journal <- paste0('publication: "*', journal[[1]][2], '*"')

    year_num <- which(str_detect(data, "^\\s+year"))
    date_val <- str_split(str_remove(str_remove_all(str_remove_all(data[year_num],
                                                                  "^\\s+"),
                                                   "[{}]"), ",$"), "=")
    date_val <- paste0(date_val[[1]][2])

    folder_name <- str_remove(str_remove(data[1], "@article\\{"), "\\,")
    ck_dir <- which(list.files(loc_dir) == folder_name)
    if (length(ck_dir) == 0) dir.create(paste0(loc_dir, folder_name))
    tmp <- file(paste0(loc_dir, folder_name, "/index.md"), "w")
    write_lines("---", tmp)
    write_lines(title, tmp)
    write_lines(paste0("publishDate: ", date_val), tmp)
    write_lines(paste0("authors: [",
                       paste0('"', authors, '"', collapse = ", "),
                       "]"), tmp)
    write_lines('abstract: ""', tmp)
    write_lines('publication_types: ["2"]', tmp)
    write_lines("featured: false", tmp)
    write_lines(journal, tmp)
    write_lines("---", tmp)
    close(tmp)
  }


  # write_file(tmp, "content/publication/publication_nbib/index.md")

}
