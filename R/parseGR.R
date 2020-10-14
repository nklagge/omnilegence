library(dplyr)
library(gender)
library(httr)
library(purrr)
library(stringr)
library(xml2)

gr_query <- function(urlstub, arglist){
  arglist <- c(key = getOption("GR_API_KEY"), arglist)
  url <- paste0("https://www.goodreads.com/", urlstub, "?")
  url %>%
    GET(query = arglist) %>%
    content(as = "parsed")
}
parse_node <- function(node){
  node %>%
    xml_children() %>%
    keep(~ xml_length(.x) == 0) %>%
    set_names(xml_name(.)) %>%
    map(xml_text) %>%
    as_tibble()
}
parse_nodeset <- function(xml, xpath){
  xml %>%
    xml_find_all(xpath) %>%
    map(parse_node) %>%
    bind_rows()
}
get_user_info <- function(user_id){
  urlstub <- "user/show"
  arglist <- list(id = user_id)
  urlstub %>%
    gr_query(arglist)
}
get_user_shelf_reviews <- function(user_id, shelf_name = "read",
                                   include_body = FALSE){
  # get review count to determine how many 200-review pages there are
  review_count <- user_id %>%
    get_user_info() %>%
    parse_nodeset("user/user_shelves/user_shelf")  %>%
    filter(name == shelf_name) %>%
    select(book_count) %>%
    as.integer()
  # derive number of 200-review pages
  page_count <- ceiling(review_count / 200)
  # pull review info from each page and consolidate
  dat <- c(1:page_count) %>%
    map(~ get_user_shelf_page_reviews(user_id, shelf_name, .x)) %>%
    bind_rows()
  # exclude review bodies unless requested
  if (!include_body){
    dat <- dat %>% select(-body)
  }
  dat
}
get_user_shelf_page_reviews <- function(user_id, shelf_name, pageno){
  urlstub <- "review/list"
  arglist <- list(v = 2,
                  id = user_id,
                  shelf = shelf_name,
                  per_page = 200,
                  page = pageno)
   k <- gr_query(urlstub, arglist)
   review_dat <- parse_nodeset(k, "reviews/review")
   book_dat <- parse_nodeset(k, "reviews/review/book") %>%
     set_names(paste0("book_", names(.)))
   author_dat <- parse_nodeset(k, "reviews/review/book/authors/author") %>%
     set_names(paste0("aut_", names(.)))
   bind_cols(review_dat, book_dat, author_dat)
}
get_author_info <- function(author_id){
  urlstub <- "author/show"
  arglist <- list(id = author_id)
  Sys.sleep(1)
  gr_query(urlstub, arglist)
}
get_book_info <- function(book_id){
  urlstub <- "book/show"
  arglist <- list(id = book_id)
  gr_query(urlstub, arglist)
}
get_author_genders <- function(dat){
  # get more author info
  j <- dat$aut_id %>%
    unique() %>%
    map(get_author_info) %>%
    map(~ parse_nodeset(.x, "author")) %>%
    bind_rows() %>%
    select(aut_id = id, aut_gender = gender)
  # join to base info
  dat %>%
    left_join(j)
}
get_popshelves <- function(book_id){
  rmshelves <- c("to-read", "currently-reading", "favorites", "book-club",
                 "kindle", "owned", "owned-books", "to-buy")
  x <- get_book_info(book_id) %>%
    xml_find_all("book/popular_shelves/shelf")
  # rate limit API requests when mapped
  Sys.sleep(1)
  cnt <- x %>%
    xml_attr("count") %>%
    as.integer()
  shf <- x %>%
    xml_attr("name")
  names(cnt) <- shf
  tibble(book_id = book_id,
         book_shf_toread = cnt["to-read"],
         book_shf_curr = cnt["currently-reading"],
         book_genre = shf[!(shf %in% rmshelves)][1])
}
genderize <- function(dat){
  dat <- dat %>%
    mutate(aut_fname = word(aut_name, 1))
  g <- dat$aut_fname %>%
    unique() %>%
    map(~ gender(.x, method = "genderize")) %>%
    bind_rows() %>%
    select(aut_fname = name, aut_gender2 = gender)
  dat %>%
    left_join(g)
}
export_md_from_gr_record <- function(rec) {
  yr_read <- substr(rec$read_at, 27, 30)
  mo_read <- match(substr(rec$read_at, 5, 7), month.abb)
  day_read <- substr(rec$read_at, 9, 10)
  san_title <- gsub("[^0-9A-Za-z ]", "", rec$book_title)
  fname <- paste0(yr_read, " ", mo_read, " ", day_read, " ", san_title, ".md")
  x <- character(14L)
  x[1] <- "---"
  x[2] <- paste0("date: ", yr_read, "-", mo_read, "-", day_read)
  x[3] <- "meta: true"
  x[4] <- paste0("title: ", san_title, " - ", rec$aut_name)
  x[5] <- "toc: false"
  x[6] <- "categories:"
  x[7] <- paste0("- ", rec$aut_name)
  x[8] <- "---"
  x[9] <- ""
  x[10] <- rec$body
  x[11] <- ""
  x[12] <- paste0("My rating: ", rec$rating, " stars  ")
  x[13] <- ""
  x[14] <- paste0("[IndieBound](https://www.indiebound.org/book/", rec$book_isbn13, ")")
  readr::write_lines(x, fname)
  invisible(TRUE)
}

dat <- get_user_shelf_reviews(2704424, "read", include_body = TRUE)
dat_test <- dat[c(1,2),]
for (i in 1:nrow(dat_test)) {
  export_md_from_gr_record(dat_test[i,])  
}

