#https://stackoverflow.com/questions/55615825/allowing-multiple-numbers-in-one-numericinput-in-shiny
extract <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}
my_fun <- function(df, color_var, breaks = ""){

  breaks_nums <- extract(breaks)


  if (!anyNA(breaks_nums) && length(breaks_nums) >= 2) {
    message("cutting color var!")
    message(breaks_nums)
    prepped_df <- df %>%
      mutate(across(all_of(color_var),
                    ~ cut(.x,
                          breaks = !!breaks_nums,
                          right = FALSE,
                          include.lowest = TRUE
                    )
      )

      )
  } else{
    message("not cutting color var!")
    prepped_df <- df
  }
  return(prepped_df)
}

my_fun(mtcars, "mpg", "0,10,15,30,Inf") %>% count(mpg)
my_fun(mtcars_sql, "mpg", "0,10,15,30,Inf") %>% count(mpg)
