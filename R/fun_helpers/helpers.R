source("R\\libraries.R")
#small functions used in more than one module
time_print <- function(str_input){
  Sys.time()%>%
    substr(12, 16)%>%
    paste(str_input)%>%
    message()
}

check_for_optional_vars <- function(df, optional_vars){
  map_lgl(optional_vars,
          ~df%>%
            as.tibble()%>%
            .[.x]%>%
            unique()%>%
            pull()%>%
            length()%>%
            `>`(1)
  )
}



