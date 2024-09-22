csv_and_dics_from_sav <- function(sav_file){
  
  df <- 
    sav_file%>%
    paste0("sav/", ., ".sav")%>%
    rlms::rlms_read()
  
  names_codes <- 
    df%>%
    map(~attr(.x, 'label'))%>%
    unlist()
  
  
  data.frame(name=names_codes, 
             code=names(names_codes)
  )%>%
    data.table::fwrite(file = paste0("dics/", 
                                     sav_file, 
                                     ".csv"
    )
    )
  
  data.table::fwrite(df, 
                     file = paste0("csv/", 
                                   sav_file, 
                                   ".csv"
                     )
  )
  
}

#sav -> csv
do_sav_csv_conversion <- function(do){
  if(do){
    time_print("starting sav->csv conversion")
    list.files("sav")%>%
      str_remove(".sav")%>%
      walk(csv_and_dics_from_sav)
    time_print("sav->csv conversion is finished")
  }
}