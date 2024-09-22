source("R\\libraries.R")
get_ind_file <- function(ind_csv){
  ind_df <-  
    data.table::fread(ind_csv, 
                      select = c("idind",
                                 "id_i",
                                 "id_h",
                                 "id_w", 
                                 "age",
                                 "year",
                                 "h5", #gender
                                 "j69.9c", #year_birth
                                 "h6", #year_birth_2
                                 "educ",
                                 "diplom",
                                 "diplom_1",
                                 "i3", #birth_place (village, city ...)
                                 "j90", #student, worker etc
                                 "occup08",
                                 "j4.2", #which education is needed for your job
                                 "j1", #job status
                                 "origsm", #is representative
                                 "inwgt"
                               )
    )%>%
    distinct()%>%
    mutate(id_i=coalesce(id_i, idind))%>%
    group_by(idind)%>%
    mutate(birth_year=j69.9c%>%
             coalesce(h6, age-year)%>%
             min() 
    )%>%
    ungroup()%>%
    mutate(univ_diploma=ifelse(
      coalesce(educ, -1)>=19  #or 19 if we want those who are at least enrolled, #21 for graduated
      | coalesce(diplom, -1)==6 
      | coalesce(diplom_1, -1)==6 
      | j90==16 #needed for those who are studying, but not graduated
      , 
      1, 
      0
    ),
    #hs_diploma=ifelse(educ>=14 | diplom>=4 | diplom_1>=6, 1, 0)
    )%>% 
    rename(profession_code=occup08)%>%
    mutate(profession_code=coalesce(profession_code, 
                                    ifelse(j1==5 | is.na(j1), -1, NA_integer_),
                                    case_when(is.na(j4.2) ~ NA_integer_,
                                              j4.2 %in% 4:5 ~ 4,
                                              TRUE ~ 7
                                    )
                                    
    )
    )%>%
    select(-educ, -diplom, -diplom_1, -j90)
}

get_hh_file <- function(hh_csv, rel_cols, id_cols, gender_cols){
  
  house_df <-  data.table::fread(hh_csv, 
                                 select = c("id_w", 
                                            "id_h", 
                                            rel_cols,
                                            id_cols,
                                            gender_cols
                                            #"nfm",
                                            #"nfm1.o", 
                                            #"nfm1.n"
                                 )
                                 
  )#%>%mutate(family_size=coalesce(nfm, nfm1.o, nfm1.n))
}

get_residence_file <- function(inc_csv){
  income_df <-  
    data.table::fread(inc_csv)%>%
    select(id_w, 
           id_h, 
           totexpr, 
           popul, 
           status,
           ncat5, 
           ncat6       
    )%>%
    mutate(residence=case_when(status>=3~"village",
                               between(popul, 3000000, 7000000)~"SPB",
                               popul>=8000000~"MSK",
                               popul>=1000000~"1mln",
                               TRUE~"city"
                               
    ),
    elders_presence=ifelse(coalesce(ncat5+ncat6, 0)>0, ncat5+ncat6, 0)
    )%>%
    select(-popul, -status, -ncat5, -ncat6)%>%  
    group_by(id_w)%>%
    mutate(totexpr=case_when(totexpr<=quantile(totexpr, 0.25, na.rm=TRUE) ~ "I Q",
                             totexpr>= quantile(totexpr, 0.25, na.rm=TRUE) & totexpr<=quantile(totexpr, 0.50, na.rm=TRUE) ~ "II Q",
                             totexpr>= quantile(totexpr, 0.50, na.rm=TRUE) & totexpr<=quantile(totexpr, 0.75, na.rm=TRUE) ~ "III Q",
                             totexpr>= quantile(totexpr, 0.75, na.rm=TRUE) & totexpr<=quantile(totexpr, 1, na.rm=TRUE) ~ "IV Q"
                             
    )%>%
      factor(levels = c("I Q", "II Q", "III Q", "IV Q"))
    )%>%
    ungroup()
}

read_source_data <- function(hh_dic="dics/household.csv",
                             hh_csv="csv/household.csv",
                             res_csv="csv/households_expenses_incomes.csv",
                             ind_csv="csv/individual.csv"
){
  
  house_dic <- data.table::fread(hh_dic, encoding = "UTF-8")
  
  rel_cols <- 
    house_dic%>%
    filter(str_detect(name, "член семьи приходится"))%>%
    pull(code)
  
  id_cols <- 
    house_dic%>%
    filter(str_detect(name, "ндивидуальный номер"))%>%
    pull(code)
  
  gender_cols <- 
    house_dic%>%
    filter(str_detect(name, "акого пола этот челов"))%>%
    pull(code)
  
  time_print("hh file preparation")
  house_df <- get_hh_file(hh_csv, rel_cols, id_cols, gender_cols)
  time_print("finished")
  
  time_print("residence file preparation")
  res_df <- get_residence_file(res_csv)
  time_print("finished")
  
  time_print("ind file preparation")
  ind_df <-  get_ind_file(ind_csv)
  time_print("finished")
  
  list(ind_df, house_df, res_df, rel_cols, id_cols, gender_cols)
  
}
