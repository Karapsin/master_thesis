source("R\\libraries.R")
source("R\\fun_helpers\\helpers.R")

final_data_clean <- function(kids_df, 
                             lower_age, 
                             upper_age,
                             kids_num_trunc,
                             kid_order_trunc
                             ){
  
  time_print("final steps")
  #for some families list of relatives within one year is different
  #this part aims to fix that
   df <- 
     kids_df%>%
      select(-relatives_list, -relatives_fam_id_list)%>% 
      left_join(#that part messes up family ties for individuals (which are not needed in this study)
                #but should fix nuclear family status and hh size for some of them
                #in which we are interested in
               kids_df%>% 
                  select(year, mother_id, father_id, relatives_list, relatives_fam_id_list)%>%
                  distinct()%>%
                  rowwise()%>%
                  mutate(rel_num=length(relatives_list))%>%
                  group_by(year, mother_id, father_id)%>%
                  filter(rel_num==max(rel_num))%>%
                  ungroup(),
                by=join_by(year, mother_id, father_id)
      )%>%
       rowwise()%>% 
       mutate(kids_num_1=(which(relatives_list==6))%>%length()%>%`+`(1))%>%  
       group_by(year, mother_id, father_id)%>%
       mutate(kids_num_2=length(unique(kid_id)),
              kids_num_3=max(kid_order)
       )%>%
       ungroup()%>%
       mutate(kids_num=case_when(kids_num_1>=kids_num_2 & kids_num_1>=kids_num_3 ~ kids_num_1,
                                 kids_num_2>=kids_num_1 & kids_num_2>=kids_num_3 ~ kids_num_2,
                                 kids_num_3>=kids_num_1 & kids_num_3>=kids_num_2 ~ kids_num_3
                       )
       )%>%
       select(-kids_num_1, -kids_num_2, -kids_num_3)%>%
       group_by(kid_id)%>%
       filter(age>=lower_age & age<=upper_age)%>%
       filter(year==min(year))
  
  
  time_print("variables construction")
  df%<>%
      mutate(birth_year=as.factor(birth_year),
             kid_univ_diploma=as.factor(kid_univ_diploma),
             father_univ_diploma=as.factor(father_univ_diploma),
             mother_univ_diploma=as.factor(mother_univ_diploma),
             family_id=paste0(father_id, mother_id),
             prof=case_when(profession_code==-1 ~ "unemployed",
                            profession_code<6 & profession_code!=0 ~ "qualified", 
                            TRUE ~ "not qualifed"
                  )
      )%>%
    group_by(father_id, mother_id)%>%
    arrange(as.numeric(birth_year))%>%
    rowwise()%>%
    # mutate(spouse=ifelse(1 %in% relatives_list, "1", "0")%>%as.factor(),
    #        has_kids=ifelse(2 %in% relatives_list, "1", "0")%>%as.factor(),
    #        half_siblings=ifelse(7 %in% relatives_list, "1", "0")%>%as.factor(),
    #        elders=ifelse(9 %in% relatives_list, "1", "0")%>%as.factor(),
    #        
    #        parents_siblings=ifelse(10 %in% relatives_list, "1", "0")%>%as.factor(),
    #        siblings_in_law=ifelse(12 %in% relatives_list, "1", "0")%>%as.factor(),
    #        
    #        other_relatives=ifelse(13 %in% relatives_list, "1", "0")%>%as.factor(),
    #        not_relatives=ifelse(14 %in% relatives_list, "1", "0")%>%as.factor(),
    #        
    #        niblings=ifelse(15 %in% relatives_list, "1", "0")%>%as.factor(),
    #        cousins=ifelse(16 %in% relatives_list, "1", "0")%>%as.factor()
    # )%>%
    mutate(nuclear_family=ifelse(any(c(1, 2, 7, 9, 10, 11, 12, 13, 14, 15, 16) %in% relatives_list),
                                 "0",
                                 "1"
                           )%>%
                           factor(levels = c("0", "1"))
    
    )%>%
    filter(!(3 %in% relatives_list))%>%
    ungroup()%>%
    mutate(kid_order=kid_order%>%as.character()%>%as.numeric())%>%
    rowwise()%>%
    mutate(hh_size=case_when((1+length(relatives_list))>=(2+kids_num) ~ (1+length(relatives_list)),
                             TRUE ~ (2+kids_num)
                             
                    )
    )%>%
    mutate(hh_size=case_when(hh_size>=7 ~ ">=7",
                             TRUE ~ as.character(hh_size)
                             
                   )%>%
                   factor(levels = c("3", "4", "5", "6", ">=7"))
    )%>%
    ungroup()%>%
    mutate(kids_num=case_when(kids_num>=kids_num_trunc ~paste0(">=", as.character(kids_num_trunc)),
                              TRUE ~kids_num%>%as.character()
                              
    )%>%
      factor(levels=c(as.character(1:(kids_num_trunc-1)), 
                      paste0(">=", as.character(kids_num_trunc))
                    )
      )
    )%>%
    ungroup()%>%
    mutate(
      kid_order=case_when(kid_order>=kid_order_trunc ~paste0(">=", as.character(kid_order_trunc)),
                          TRUE ~kid_order%>%as.character()
                          
      )%>%
        factor(levels=c(as.character(1:(kid_order_trunc-1)), 
                        paste0(">=", as.character(kid_order_trunc))
                      )
        )
    )%>%
    mutate(birth_year=birth_year%>%as.character()%>%as.numeric())%>%
    filter(birth_year>=1970)%>%
    mutate(birth_year_trunc=birth_year%>%`/`(5)%>%floor()%>%`*`(5))%>%
    mutate(birth_year_trunc=as.factor(birth_year_trunc))%>%
    left_join(kids_df%>%   #lag income
                select(kid_id,
                       year,
                       totexpr,
                       age)%>%
                filter(age<=18)%>%
                select(-age)%>%
                rename(lag_expenses=totexpr,
                       lag_expenses_year=year       
                ),
              join_by(kid_id==kid_id, closest(year>=lag_expenses_year))

    )

  
  
  all_df <- get_ind_file("csv/individual.csv")
  
  #idind of those kids who are living in different hh than their parents
  #if in this wave id_h for kid is different from id_h for his mother and his father
  #and year of wave which has been used in the model is >= that year of this wave
  #then he is living separetely
  
  separated_id <- 
    df%>%
      select(year, id_w, kid_id, father_id, mother_id, age)%>%
      mutate(year=as.character(year)%>%as.numeric())%>%
      left_join(all_df%>%
                  select(year, idind, id_h)%>%
                  rename(f_id=idind, f_h=id_h, f_year=year),
                by=c("father_id"="f_id")
      )%>%
      left_join(all_df%>%
                  select(year, idind, id_h)%>%
                  rename(k_id=idind, k_h=id_h, k_year=year),
                by=c("kid_id"="k_id")
      )%>%
      left_join(all_df%>%
                  select(year, idind, id_h)%>%
                  rename(m_id=idind, m_h=id_h, m_year=year),
                by=c("mother_id"="m_id")
      )%>%
      filter(k_h!=f_h & k_h!=m_h & f_year==k_year & k_year==m_year)%>%
      filter(year>=k_year)%>%
      pull(kid_id)%>%
      unique()

  
  df%>%
    mutate(is_separated=ifelse(kid_id %in% separated_id, "1", "0")%>%
                          factor(levels = c("0", "1")),
           
           residence=residence%>%
             factor(levels=c("city", "village",  "1mln", "MSK", "SPB"))
    )%>%
    filter(kid_id!=18773)%>% #anomaly family ties in the hh file
    rename(univ_diploma=kid_univ_diploma)%>%
    select(-relatives_fam_id_list, -relatives_list)%>%
    distinct()
    
}



