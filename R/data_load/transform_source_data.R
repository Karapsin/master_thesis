source("R\\libraries.R")
#e.g.:
#person 1 (male) is a son of person 2 (female)
#then in the table (here text is used instead of numeric codes):
#
# kid_id  parent_id   value   relatives_list
#    1        2       son        mother
#    2        1       mother     son
#
# i.e. "kid" is that for whom in "value" column described how he is related to "parent"
#       and codes for relatives in the "relatives list" column are also for him

create_relatives_list <- function(ind_df, house_df, rel_cols, id_cols, gender_cols){
  
  time_print("searching for gender in the hh_df")
  gender_df <- 
    house_df%>%
      select(id_w, id_h, any_of(gender_cols))%>% #any_of (gender_cols) = select all columns for which names are in gender_cols vector
      pivot_longer(cols=-c("id_w", "id_h"),
                   names_to = "fam_num",
                   values_to="gender"
      )%>%
      filter(!is.na(gender))%>%
      mutate(fam_num=sub("\\..*", "", fam_num)%>% #get all before the first dot
                       str_remove("b"), #remove b
             gender=ifelse(gender=="1", "male", "female")
      )
  time_print("finished")
  
  time_print("searching for relatives pairs")
  relatives_parent_df <- 
    house_df%>%
    select(id_w, id_h, any_of(rel_cols))%>%
    pivot_longer(cols=-c("id_h", "id_w"))%>%
    filter(!is.na(value))%>%
    mutate(kid=sub("\\..*", "", name)%>% #get all before the first dot
             str_remove("b"), #remove b
           
           parent=sub("^[^.]+.","", name)%>% #get all after the first dot
             substring(2)%>% #remove 1st character (9 or .)
             str_remove("\\.") #remove dot if it is here
    )%>% #not actual kids and parents, however if value=4, that is indeed kid-parent pair
    select(id_w, id_h, kid, parent, value)%>%
    distinct()
  time_print("finished")
  
  time_print("id assigning in house df")
  id_df <- 
    house_df%>%
    select(id_w, id_h, any_of(id_cols))%>% #any_of (id_cols) = select all columns for which names are in id_cols vector
    pivot_longer(cols=-c("id_w", "id_h"),
                 names_to = "fam_num",
                 values_to="idind"
    )%>%
    filter(!is.na(idind))%>%
    mutate(fam_num=str_remove(fam_num, "idind"))
  time_print("finished")
  
  time_print("id and assigning to kid-parent pairs")
  kid_parent_id_df <- 
    relatives_parent_df%>%
    inner_join(relatives_parent_df%>%
                 group_by(id_w, id_h, kid)%>%
                 summarise(relatives_list=list(value),
                           relatives_fam_id_list=list(parent)
                 )%>%
                 ungroup(),
               by=c("id_w"="id_w", "id_h"="id_h", "kid"="kid")
               
    )%>%
    inner_join(id_df%>%
                 rename(kid_id_ind=idind), 
               by=c("id_w"="id_w", 
                    "id_h"="id_h", 
                    "kid"="fam_num"
               )
    )%>%
    inner_join(id_df%>%
                 rename(parent_id_ind=idind), 
               by=c("id_w"="id_w", 
                    "id_h"="id_h", 
                    "parent"="fam_num"
               )
    )%>%
    inner_join(gender_df, 
               by=c("id_w"="id_w", 
                    "id_h"="id_h", 
                    "kid"="fam_num"
               )
    )%>%
    inner_join(gender_df%>%
                 mutate(parent_type=ifelse(gender=="male", "father", "mother"))%>%
                 select(-gender), 
               by=c("id_w"="id_w", 
                    "id_h"="id_h", 
                    "parent"="fam_num"
               )
    )%>%
    select(id_w, id_h, value, kid_id_ind, gender, parent_id_ind, parent_type, relatives_list, relatives_fam_id_list)%>%
    distinct()
  time_print("finished")
  
  kid_parent_id_df
}

assign_parents <- function(kid_parent_id_df, ind_df, tie_value=4){
  time_print("combining ind df with parent-kid pairs, some transformations")

  kid_parent_id_df%>%
    filter(kid_id_ind!=56975)%>% #2 fathers 0_0
    filter(value==tie_value)%>%
    inner_join(ind_df%>%
                 select(id_w, year)%>%
                 distinct(), 
               by=c("id_w"="id_w")          
    )%>%
    pivot_wider(id_cols=c("year", "id_w", "id_h", "kid_id_ind", "gender", "relatives_list", "relatives_fam_id_list"),
                names_from=parent_type,
                values_from=parent_id_ind            
    )%>%
    rowwise()%>%
    filter(length(mother)!=0 & length(father)!=0)%>%
    ungroup()%>%
    inner_join(ind_df%>%
                 select(idind, id_w, birth_year)%>%
                 distinct(),
               by=c("id_w"="id_w", "kid_id_ind"="idind")
    )%>%
    group_by(id_w, birth_year, year, mother)%>%
    mutate(dad_count=length(unique(father)))%>%
    group_by(id_w, birth_year, year, father)%>%
    mutate(mom_count=length(unique(mother)))%>%
    filter(mom_count==1 & dad_count==1)%>%
    select(-dad_count, -mom_count)%>%
    ungroup()%>%
    mutate(mother=unlist(mother),
           father=unlist(father)       
    )%>%
    rename(father_id=father, 
           mother_id=mother       
    )%>%
    select(year, id_w, id_h, kid_id_ind, father_id, mother_id, relatives_list, relatives_fam_id_list, birth_year)%>%
    distinct()%>%
    group_by(kid_id_ind)%>%
    mutate(birth_year=min(birth_year))%>%
    rename(kid_id=kid_id_ind)%>%
    ungroup()
}


transform_source_data <- function(ind_df, house_df, res_df, rel_cols, id_cols, gender_cols){
  
  
  kids_df <- 
    create_relatives_list(ind_df, house_df, rel_cols, id_cols, gender_cols)%>%
      assign_parents(ind_df=ind_df, tie_value=4)

  
  kids_df%<>%
      inner_join(kids_df%>%
                   select(father_id, mother_id, kid_id, birth_year)%>%
                   distinct()%>% #that is the reason why join is needed
                   group_by(father_id, mother_id)%>%
                   mutate(kid_order=dense_rank(birth_year))%>%
                   ungroup(),
                 
                 by=c("father_id"="father_id", 
                      "mother_id"="mother_id", 
                      "kid_id"="kid_id",
                      "birth_year"="birth_year"  
                    )
      )
  
  
  
  time_print("finished")
  ###############################################################################
  #for those parents who once reported about their education, but then were too shy
  time_print("filling education status for those who once reported it, but then were too shy")
  parents_diplomas <- 
    ind_df%>%
    group_by(idind)%>%
    mutate(univ_diploma=univ_diploma%>%
             coalesce(0)%>%
             max()
    )%>% 
    ungroup()%>%
    filter(!is.na(univ_diploma))%>%
    select(idind, year, univ_diploma)
  time_print("finished")
  
  
  time_print("pre_fin df formation") 
  kids_df <- 
    ind_df%>%
      filter(!is.na(univ_diploma))%>%
      select(idind, year, univ_diploma, h5, age, profession_code, origsm)%>%
      mutate(h5=ifelse(h5==1, "male", "female"))%>%
      rename(kid_univ_diploma=univ_diploma,
             gender=h5,
             year_i=year #nedeed for closest() in the following join
      )%>%
      inner_join(kids_df,
                 join_by(idind==kid_id, closest(year_i>=year))
                 
      )%>%
      select(-year)%>%
      rename(kid_id=idind)%>% 
      left_join(parents_diplomas%>%
                   rename(father_univ_diploma=univ_diploma),
                 join_by(father_id==idind, closest(year_i>=year))
                 
      )%>%
      select(-year)%>%
      left_join(parents_diplomas%>%
                   rename(mother_univ_diploma=univ_diploma),
                 join_by(mother_id==idind, closest(year_i>=year))
      )%>%
      select(-year)%>%
      filter(!is.na(father_univ_diploma) & !is.na(mother_univ_diploma))%>%
      inner_join(res_df,
                 by=c("id_w"="id_w", "id_h"="id_h")           
      )%>%
      rename(year=year_i)%>%
    # filter(!is.na(totexpr))%>%
      ungroup()%>%
      # inner_join(house_df%>%
      #              select(id_w, id_h, family_size),
      #            by=c("id_w"="id_w", "id_h"="id_h")           
      # )%>%
      group_by(year, id_w, id_h, father_id, mother_id)%>%
      mutate(kids_gender_list=list(gender))%>%
      ungroup()%>%
      rowwise()%>%
      mutate(other_gender_siblings=ifelse(ifelse(gender=="male", "female", "male") %in% kids_gender_list,
                                                 "1", 
                                                 "0"
                                   )%>%
                                   as.factor()
      )%>%
      ungroup()
  time_print("finished") 
  
  kids_df
}
