rm(list=ls())
gc()
options(scipen = 999)
source("R\\data_load\\sav_funs.R")
source("R\\data_load\\read_source_data.R")
source("R\\data_load\\transform_source_data.R")
source("R\\data_load\\final_data_clean.R")
source("R\\fun_helpers\\matryoshka_funs.R")
source("R\\fun_helpers\\get_or_plot.R")

do_sav_csv_conversion(FALSE)

#can be done via one pipe chain, 
#however I prefer to leave it like this for debugging purposes
source_data <- read_source_data()
transformed_source <-  
  source_data%>%
    rlang::exec(transform_source_data, !!!.)

df <- 
  transformed_source%>%
    final_data_clean(lower_age=22, 
                     upper_age=250,
                     kids_num_trunc=4,
                     kid_order_trunc=3
    )%>%
    inner_join(source_data[[1]]%>%
                 select(idind, birth_year)%>%
                 distinct()%>%
                 rename(mother_id=idind, mother_birth_year=birth_year),
               by=join_by(mother_id)
    )%>%
    mutate(mother_birth_year_trunc=mother_birth_year%>%`/`(10)%>%floor()%>%`*`(10)%>%as.factor())

save(df, file="RData\\df_sample.RData")

#categorical variables which are used with fun if and only if 
#there are more than 1 level in the df
optional_variables <- c("is_separated", "birth_year_trunc")



# ###########################################################
# #different specifications robustness
# ###########################################################
#all variables with names
add_variables_list <- 
  list(kids_num = "kids_num",
       kid_order="kid_order",
       gender="gender",
       birth_year_trunc="birth_year_trunc",
       parents_education = c("mother_univ_diploma", "father_univ_diploma"),
       other_gender_siblings="other_gender_siblings",
       is_nuclear_family = "nuclear_family",
       residence = "residence",
       lag_expenses = "lag_expenses"
       
  )

robust_table <- 
  list(df,
       df%>%filter(is_separated=="0"),
       df%>%filter(birth_year_trunc %in% c("1970", "1975")),
       df%>%filter(birth_year_trunc %in% c("1980", "1985")),
       df%>%filter(birth_year_trunc %in% c("1990", "1995", "2000"))
  )%>%
    map(~run_all_specifications(df=.x, 
                                dependent_var='univ_diploma', 
                                independent_list=add_variables_list, 
                                optional_variables=c(optional_variables)
         )%>%
        get_p_values_from_specifications(c("kids_num", "kid_order"))%>%
        set_names(c("coeffs"))
    )%>%
    set_names(c("all", "not separated", "1970-1979", "1980-1989", "1990-2000"))

robust_table

save(robust_table, file="RData\\specification_robustness_table.RData")

#??consistent with or plot estimates
# 
glm(reformulate(add_variables_list[1:8]%>%unlist(), "univ_diploma"),
    data = df,
    family = "binomial"
  )%>%
  tidy(conf.int=TRUE)%>%
  mutate(conf.low=exp(conf.low),
         conf.high=exp(conf.high),
         estimate=exp(estimate)
  )

  
glm(reformulate(add_variables_list[1:8]%>%unlist(), "univ_diploma"),
      data = df,
      family = "binomial"
  )%>%
    tidy()%>%
    mutate(estimate=exp(estimate))%>%
    print(n=100)
 
  
# ###########################################################
# #or plot
# ###########################################################
#or=odds ration
dependent <- "univ_diploma"
show_explanatory <- c("father_univ_diploma",
                      "mother_univ_diploma",
                      "gender",
                      
                      "residence",
                      
                      "birth_year_trunc",
                     
                      "kids_num",
                      "kid_order",

                      "nuclear_family",
                      "other_gender_siblings"
                     # ,"lag_expenses"
)

hide_explanatory <- c()

list(df,
     df%>%filter(is_separated=="0")
)%>%
  map(~get_or_plot(df=.x,
                   dependent=dependent,
                   show_explanatory=show_explanatory,
                   hide_explanatory=hide_explanatory,
                   optional_variables=optional_variables,
                   confint_type="profile",
                   table_text_size = 5,
                   title_text_size = 5 
                   
       )
  )
  

or_plots[1]
or_plots[[1]]
# ###########################################################
# #auc robustness
# ###########################################################
#train, test
get_metrics <- function(df, current_prop, optional_variables, n){
  
  get_values <- function(){
                            df%<>%filter(residence!="SPB")
                            
                            source("R\\fun_helpers\\helpers.R")
                            df_split <- initial_split(df, prop=current_prop)
                            
                            vars <- add_variables_list[1:6]%>%
                                      unlist()
                            
                            #vars <- "kids_num"
                            
                            rec <- 
                              vars%>%
                              reformulate("univ_diploma")%>%
                              recipe(data=training(df_split))
                            
                            logistic_reg() %>%
                              set_engine(engine = "glm")%>%
                              set_mode('classification')%>%
                              last_fit(rec, 
                                       split = df_split,
                                       metrics=metric_set(accuracy, recall, precision, roc_auc)
                              )%>%
                              collect_metrics()%>%
                              select(.metric, .estimate)
  }
  
  output <- NULL
  attempt <- 1
  while( is.null(output) && attempt <= n ) {
    attempt <- attempt + 1
    try(
      output <- get_values()
    )
  } 
  
  output
  
}

get_auc_df <- function(df, label, lower, upper, step, times, n){
  
  auc_test <- 
    seq(lower, upper, step)%>%
      map_df(function(x){map_df(1:times, 
                                  ~get_metrics(df, x, optional_variables, n)
                            )%>%
                              group_by(.metric)%>%
                              summarise(avg_estimate=mean(.estimate))%>%
                              mutate(prop=x)
                              
              }
      )
  
  auc_test%>%
    mutate(type=label)
}

#threshold 0.5 is used 
auc_df <- 
  map2_df(list(df),
          c("All"),
          ~get_auc_df(.x, 
                      .y, 
                      lower=0.25, 
                      upper=0.75, 
                      step=0.01,
                      times=10,
                      n=300
           )
          
  )

save(auc_df, file="RData\\auc_df.RData")

auc_df%>%
  ggplot(aes(x=prop, y=avg_estimate))+
    facet_grid(~.metric)+
    geom_line(size=2,  color="darkblue")+
    #geom_label(aes(label=round(avg_estimate, 3)), fill="white", color="black", size=4)+
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.1) )+
    scale_y_continuous(breaks = seq(0.65, 1, 0.05))+
    labs(x="", y="")+
    theme(legend.title = element_blank(),
          legend.position = c(0.05, 0.85),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(size=14, color="black"),
          axis.text.y = element_text(size=14, color="black"),
          strip.text = element_text(size=14, color="black")
      
    )


# ###########################################################
# #sample vs total
# ###########################################################

total_df <- 
  source_data[[1]]%>%
    filter(age>=22 & birth_year>=1970)%>% 
    mutate(gender=ifelse(h5==1,"male", "female"),
           prof=case_when(profession_code==-1 ~ "unemployed",
                          profession_code<6 & profession_code!=0 ~ "qualified", 
                          TRUE ~ "not qualifed"
                ),
           univ_diploma=coalesce(univ_diploma, 0)
    )%>%
    select(year, idind, id_i, id_h, id_w, age, gender, univ_diploma, prof, origsm, inwgt)%>%
    group_by(idind)%>%
    filter(year==min(year))%>%
    ungroup()%>%
    inner_join(source_data[[3]]%>%
                select(id_w, id_h, residence),
              by=c("id_w"="id_w", "id_h"="id_h")          
    )%>%
    left_join(source_data[-3]%>%
                rlang::exec(create_relatives_list, !!!.)%>%
                select(id_w, id_h, kid_id_ind, relatives_list)%>%
                distinct(),
              by=join_by(id_w, id_h, idind==kid_id_ind)
    )%>%
    rowwise()%>%
    mutate(is_separated = ifelse(4 %in% relatives_list, "0", "1"),
           is_full_family_and_not_separated = ifelse(length(which(relatives_list==4))==2, "1", "0")       
    )%>%
    ungroup()

save(total_df, file="RData\\df_total.RData")

get_shares_by_2_groups <- function(df, group_var_0, group_var, id_var){
  
  group_var_0 <- ensym(group_var_0)
  group_var_0_str<- rlang::as_string(group_var_0)
  
  group_var <- ensym(group_var)
  group_var_str<- rlang::as_string(group_var)
  
  id_var <- enquo(id_var)
  
  if(group_var_str!=group_var_0_str){
  
  df%>%
    group_by(!!group_var, !!group_var_0)%>%
    summarise(size=length(unique(!!id_var)))%>%
    group_by(!!group_var)%>%
    mutate(share=size/sum(size)%>%coalesce(0))%>%
    select(-size)%>%
    pivot_wider(names_from=group_var_0_str, values_from = "share")
    
  } else{
    
  df%>%
    group_by(!!group_var_0)%>%
    summarise(size=length(unique(!!id_var)))%>%
    mutate(share=size/sum(size)%>%coalesce(0))%>%
    select(-size)%>%
    pivot_wider(names_from=group_var_0_str, values_from = "share")
  }
}

get_sample_total_diff <- function(total_df, df, group_var_0, group_var){
  
  group_var_0 <- ensym(group_var_0)
  group_var_0_str<- rlang::as_string(group_var_0)
  
  group_var <- ensym(group_var)
  group_var_str<- rlang::as_string(group_var)
  
  col_num_1 <- ifelse(group_var_str!=group_var_0_str, 2, 1)
  col_num_2 <- ifelse(group_var_str!=group_var_0_str, 3, 2)
  
  df_total <- 
    get_shares_by_2_groups(df=total_df, 
                           group_var_0=!!group_var_0,
                           group_var=!!group_var, 
                           id_var=idind
        )%>%
        arrange(!!group_var)
      
  df_sample <- 
    get_shares_by_2_groups(df=df, 
                           group_var_0=!!group_var_0, 
                           group_var=!!group_var, 
                           id_var=kid_id
      )%>%
      arrange(!!group_var)
    
  res <- df_total[group_var_str]
  
  for(col_name in names(df_total)){
    
    if(col_name!=group_var_str){
      
      df_sample[col_name] <- coalesce(df_sample[col_name]%>%unlist(), 0)
      df_total[col_name] <- coalesce(df_total[col_name]%>%unlist(), 0)
      
      res[col_name] <-  df_sample[col_name] - df_total[col_name] 
      
    }
  }
  
  
  res
}

get_deviations_df <- function(new_total, new_df){
  get_sample_total_diff(new_total,
                        new_df,
                        residence, 
                        year
  )%>%
    select(year, village)%>%
    inner_join(get_sample_total_diff(new_total,
                                     new_df,
                                     univ_diploma, 
                                     year
    )%>%
      select(year, `0`)%>%
      rename(no_diploma=`0`),
    by=c("year"="year")
    )%>%
    inner_join(get_sample_total_diff(new_total,
                                     new_df, 
                                     gender,
                                     year
    )%>%
      select(year, male),
    by=c("year"="year")
    )%>%
    mutate(period=case_when(between(year, 1990, 1999)~"1994-1999",
                            between(year, 2000, 2004)~"2000-2004",
                            between(year, 2005, 2009)~"2005-2009",
                            between(year, 2010, 2014)~"2010-2014",
                            between(year, 2015, 2018)~"2015-2018",
                            between(year, 2019, 2022)~"2019-2022"
                            
    )
    )%>%
    mutate(period=factor(period, levels = c("1994-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018", "2019-2022")))%>%
    pivot_longer(cols=-c("year", "period"))
}



total_df$univ_diploma%>%unique()

new_total <- 
  total_df%>%
    filter(origsm==1 & age<=26)

#excluding those who are living in full families and with parents which diploma status can not be defined
excl_id <- 
  new_total%>%
    filter(is_full_family_and_not_separated=="1" & !(idind %in% df$kid_id))%>%
    pull(idind)%>%
    unique()

new_df <- 
  df%>%
    filter(age<=26)

deviations_df <- 
  get_deviations_df(new_total, 
                    new_df
  )%>%
    mutate(is_separated="all")%>%
    rbind(get_deviations_df(new_total%>%filter(is_separated=="0" ), 
                            new_df%>%filter(is_separated=="0")
          )%>%
           mutate(is_separated="not_separated")
    )
save(deviations_df, file="deviations.RData")

#########################################################################################
#twins

full_df <- 
  transformed_source%>%
  final_data_clean(lower_age=22, 
                   upper_age=250,
                   kids_num_trunc=300,
                   kid_order_trunc=300
  )

twins_parents <- 
  full_df%>%
    group_by(father_id, mother_id, kid_order)%>%
    count()%>%
    filter(n>1)%>%
    ungroup()%>%
    select(father_id, mother_id)%>%
    distinct()
  

twins_df <- 
  full_df%>%
    inner_join(twins_parents, by=join_by(father_id, mother_id))%>%
    distinct()



###################################################################################

new_df%>%
  select(year, univ_diploma)%>%
  group_by(year)%>%
  summarise(num=sum(ifelse(univ_diploma=="1", 1, 0))/length(year))

new_total%>%
  select(year, univ_diploma)%>%
  group_by(year)%>%
  summarise(num=sum(ifelse(univ_diploma=="1", 1, 0))/length(year))


