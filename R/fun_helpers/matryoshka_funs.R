source("R\\fun_helpers\\helpers.R")
#run model multiple times ('matryoshka procedure')
#1st one with the 1st element of the 'independent_list' list
#2nd one with 1st and 2nd elements of that list
#etc...

#if element of 'independent_list' list:
# 1) is vector, then all elements of that vector are added
# 2) is list with n elements, then n models are evaluated: 
#    each contains only one element of that list
#    (elements from that list never occurs in one model)
#   
#    after all n steps are finished,
#    we proceed the 'matryoshka procedure' 
#    with the last element of the list


run_all_specifications <- function(df, 
                                   dependent_var, 
                                   independent_list,
                                   optional_variables #categorical variables which are used only if there are more than 1 level in the df
                                   
                          ){
  
  current_independent_vars <- c()
  
  if(length(optional_variables!=0)){
    current_independent_vars <- optional_variables[check_for_optional_vars(df, optional_variables)]
  } 
  
  fit_logit <- function(independent){
    reformulate(independent, dependent_var)%>% #creates formula
      glm(data = df, family = "binomial")%>%
      list()
  }
  

  
  models_list <- list()
  for(i in 1:length(independent_list)){
    
    #not list procedure     
    if(!is.list(independent_list[[i]])){
  
      current_independent_vars %<>% c(independent_list[[i]])
      print(reformulate(current_independent_vars, dependent_var))
      
      models_list %<>%
        append(fit_logit(current_independent_vars)%>%
                 set_names(names(independent_list)[i])
        )

    } 
    
    #list procedure
    else{
      
      elements_num <- length(independent_list[[i]])
      
      #not map because we want to preserve glm class
      for(j in 1:elements_num){
        
        new_vars <- unlist(independent_list[[i]][j])
        print(reformulate(c(current_independent_vars, new_vars), dependent_var))
        
        models_list%<>%
          append(c(current_independent_vars, new_vars)%>%
                    fit_logit()%>%
                    set_names(names(independent_list[[i]][j]))
          )
      }
      
    
      
      #proceeding with the last element of the list
      current_independent_vars %<>% c(unlist(independent_list[[i]][elements_num]))
    }
    
    
    
  }
  
  #output
  models_list
  
}

#runs 'run_all_specifications' fun for df,
#extracts p-values into a table
get_p_values_from_specifications <- function(models_list,
                                             variables_of_interest
                                    ){
  coeffs <- list()
  for(i in 1:length(models_list)){
    
    coeffs[[i]] <- 
      models_list[[i]]%>%
               tidy()%>%
               filter(str_detect(term, 
                                 paste0(variables_of_interest, 
                                        collapse = "|")
                                 )
               )%>%
               data.frame()%>%
               add_row(term="nobs", estimate=glance(models_list[[i]])$nobs)%>%
               mutate(specification=names(models_list)[i])
    
  }
  
  format_str <- function(str){round(str, 3)%>%format()}
  
  coeffs_std_errors <-
    coeffs%>%
      bind_rows()%>%
      mutate(str_desc=ifelse(term!="nobs",
                             paste0(round(estimate, 3), 
                                   " (", 
                                   round(std.error, 3),
                                   ")"
                             ),
                             round(estimate)
                      )
      )%>%
      mutate(str_desc= case_when(p.value<0.001~paste0(str_desc,"***"),
                                 p.value<0.01~paste0(str_desc, "**"),
                                 p.value<0.05~paste0(str_desc, "*"),
                                 TRUE~str_desc
                       )
      )%>%
      select(term, str_desc, specification)%>%
      mutate(specification=paste0("+", str_replace_all(specification, "_", " "))
      )%>%
      pivot_wider(names_from = "specification",
                  values_from = c("str_desc")
      )

   coeffs_std_errors%>%
     arrange(term)%>%
     list()

}
