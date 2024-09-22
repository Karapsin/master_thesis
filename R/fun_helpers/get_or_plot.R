source("R\\libraries.R")
source("R\\fun_helpers\\helpers.R")
get_or_plot <- function(df,
                        dependent,
                        show_explanatory,
                        hide_explanatory,
                        optional_variables, #categorical variables which are used only if there are more than 1 level in the df (not shown on the graph),
                        confint_type,
                        table_text_size,
                        title_text_size 
               ){

  factorlist <- summary_factorlist(df,
                                   dependent, 
                                   show_explanatory,
                                   total_col = TRUE,
                                   fit_id = TRUE
  )
  
  vars <- c(show_explanatory, hide_explanatory, optional_variables[check_for_optional_vars(df, optional_variables)])
  
  df%>%
    or_plot(dependent, 
            vars,
            remove_ref=TRUE,
            factorlist = factorlist,
            breaks = c(1, 2, 3, 4),
            confint_type=confint_type,
            table_text_size=table_text_size,
            title_text_size =title_text_size 
    )
}
