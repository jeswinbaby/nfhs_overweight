library(dplyr)
library(purrr)
retrieve_label <- function(vec, var_name,
                           overweight_analysis_labels = NULL,
                           domain_name = NULL,...){

  existing_var_label <- attr(vec,"label")

  if(!is.null(existing_var_label)){
    print(paste0("Variable ",var_name,": Using default label (class 'labelled')"))
    return(existing_var_label)
  }

  # Read cgm labels dataframe
  if(is.null(overweight_analysis_labels)){
    overweight_analysis_labels_file <- "overweight_analysis_variables.xlsx"
    print(paste0("Using default if no existing variable labels: ",overweight_analysis_labels_file))

    overweight_analysis_labels <- readxl::read_excel("documentation/overweight_analysis_variables.xlsx"
                                                     , sheet="variable_labels")
  }



  if(is.null(existing_var_label)){
    # # Need to build in error catch: Possible that domain might not be specified
    # domain_labels <- overweight_analysis_labels %>%
    #   dplyr::filter(domain %in% domain_name)


    if(!is.null(domain_name)){
      var_label <- overweight_analysis_labels %>%
        dplyr::filter(domain == domain_name,variable == var_name) %>%
        dplyr::select(label) %>%
        pull()


      if(length(var_label)==0){
        print(paste0("Variable ",var_name,": No label found in domain- ",domain_name))
        return(NA)
      }

    }

    if(is.null(domain_name)){
      var_label <- overweight_analysis_labels %>%
        dplyr::filter(variable == var_name) %>%
        dplyr::select(label) %>%
        pull()

      if(is.null(var_label)){
        print(paste0("Variable ",var_name,": No label found"))
        return(NA)
      }


    }

    if(length(var_label)>1){
      print(paste0("Variable name ",var_name,
                   " has ",length(var_label),
                   " occurences. Returning 1st occurence"))
    }

    if(length(var_label)==1){
      print(paste0("Variable name ",var_name,
                   ": ",var_label))
    }

    return(var_label[1])


  }
}



label_variables <- function(df,domain_name = NULL,...){
  # Add variable labels if not already present
  df_labelled <- data.frame()
  df_labelled <- map_dfc(.x=colnames(df),
                     .f = function(x){
                       labelled::var_label(df[[x]]) <- retrieve_label(vec=df[[x]],
                                                                      var_name = x,
                                                                      domain_name=domain_name);
                       return(df[[x]])
                     }
    )
  names(df_labelled) <- names(df)

  return(df_labelled)

  }


label_factor <- function(vec,
                         var_label = NULL,
                         levels_vec=NULL,
                         labels_vec=NULL){

  var_label <- var_label

  existing_var_label <- if(!is.null(attr(vec,"label"))){attr(vec,"label")} else{NULL}

  vec_stata <- vec

  if(!is.null(levels_vec) & !is.null(labels_vec)){
    vec_stata <- factor(vec,levels=levels_vec,
                         labels=labels_vec)
    # Hmisc::label(vec_factor) <- var_label

  }

  if(is.null(var_label)) {var_label <- existing_var_label}

  labelled::var_label(vec_stata) <- var_label
  return(vec_stata)
}

