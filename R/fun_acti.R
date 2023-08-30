# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# pkg <- c(
#   'dplyr', 'tidyr'#, 'purrr' #Data wrangling
#   , 'atlas.skew' #Bounded variable skewness
#   , 'atlas.ftools' #Factor analysis tools
#   , 'atlas.eqvl' #Equivalence
#   , 'vctrs' #Data frame subclasses
# )
# 
# # Activate / install packages
# lapply(pkg, function(x)
#   if(!require(x, character.only = T))
#   {install.packages(x); require(x)})
# 
# # Package citation
# # lapply(pkg, function(x)
# #   {citation(package = x)})

# [FUNCTIONS] ---------------------------
# - Generalism function ---------------------------------------------------
fun_acti_generalism <- function(
    dbl_profile
    , dbl_scale_lb = 0
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be numeric." =
      is.numeric(dbl_profile)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  # Drop NAs
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile
  
  # Apply bounded variable skewness function
  fun_skew_sdmode(
    dbl_var =
      dbl_profile
    , dbl_scale_lb =
      dbl_scale_lb
    , dbl_scale_ub =
      max(dbl_profile)
  ) -> dbl_generalism
  
  rm(dbl_profile)
  
  # Output
  return(dbl_generalism)
  
}

# - Indispensability function ---------------------------------------------
fun_acti_indispensability <- function(
    dbl_profile
    , dbl_scale_lb = 0
    , dbl_generalism = NULL
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be a numeric." =
      is.numeric(dbl_profile)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_generalism' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generalism)
        , is.null(dbl_generalism)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  if(is.null(dbl_generalism)){
    
    fun_acti_generalism(
      dbl_profile
      , dbl_scale_lb =
        dbl_scale_lb
    ) -> dbl_generalism
    
  }
  
  dbl_generalism[[1]] -> dbl_generalism
  
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile
  
  # Equivalence of normalized scores
  fun_eqvl_equivalence(
    dbl_var =
      dbl_profile
    , dbl_scale_lb =
      dbl_scale_lb
    , dbl_scale_ub =
      max(dbl_profile)
    , dbl_scaling =
      1 - dbl_generalism
  ) -> dbl_indispensability
  
  if(is.matrix(dbl_profile)){
    
    colnames(dbl_profile) ->
      names(dbl_indispensability)
    
  } else {
    
    names(dbl_profile) ->
      names(dbl_indispensability)
    
  }
  
  # Output
  return(dbl_indispensability)
  
}

# - Competency function ---------------------------------------------------
fun_acti_competency <- function(
    dbl_profile
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , dbl_generalism = NULL
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be numeric." =
      is.numeric(dbl_profile)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_generalism' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generalism)
        , is.null(dbl_generalism)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  if(is.null(dbl_generalism)){
    
    fun_acti_generalism(
      dbl_profile
      , dbl_scale_lb =
        dbl_scale_lb
    ) -> dbl_generalism
    
  }
  
  dbl_generalism[[1]] -> dbl_generalism
  
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile
  
  # Weighted mean of normalized item scores
  # adjusted by item importance and generalism
  weighted.mean(
    x =
      dbl_profile / (
        dbl_scale_ub -
          dbl_scale_lb
      ) -
      dbl_scale_lb / (
        dbl_scale_ub -
          dbl_scale_lb
      )
    , w =
      fun_acti_indispensability(
        dbl_profile = 
          dbl_profile
        , dbl_scale_lb = 
          dbl_scale_lb
        , dbl_generalism =
          dbl_generalism
      )
  ) -> dbl_competency
  
  # Output
  return(dbl_competency)
  
}

# - Classifier function -------------------------------------------------
fun_acti_classifier <- function(
    dbl_var
    , dbl_scale_lb = 0
    , dbl_scale_ub = 1
    , int_levels = 5
){
  
  # Arguments validation
  stopifnot(
    "'dbl_var' must be numeric." = 
      is.numeric(dbl_var)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." = 
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." = 
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'int_levels' must be an integer." = 
      is.numeric(int_levels)
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  int_levels[[1]] -> int_levels
  ceiling(int_levels) -> int_levels
  
  # Classify competency level
  findInterval(
    dbl_var
    , seq(
      dbl_scale_lb, 
      dbl_scale_ub, 
      length.out = 1 +
        int_levels
    )
    , all.inside = T
  ) -> int_class_id
  
  names(dbl_var) -> 
    names(int_class_id)
  
  # Output
  return(int_class_id)
  
}

# - Numerical ACTI --------------------------------------------------------
fun_acti_type <- function(
    df_data
    , efa_model
    , chr_factor_labels = NULL
    , chr_data_id = NULL
    , dbl_scale_lb = 0
    , dbl_generalism = NULL
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(df_data)
        , any(
          loadings(efa_model)[,] %>%
            rownames() %in%
            names(df_data)
        )))
  
  stopifnot(
    "'chr_factor_labels' must be either NULL or a character vector with labels for each factor." =
      any(
        is.null(chr_factor_labels)
        , all(
          is.character(chr_factor_labels)
          , length(chr_factor_labels) ==
            efa_model$factors
        )
      )
  )
  
  stopifnot(
    "'chr_data_id' must be either NULL or a character vector with labels for each observation." =
      any(
        is.null(chr_data_id)
        , all(
          is.character(chr_data_id)
          , length(chr_data_id) ==
            nrow(df_data)
        )
      )
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_generalism' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generalism)
        , is.null(dbl_generalism)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  df_data %>%
    select(any_of(
      efa_model$
        model %>%
        colnames()
    )) -> df_data
  
  dbl_generalism[[1]] -> dbl_generalism
  
  if(is.null(dbl_generalism)){
    
    apply(
      df_data, 1
      , fun_acti_generalism
      , dbl_scale_lb =
        dbl_scale_lb
    ) -> dbl_generalism
    
  }
  
  # Factor scores
  fun_ftools_factor_scores(
    df_data =
      df_data
    , efa_model =
      efa_model
    , lgc_pivot = F
  ) -> df_factor_scores
  
  rm(df_data)
  
  # Apply indispensability function
  mapply(
    function(profile, generalism){
      
      # Calculate item indispensability
      fun_acti_indispensability(
        dbl_profile = profile
        , dbl_scale_lb = dbl_scale_lb
        , dbl_generalism = generalism
      ) -> dbl_indispensability
      
      # Output
      return(dbl_indispensability)
      
    }
    , profile = as_tibble(t(
      df_factor_scores
    )) 
    , generalism = dbl_generalism
  ) %>% 
    t() %>%
    as_tibble() -> 
    df_factor_scores
  
  # Name factors
  if(is.null(chr_factor_labels)){
    
    paste0('F', 1:ncol(
      df_factor_scores
    )) -> chr_factor_labels
    
  }
  
  names(
    df_factor_scores
  ) <- chr_factor_labels
  
  rm(chr_factor_labels)
  
  # Sort
  apply(
    df_factor_scores, 1
    , sort
    , decreasing = T
    , simplify = F
  ) -> list_factor_scores
  
  rm(df_factor_scores)
  
  # Classify scores
  lapply(
    list_factor_scores
    , function(x){
      
      fun_acti_classifier(
        dbl_var = x
        , dbl_scale_lb = 0
        , dbl_scale_ub = 1
        , int_levels = 3
      )
      
    }
  ) -> list_classification
  
  # Keep only dominant and auxiliary factors (drop minor)
  lapply(
    list_classification
    , function(x){return(x[x > 1])}
  ) -> list_classification
  
  Map(
    function(factor_scores, factor_class){
      
      # Get dominant and auxiliary factors
      factor_scores[
        names(factor_scores) %in%
          names(factor_class)
      ] -> factor_scores
      
      # Output
      return(factor_scores)
      
    }
    , factor_scores = list_factor_scores
    , factor_class = list_classification
  ) -> list_factor_scores
  
  chr_data_id -> names(list_classification)
  chr_data_id -> names(list_factor_scores)
  chr_data_id -> names(dbl_generalism)
  
  rm(chr_data_id)
  
  # ACTI data frame
  dbl_generalism %>%
    as_tibble(
      rownames = 'occupation'
    ) %>%
    rename(
      generalism = 2
    ) %>% 
    full_join(
      bind_rows(
        list_factor_scores
        , .id = 'occupation'
      ) %>% 
        pivot_longer(
          cols = where(is.numeric)
          , names_to = 'factor'
          , values_to = 'acti_score'
        )
    ) %>%
    full_join(
      bind_rows(
        list_classification
        , .id = 'occupation'
      ) %>% 
        pivot_longer(
          cols = where(is.numeric)
          , names_to = 'factor'
          , values_to = 'class'
        )
    ) -> df_acti
  
  rm(list_factor_scores)
  rm(list_classification)
  rm(dbl_generalism)
  
  df_acti %>% 
    mutate(
      class = 
        case_match(
          class
          , 2 ~ 'Aux'
          , 3 ~ 'Dom'
        )
    ) %>%
    drop_na() %>% 
    group_by(
      occupation
    ) %>% 
    arrange(desc(
      acti_score
    ), .by_group = T
    ) %>% 
    ungroup() -> 
    df_acti
  
  # ACTI type acronym helper function
  fun_acti_type_helper <- function(df_data){
    
    # ACTI type acronym
    df_data %>%
      filter(
        class == 'Dom'
      ) %>%
      pull(factor) %>%
      paste0(
        collapse = '-'
      ) -> chr_dom
    
    df_data %>%
      filter(
        class != 'Dom'
      ) %>%
      pull(factor) %>%
      paste0(
        collapse = '-'
      ) -> chr_aux
    
    if(chr_aux != ''){
      
      paste0(
        '/', chr_aux
      ) -> chr_aux
      
    }
    
    paste0(
      chr_dom
      , chr_aux
    ) -> chr_acti_type
    
    # Output
    return(chr_acti_type)
    
  }
  
  # Calculate ACTI acronyms
  df_acti %>% 
    split(.$occupation) %>% 
    sapply(
      fun_acti_type_helper
    ) %>% 
    as_tibble(
      rownames = 'occupation'
    ) %>%
    rename(
      acti_type = 2
    ) %>% 
    left_join(
      df_acti
    ) -> df_acti
  
  df_acti %>%
    new_data_frame(
      class = c('df_acti', 'tbl')
    ) -> df_acti
  
  # Output
  return(df_acti)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Data ------------------------------------------------------------------
# library(readr)
# 
# read_rds(
#   'C:/Users/Cao/Documents/Github/atlas-research/data/efa/efa_equamax_14factors.rds'
# ) -> efa_model
# 
# read_csv(
#   'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023_efa.csv'
# ) -> df_occupations
# 
# # read_csv(
# #   'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
# # ) -> df_input
# 
# # - Generalism test -------------------------------------------------------
# fun_acti_generalism(
#   dbl_profile =
#     rnorm(50, 50, 25) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
# )
# 
# fun_acti_generalism(
#   dbl_profile =
#     rnorm(50, 50, 5) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
# )
# 
# fun_acti_generalism(
#   dbl_profile =
#     rnorm(50, 50, 0) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
# )
# 
# # - Indispensability test -------------------------------------------------
# fun_acti_indispensability(
#   dbl_profile =
#     rnorm(50, 50, 25) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
# ) %>% round(4)
# 
# # - Competency test -------------------------------------------------------
# fun_acti_competency(
#   dbl_profile =
#     rnorm(50, 100, 25) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
# 
# fun_acti_competency(
#   dbl_profile =
#     rnorm(50, 50, 25) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
# 
# fun_acti_competency(
#   dbl_profile =
#     rnorm(50, 50, 5) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
# 
# fun_acti_competency(
#   dbl_profile =
#     rnorm(50, 50, 0) %>%
#     pmax(0) %>%
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
# 
# # - Numerical ACTI test ---------------------------------------------------
# df_occupations %>%
#   slice_sample(n = 2) ->
#   dsds
# 
# fun_acti_type(
#   df_data = dsds
#   , chr_factor_labels = c(
#     'Ds', 'Eg', 'Hs',
#     'Mn', 'Tr', 'Ad',
#     'So', 'Ah', 'Hz',
#     'An', 'Mt', 'Rb',
#     'In', 'Mc'
#   )
#   , chr_data_id =
#     dsds$occupation
#   , efa_model = efa_model
#   , dbl_scale_lb = 0
# ) %>% print(n = Inf)
