#Manipulation check is designed for categorizations1_data.csv. It makes sure that every participant
#used the correct pronoun three times. 


read_data <- function(system = "mac"){ #file pahts differ depending on if the computer is a mac or windows
  if(system == "Windows"){
    d <- read.csv("data\\categorization1_data.csv", na.strings = "")
  } else{
    d <- read.csv("data/categorization1_data.csv", na.strings = "")
  }
  
  d <- d  %>%
    mutate(dupe = duplicated(IPAddress)) %>% #Checks if IP adress has occurred previously
    filter(dupe == FALSE) %>%  # removes duplicated IP adresses
    mutate( across(starts_with("ess"),  # changes responses to numerical
                   ~recode(.,"Stammer inte alls\n1" ="1",
                           "Stammer helt\n7" = "7") %>% as.integer()),
            across(starts_with("hen_attitudes"),
                   ~recode(.,"Mycket bra\n7" ="1",
                           "Mycket daligt\n1" = "7") %>% as.integer()),
            across(starts_with("GIS"),  # changes responses to numerical
                   ~recode(.,"Stammer inte alls\n1" ="1",
                           "Stammer helt\n7" = "7") %>% as.integer()), 
            id =row_number(),
            hen_use = 
              recode(hen_use,
                     "Aldrig" = 1,
                     "Nagon gang om aret" = 2,
                     "Nagon gang i manaden" = 3,
                     "Nagon gang per vecka" = 4,
                     "Dagligen" = 5) %>% as.integer(),
            hen_hear = 
              recode(hen_hear,
                     "Aldrig" = 1,
                     "Nagon gang om aret" = 2,
                     "Nagon gang i manaden" = 3,
                     "Nagon gang per vecka" = 4,
                     "Dagligen" = 5) %>%  as.integer()) %>% 
    rename(id_feminist = essentialism_7 ,id_lgbtq = essentialism_8)
  
  d <- d[-c(1:2),]
}

manipulation_check <- function(d){
  #manipulation check
  conditions <- c("hen1", "hen2", "hen3", "han1", "han2", "han3", "hon1", "hon2", "hon3", "control1", "control2", "control3")

  #First, create a column called condition in d with the first three letters in each of
  #the three sentences
  d_conditions <- apply( d[,conditions], 2 , substr, start = 1, stop = 3 ) 
  d_conditions <-  t(apply(d_conditions, 1, function(x) c(x[!is.na(x)], x[is.na(x)]))) %>%
    data.frame() %>% select(1:3)
  d$condition_tmp <- paste(d_conditions[,1], d_conditions[,2], d_conditions[,3], sep = "") %>%
    tolower()
  
  #then check if they used the same pronoun in the all three sentencs AND if they weren't
  #in the control condition. This involves creating a function within the function, doublecheck.
  d$completed <- d$condition_tmp == "henhenhen"|d$condition_tmp == "hanhanhan"| d$condition_tmp == "honhonhon"
  doublecheck <- function(x){
    ifelse(d$completed == FALSE, grepl(x, d$condition_tmp), F) 
  }
  doublechecks <- doublecheck("han") == T & doublecheck("hon") == T & doublecheck("hen") == T
  d$condition <- ifelse(d$completed == F, "control", d$condition_tmp) %>% 
    ifelse(doublechecks == T, NA, .) %>% 
    recode(., "henhenhen" = "hen", "honhonhon" = "hon", "hanhanhan" = "han")
  
  for (i in c("hen", "na", "han", "hon")){
    d$condition <- ifelse(d$completed == F & grepl(i, d$condition_tmp) == T, NA, d$condition ) 
  }
  return(d)
}

#standardize a variable
standardize <- function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

#bootstrap the difference between two means. Takes two vectors of numbers and produces
#bootstrapped means and 95% uncertainty intervals.
bootstrap_diff <- function(d1, d2) {
  boot1 <- replicate(1e5, mean(sample(d1, length(d1), replace = TRUE), na.rm = TRUE ) )
  boot2 <- replicate(1e5, mean(sample(d2, length(d2), replace = TRUE), na.rm = TRUE ) )
  diff <- boot1 - boot2
  c(mean(diff), quantile(diff, prob = c(0.025, 0.975)))
}


analysis_non_par <- function(d){
  #First, a function to simply bootstrap a mean for one column of data
  boostrap_mean <- function(d) {
    dd <- d[sample(1:nrow(d), size = nrow(d), replace = TRUE),]
    mm <- apply(dd, 2, mean, na.rm = TRUE)
    mm
  }
  # Do bootstrap
  dmean <- replicate(1e3, boostrap_mean(d[, cols]))
  
  # Calculate mean values and 95 % CIs
  m <- apply(d[,cols], 2, mean, na.rm = TRUE)
  cimean <- apply(dmean, 1, quantile, probs = c(0.025, 0.975))
  
  # Pack output in matrix called mean_data
  mean_data <- rbind(m, cimean)
  rownames(mean_data) <- c('mean', '95%lo', '95%hi')
  
  # Display output, rounded to 2 decimals
  round(mean_data, 2)
}

get_hdi <- function(fit) {
  
  fit %>% 
    as_draws_df() %>% 
    transmute(hen = inv_logit_scaled(b_conditionhen),
              han = inv_logit_scaled(b_conditionhan)) %>% 
    mutate(diff = hen-han) %>% 
    # yields the highest-density *continuous* interval
    mode_hdci() %>% 
    select(contains(".lower")|contains(".upper")) 
  
}
