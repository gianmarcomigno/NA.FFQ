##### Compare different imputing methods
# Imputing methods with: none, 0, mean, median, mode 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
df %>%
  mutate_at(vars(19:98), ~replace(., is.na(.), 0))                                   # 0 imputation
  mutate_at(vars(19:98), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))             # mean imputation
  mutate_at(vars(19:98), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))           # median imputation
  mutate_at(vars(19:98), ~ifelse(is.na(.x), Mode(.x), .x))                           # mode imputation
  #write.csv(.,file = "~/df_0_imp.csv")                                              # save different .cvs files
  
# MICE
df_red <- df %>% select(-c(id,walk91, walk93, walk95, walk97, age_bin, bmi_bin, charlsonA)) # Exclude redundant variables
# Start imputation (took 2h)
imp <- mice(df_red, maxit=0)
imp2 <- mice(df_red, maxit = 5,  # 5 datasets
             predictorMatrix = imp$predictorMatrix, method = imp$method, print =  T) # print process
imp2 <- readRDS("dataset_reduced_imputed.rds")
# Get the 5 imputed dataset (change action=1--5 and name)
complete(imp2, action = 1, include = FALSE) %>% cbind(df$id, .) %>% rename(., lopnr = 'df$id') %>% write.csv(., "df_mice_1_imp.csv", row.names = F)

# KNN: https://pubmed.ncbi.nlm.nih.gov/17601360/
knn_df <- DMwR::knnImputation(df_red)
knn_df %>% cbind(df$id, .) %>% rename(., lopnr = 'df$id') %>% write.csv(., "df_knn_imp.csv", row.names = F)

# EM algorithm
# + simulation http://juliejosse.com/wp-content/uploads/2018/07/LectureNotesMissing.html#12)_two_recommended_methods:_em__multiple_imputation 
# EM algorithm code: https://www.r-bloggers.com/2013/07/imputing-missing-data-with-expectation-maximization/ 
EMalg <- function(x, tol=.001){
  missvals <- is.na(x)
  new.impute<-x
  old.impute <- x
  count.iter <- 1
  reach.tol <- 0
  sig <- as.matrix(var(na.exclude(x)))
  mean.vec <- as.matrix(apply(na.exclude(x),2,mean))
  while(reach.tol != 1) {
    for(i in 1:nrow(x)) {
      pick.miss <-( c( missvals[i,]) )
      if ( sum(pick.miss) != 0 ) {
        inv.S <- solve(sig[!pick.miss,!pick.miss]) # we need the inverse of the covariance
        # Run the EM
        new.impute[i,pick.miss] <- mean.vec[pick.miss] +
          sig[pick.miss,!pick.miss] %*%
          inv.S %*%
          (t(new.impute[i,!pick.miss])- t(t(mean.vec[!pick.miss])))
      }
    }
    sig <- var((new.impute))
    mean.vec <- as.matrix(apply(new.impute,2,mean))
    if(count.iter > 1){ # we don't want this to run on the first iteration or else if fails
      for(l in 1:nrow(new.impute)){
        for(m in 1:ncol(new.impute)){
          if( abs((old.impute[l,m]-new.impute[l,m])) > tol ) {reach.tol < - 0} else {reach.tol <- 1}
        }
      }
    }
    count.iter <- count.iter+1 # used for debugging purposes to ensure process it iterating properly
    old.impute <- new.impute
  }
  return(new.impute)
}

df_red <- df %>% select(-c(id,walk91, walk93, walk95, walk97, age_bin, bmi_bin, charlsonA, na_sum)) # remove also na_sum for singularity problems
EM_df <- EMalg(df_red)
# replace negative (~5k) values with 0s
EM_df[EM_df<0] <- 0
EM_df %>% cbind(df$id, .) %>% rename(., lopnr = 'df$id') %>% write.csv(., "df_EM_imp.csv", row.names = F)
