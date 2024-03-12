
predcvglmnet=function(xtrain,ytrain,k=10,alpha=1)
{
  # xtrain=matrix of predictors
  # ytrain=vector of target (0-1)
  # k= # folds in CV
  # alpha=alpha parameter in glmnet
  
  # value: the CV predicted probabilities
  
  library(glmnet)
  set.seed(375869)
  n=nrow(xtrain)
  pred=rep(0,n)
  per=sample(n,replace=FALSE)
  tl=1
  for(i in 1:k)
  {
    tu=min(floor(tl+n/k-1),n)
    if(i==k){tu=n}
    cind=per[tl:tu]
    fit = cv.glmnet(xtrain[-cind,], 
                    ytrain[-cind], 
                    family="binomial", alpha=alpha
    )
    
    pred[cind]=predict(fit,new=xtrain[cind,],s="lambda.1se", type="response")
    tl=tu+1
  }
  pred
}
bestcutp=function(predcv,y,gainmat=diag(2),cutp=seq(0,1,.02),plotit=FALSE)
{
  # predcv = vector of predicted probabilities (obtained out-of-sample by CV for example)
  # y = vector of target (0-1)
  # gainmat = gain matrix (2X2)  (we want to maximize the gain)
  #	(1,1) = gain if pred=0 and true=0
  #	(1,2) = gain if pred=0 and true=1
  #	(2,1) = gain if pred=1 and true=0
  #	(2,2) = gain if pred=1 and true=1
  # cutp=vector of thresholds to try
  # plotit=plot or not the results
  
  # value: a list with 
  #		1) matrix giving the thresholds and estimated mean gains 	
  #		2) the threshold with maximum gain, with the associated mean gain
  
  nc=length(cutp)
  gain=rep(0,nc)
  for(i in 1:nc)
  {
    pred=as.numeric(predcv>cutp[i])
    gain[i]=mean(gainmat[1,1]*(pred==0)*(y==0)+gainmat[1,2]*(pred==0)*(y==1)+
                   gainmat[2,1]*(pred==1)*(y==0)+gainmat[2,2]*(pred==1)*(y==1))
  }
  if(plotit){plot(cutp,gain,type="l",xlab="threshold",ylab="gain")}
  out=list(NULL,NULL)
  out[[1]]=cbind(cutp,gain)
  out[[2]]=out[[1]][which.max(gain),]
  out
}

set.seed(234) # Reproducibility
stage1_split <- initial_split(intersections_df, prop = 3/4, strata = acc_bin)
stage1_train <- training(stage1_split)
stage1_test <- testing(stage1_split)

lm_rec <- 
  recipe(acc_bin ~., data = stage1_train) |> 
  step_rm(acc) |> 
  # Imputing missing observations with nearest observations
  step_impute_knn(land_use, neighbors = 3, impute_with = imp_vars(x, y)) |> 
  # Create roles to keep certain information for post prediction analysis
  update_role(c(int_no, x, y), new_role = "ID") |> 
  add_role(contains(variables$geometry_data), new_role = "geometry_data") |> 
  add_role(contains(variables$traffic_data), new_role = "traffic_data") |>
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) |> 
  # May differ for boosting
  step_dummy(all_factor_predictors(), one_hot = FALSE) |> 
  step_nzv(all_predictors()) |> 
  step_unknown(all_nominal_predictors(), new_level = "NA") |> 
  step_poly(has_role("traffic_data"), degree = 2)

stage1_prep <- 
  lm_rec |> 
  prep() |> 
  bake(new_data = NULL)

library(glmnet)
tictoc::tic()
set.seed(234)
stage1_LASSO <- 
  cv.glmnet(as.matrix(select(stage1_prep, -c(acc_bin, int_no:y))), 
            stage1_prep$acc_bin,
          family = "binomial", 
          nfolds = 10,
          alpha = 1)

tictoc::toc()


tictoc::tic()
set.seed(234)
stage1_dCVLASSO <- predcvglmnet(as.matrix(select(stage1_prep, -c(acc_bin, int_no:y))), 
                                stage1_prep$acc_bin, 
                                k=10, 
                                alpha=1
                                )
tictoc::toc()





bestcutp(stage1_dCVLASSO, stage1_prep$acc_bin, 
         gainmat = matrix(c(1, -5, 0, 10), nrow = 2), cutp = seq(0,1,.02), plotit = TRUE)



test_phat <- 
  predict(stage1_LASSO, 
        newx = as.matrix(bake(prep(lm_rec), new_data = stage1_test) |> 
                                         select(-c(acc_bin, int_no:y))), 
        s = "lambda.1se", type="response")|> 
  as_tibble() |> 
  mutate(pred_class = if_else(lambda.1se<=0.48, factor("0"), factor("1")),
         truth = stage1_test$acc_bin)

full_phat <- 
  predict(stage1_LASSO, 
          newx = as.matrix(bake(prep(lm_rec), new_data = intersections_df) |> 
                             select(-c(acc_bin, int_no:y))), 
          s = "lambda.1se", type="response")|> 
  as_tibble() |> 
  mutate(int_no = intersections_df$int_no,
         pred_class = if_else(lambda.1se<=0.48, factor("0"), factor("1")),
         acc_bin = intersections_df$acc_bin) |> 
  select(int_no, acc_bin, pred_prob = lambda.1se, pred_class)

write_parquet(full_phat, "./output/stage1_pred.parquet")

# Extra ----
coef_stage1 <- 
  coef(stage1_LASSO, s = "lambda.1se") |> 
  as.matrix() |> 
  as_tibble(rownames = "term") |>
  filter(term != "(Intercept)") |>
  mutate(selected = if_else(s1 == 0, "No", "Yes")) |>
  select(term, selected)
p1 <- stage1_LASSO  |> 
  vip::vi() |> 
  filter(Importance != 0) |> 
  ggplot(aes(Variable, Importance, color = Sign)) +
  geom_point()+
  coord_flip() + 
  theme_minimal() +
  ggtitle("Variables selected in Stage 1 (coefficient size and sign)")
