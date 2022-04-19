
valid$leave_pct <- valid$Leave/valid$turnout

# Validation function

validate_vote <- function(vote_model) {
  
  ps_frame$pred_leave <- NA
  
  ps_frame$pred_leave[which(ps_frame$ward %in% validation_wards)] <- 
    predict(vote_model,
            newdata=ps_frame[which(ps_frame$ward %in% validation_wards),],
            type="response",allow.new.levels=TRUE)
  
  wards_sum <- ps_frame[which(wards$ward %in% validation_wards),] %>% 
    group_by(ward) %>%
    summarise(pop = sum(value),
              turnout = sum(voters),
              pred_leave_pct = weighted.mean(pred_leave,voters))
  
  wards_sum$true_leave_pct <- valid$leave_pct[match(wards_sum$ward,valid$WardCode)]
  
  val_mse <- mean((wards_sum$true_leave_pct-wards_sum$pred_leave_pct)^2,na.rm=TRUE)
  
  train_AIC <- AIC(vote_model)
  
  print(paste(" Validation Wards MSE: ", round(val_mse,3), " "))
  print(paste(" MODEL AIC: ", round(train_AIC,0)," "))
  
  return(c(val_mse,train_AIC))
  
}

# Function to find next variable

#FUnction to pick next variable

find_next_vote <- function(vote_model){
  
  poss_variables <- colnames(bes[which(bes$p_eurefvote<999),c(405:406,409:423,426:427,431,438:458,460:463,469:477,481,483:487)])
  
  to_test <- poss_variables[!poss_variables %in% variables_included]
  
  test_scores <- c()
  
  for (variable in to_test){
    
    test_formula <- paste("p_eurefvote ~ fitted.values(vote_model) + ",variable)
    
    test_model <- glm(test_formula,data=bes[which(bes$p_eurefvote<999),])
    
    test_scores <- append(test_scores,AIC(test_model))
    
  }
  
  return(to_test[test_scores==min(test_scores)])
  
}

# Loop

variables_included <- c("1 ")

formulas <- c()
aic_scores <- c()
mse_scores <- c()
model_no <- c()

for (i in 0:60){
  
  model_no <- append(model_no, i)
  print(paste(" MODEL NUMBER:",i,""))
  
  slopes <- paste(variables_included,collapse="+")
  
  fes_included <- variables_included
  fes_included[1] <- " "
  
  fe <- paste(fes_included,collapse="+")
  
  formula <- paste("p_eurefvote ~ (1|msoa) + (1|pcon) + (1|la) +
                   (1|agegroup:edlevel:rgn) +
                   (1|msoa:agegroup) + (1|msoa:edlevel) +
                   (1|la:agegroup) + (1|la:edlevel) +
                   (",slopes,"|rgn) +
                   (",slopes,"|la) +
                   (",slopes,"|edlevel) +
                   (",slopes,"|agegroup)",
                   fe,sep="")
  
  formulas <- append(formulas,formula)
  
  vote_model <- glmer(formula,data=bes[which(bes$p_eurefvote<999),],
                      family="binomial",
                      control = glmerControl(calc.derivs = FALSE,
                                             optimizer="bobyqa"))
  
  
  
   mse_aic <- validate_vote(vote_model)
  
   mse_scores <- append(mse_scores,mse_aic[1])
   aic_scores <- append(aic_scores,mse_aic[2])
     
   new_variable <- find_next(turnout_model)
     
   variables_included <- append(variables_included,new_variable)
     
   print(paste(" NEW VARIABLE ADDED:",new_variable,""))
 
   if (i > 0){
     
     par(mar = c(5, 4, 4, 4) + 0.3)
     plot(model_no,aic_scores)
     lines(model_no,aic_scores,col="red")
     par(new=TRUE)
     plot(model_no,mse_scores,axes=FALSE,xlab="",ylab="")
     lines(model_no,cor_scores,col="green")
     axis(side=4,at=pretty(range(cor_scores)))
     abline(v=model_no[which(aic_scores==min(aic_scores))],lty=2)
     abline(v=model_no[which(cor_scores==min(mse_scores))],lty=3)
     
   }
   
   results <- data.frame(model_no,
              formulas,
              aic_scores,
              mse_scores)
   
   write.csv(results,"model/vote_models.csv")
   
 }
   
   
   