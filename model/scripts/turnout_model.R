for (package in c("lme4","readxl")){
  if (!package %in% installed.packages()[,1]){
    install.packages(package)
  }
}

library(lme4)
library(readxl)

wards1 <- ps_frame %>% group_by(ward) %>%
  summarise_at(vars(la:rgn), funs(first(.)))

wards2 <- ps_frame %>% group_by(ward,msoa,pcon,name) %>%
  summarise_at(vars(agegroup,edlevel), funs(first(.)))

wards3 <- ps_frame %>% group_by(ward,msoa,pcon,name) %>%
  summarise_at(vars(value), funs(sum(.)))

wards4 <- ps_frame %>% group_by(ward) %>%
  summarise_at(vars(female_pct:private_rented_pct), funs(weighted.mean(.,w=value)))

wards <- left_join(wards2,wards3,by=c("ward","msoa","pcon","name"))

wards <- reduce(list(wards,wards1,wards4), left_join, by=c("ward"))

# Validation function

validate <- function(turnout_model) {
  
  wards$pred_turnout <- NA
  
  wards$pred_turnout[which(wards$ward %in% validation_wards)] <- 
    predict(turnout_model,
            newdata=wards[which(wards$ward %in% validation_wards),],
            type="response",allow.new.levels=TRUE)
  
  wards_sum <- wards[which(wards$ward %in% validation_wards),] %>% 
    group_by(ward) %>%
    summarise(pop = sum(value),
              pred_turnout_pct = weighted.mean(pred_turnout,value))
  
  wards_sum <- left_join(wards_sum, wards4, by="ward")
  
  wards_sum$true_turnout <- valid$turnout[match(wards_sum$ward,valid$WardCode)]
  
  wards_sum$true_turnout_pct <- wards_sum$true_turnout/wards_sum$pop
  
  val_cor <- cor(wards_sum$true_turnout_pct, wards_sum$pred_turnout_pct)

  train_AIC <- AIC(turnout_model)
  
  print(paste(" Validation Wards Correlation: ", round(val_cor,3), " "))
  print(paste(" MODEL AIC: ", round(train_AIC,0)," "))
  
  return(c(val_cor,train_AIC))
  
}

#FUnction to pick next variable

find_next <- function(turnout_model){
  
  poss_variables <- colnames(bes[which(bes$p_eurefturnout<999),c(405:406,409:423,426:427,431,438:458,460:463,469:477,481,483:487)])
  
  to_test <- poss_variables[!poss_variables %in% variables_included]
  
  test_scores <- c()
  
  for (variable in to_test){
    
    test_formula <- paste("p_eurefturnout ~ fitted.values(turnout_model) + ",variable)
    
    test_model <- glm(test_formula,data=bes[which(bes$p_eurefturnout<999),])
    
    test_scores <- append(test_scores,AIC(test_model))
    
  }

  return(to_test[test_scores==min(test_scores)])
  
}

# Loop

# variables_included <- c("1 ")
# 
# formulas <- c()
# aic_scores <- c()
# cor_scores <- c()
# model_no <- c()
# 
# for (i in 0:60){
#   
#   model_no <- append(model_no, i)
#   print(paste(" MODEL NUMBER:",i,""))
#     
#   slopes <- paste(variables_included,collapse="+")
#   
#   fes_included <- variables_included
#   fes_included[1] <- " "
#   
#   fe <- paste(fes_included,collapse="+")
#     
#   formula <- paste("p_eurefturnout ~ (1|msoa) + (1|pcon) + (1|la) +
#                   (1|agegroup:edlevel:rgn) +
#                   (1|la:agegroup) + (1|la:edlevel) +
#                   (",slopes,"|rgn) +
#                   (",slopes,"|edlevel) +
#                   (",slopes,"|agegroup)",
#                    fe,sep="")
#     
#   formulas <- append(formulas,formula)
#     
#   turnout_model <- glmer(formula,data=bes[which(bes$p_eurefturnout<999),],
#                           family="binomial",
#                          control = glmerControl(calc.derivs = FALSE,
#                                                 optimizer="bobyqa"))
#   
#   summary(turnout_model)
#   
#     
#   cor_aic <- validate(turnout_model)
#     
#   cor_scores <- append(cor_scores,cor_aic[1])
#   aic_scores <- append(aic_scores,cor_aic[2])
#     
#   new_variable <- find_next(turnout_model)
#     
#   variables_included <- append(variables_included,new_variable)
#     
#   print(paste(" NEW VARIABLE ADDED:",new_variable,""))
# 
#   if (i > 0){
#     
#     par(mar = c(5, 4, 4, 4) + 0.3)
#     plot(model_no,aic_scores)
#     lines(model_no,aic_scores,col="red")
#     par(new=TRUE)
#     plot(model_no,cor_scores,axes=FALSE,xlab="",ylab="")
#     lines(model_no,cor_scores,col="green")
#     axis(side=4,at=pretty(range(cor_scores)))
#     abline(v=model_no[which(aic_scores==min(aic_scores))],lty=2)
#     abline(v=model_no[which(cor_scores==max(cor_scores))],lty=3)
#     
#   }
#   
#   results <- data.frame(model_no,
#              formulas,
#              aic_scores,
#              cor_scores)
#   
#   write.csv(results,"model/turnout_models.csv")
#   
# }

results <- read.csv("model/turnout_models.csv")

#formula <- results$formulas[which(results$cor_scores==max(results$cor_scores))]

#summary(turnout_model <- glmer(formula,data=bes[which(bes$p_eurefturnout<999),],
                       #family="binomial",
                       #control = glmerControl(calc.derivs = FALSE,
                                              #optimizer="bobyqa")))

#save(turnout_model, file="model/models/turnout_model.rda")

load("model/models/turnout_model.rda")

wards$pred_turnout <- NA
wards$pred_turnout[which(wards$ward %in% validation_wards)] <- predict(turnout_model,
                                                                             newdata=wards[which(wards$ward %in% validation_wards),],
                                                                             type="response",
                                                                             allow.new.levels=TRUE)

