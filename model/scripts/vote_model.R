for (package in c("matrixStats")){
  if (!package %in% installed.packages()[,1]){
    install.packages(package)
  }
}

library(matrixStats)


valid$leave_pct <- valid$Leave/valid$turnout

# Validation function

validate_vote <- function(vote_model) {
  
  wards$pred_leave <- NA
  
  wards$pred_leave[which(wards$ward %in% validation_wards)] <- 
    predict(vote_model,
            newdata=wards[which(wards$ward %in% validation_wards),],
            type="response",allow.new.levels=TRUE)
  
  wards_sum <- wards[which(wards$ward %in% validation_wards),] %>% 
    group_by(ward) %>%
    summarise(pop = sum(value),
              turnout = sum(voters),
              pred_leave_pct = weighted.mean(pred_leave,voters))
  
  wards_sum$true_leave_pct <- valid$leave_pct[match(wards_sum$ward,valid$WardCode)]
  
  val_mse <- mean((wards_sum$true_leave_pct-wards_sum$pred_leave_pct)^2,na.rm=TRUE)
  
  print(plot(wards_sum$true_leave_pct,wards_sum$pred_leave_pct,xlim=c(0,1),ylim=c(0,1)))
  print(abline(a=0,b=1,col="red"))
  
  train_AIC <- AIC(vote_model)
  
  print(paste(" Validation Wards MSE^0.5: ", round(sqrt(val_mse),3), " "))
  print(paste(" MODEL AIC: ", round(train_AIC,0)," "))
  
  return(c(val_mse,train_AIC))
  
}

# Function to find next variable

remove_one <- function(vote_model){
  
  df <- data.frame(global=numeric(),
                   rgn=numeric(),
                   la=numeric(),
                   edlevel=numeric(),
                   agegroup=numeric())
  
  i <- 0
  
  for (variable in variables_included[2:length(variables_included)]){
    
    i <- i+1
    
    global <- abs(coef(vote_model)[["msoa"]][[variable]][1]*sd(unlist(wards4[,variable])))
    
    rgn <- (max(coef(vote_model)[["rgn"]][[variable]])-min(coef(vote_model)[["rgn"]][[variable]]))*sd(unlist(wards4[,variable]))
    
    la <- (max(coef(vote_model)[["la"]][[variable]])-min(coef(vote_model)[["la"]][[variable]]))*sd(unlist(wards4[,variable]))
      
    edlevel <- (max(coef(vote_model)[["edlevel"]][[variable]])-min(coef(vote_model)[["edlevel"]][[variable]]))*sd(unlist(wards4[,variable]))
      
    agegroup <- (max(coef(vote_model)[["agegroup"]][[variable]])-min(coef(vote_model)[["agegroup"]][[variable]]))*sd(unlist(wards4[,variable]))
    
    df[i,] <- c(global,rgn,la,edlevel,agegroup)
    
  }
  
  means <- rowMeans(data.frame(rank(df$global),rank(df$rgn),rank(df$edlevel),rank(df$agegroup)))
  maxs <- creditmodel::rowMaxs(data.frame(rank(df$global),rank(df$rgn),rank(df$edlevel),rank(df$agegroup)))
  
  variable_remove <- variables_included[2:length(variables_included)][maxs==min(maxs)]
  
  if (length(variable_remove)>1){
    
    variable_remove <- variable_remove[means[variables_included[2:length(variables_included)] %in% variable_remove]==min(means[variables_included[2:length(variables_included)] %in% variable_remove])]
      
  } 
  
  if (length(variable_remove)>1){
    
    variable_remove <- variable_remove[2]
    
  }
  
  return(variable_remove)
  
}

remove_one2 <- function(vote_model){
  
  z_scores <- coef(summary(vote_model))[2:length(coef(summary(vote_model))[,"z value"]),"z value"]
  
  variable_remove <- names(z_scores[abs(z_scores)==min(abs(z_scores))])
  
  return(variable_remove)
  
}


# Loop

# variables_included <- c("1 ",colnames(bes)[c(406:407,409:411,416:419,425,429,431,435,443,459,463,470,471,473,479,485)])
# 
# formulas <- c()
# aic_scores <- c()
# mse_scores <- c()
# model_no <- c()
# 
# 
# for (i in 1:22){
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
#   formula <- paste("p_eurefvote ~ (1|msoa) + (1|pcon) +
#                    (1|agegroup:edlevel:rgn) +
#                    (1|msoa:agegroup) + (1|msoa:edlevel) +
#                    (1|la:agegroup) + (1|la:edlevel) +
#                    (",slopes,"|rgn) +
#                    (",slopes,"|la) +
#                    (",slopes,"|edlevel) +
#                    (",slopes,"|agegroup)",
#                    fe,sep="")
# 
#   formulas <- append(formulas,formula)
# 
#   #vote_model <- glmer(formula,data=bes[which(bes$p_eurefvote<999),],
#   vote_model <- glmer(formula,data=bes[which(bes$p_eurefvote<999),],
#                       family="binomial",
#                       control = glmerControl(calc.derivs = FALSE,
#                                              optimizer="bobyqa"))
#   
#   save(vote_model, file="model/models/latest_vote_model.rda")
# 
# 
# 
#   mse_aic <- validate_vote(vote_model)
# 
#   mse_scores <- append(mse_scores,mse_aic[1])
#   aic_scores <- append(aic_scores,mse_aic[2])
#   
#   if (mse_aic[1]==min(mse_scores)){
#     
#     save(vote_model, file="model/models/vote_model.rda")
#     
#   }
#   
#   save.image("workspace_image.rda")
# 
#   variable_remove <- remove_one2(vote_model)
# 
#   variables_included <- variables_included[which(variables_included!=variable_remove)]
# 
#   print(paste(" VARIABLE REMOVED:",variable_remove,""))
# 
#   if (i > 0){
# 
#     par(mar = c(5, 4, 4, 4) + 0.3)
#     plot(model_no,aic_scores)
#     lines(model_no,aic_scores,col="red")
#     par(new=TRUE)
#     plot(model_no,mse_scores,axes=FALSE,xlab="",ylab="")
#     lines(model_no,mse_scores,col="green")
#     axis(side=4,at=pretty(range(mse_scores)))
#     abline(v=model_no[which(aic_scores==min(aic_scores))],lty=2)
#     abline(v=model_no[which(mse_scores==min(mse_scores))],lty=3)
# 
#   }
# 
#   results <- data.frame(model_no,
#                         formulas,
#                         aic_scores,
#                         mse_scores)
# 
#   write.csv(results,"model/vote_models2.csv")
# 
# }

formula <- "p_eurefvote ~ (1|msoa) + (1|pcon) +
                   (1|agegroup:edlevel:rgn) +
                   (1|msoa:agegroup) + (1|msoa:edlevel) +
                   (1|la:agegroup) + (1|la:edlevel) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|rgn) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|la) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|edlevel) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|agegroup) +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct"

# 0.003619088 

formula <- "p_eurefvote ~ (1|msoa) + (1|pcon) +
                   (1|agegroup:edlevel:rgn) +
                   (1|pcon:agegroup) + (1|pcon:edlevel) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|rgn) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|la) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|edlevel) +
                   (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|agegroup) +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct"

# 0.003695319

# formula <- "p_eurefvote ~ (1|msoa) + (1|pcon) +
#                    (1|agegroup:edlevel:rgn) +
#                    (1|pcon:agegroup) + (1|pcon:edlevel) +
#                    (1|la:agegroup) + (1|la:edlevel) +
#                    (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|rgn) +
#                    (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|la) +
#                    (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|edlevel) +
#                    (1 +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct|agegroup) +fulltimestudent_pct+health_fair_pct+nevermarried_pct+british_only_pct+higher_manager_pct+lower_manager_pct+hindu_pct"
# 
# 
# vote_model <- glmer(formula,data=bes[which(bes$p_eurefvote<999),],
#                     family="binomial",
#                     control = glmerControl(calc.derivs = FALSE,
#                                            optimizer="bobyqa"))
# 
# validate_vote(vote_model)
# 
# save(vote_model,file="model/models/vote_model.rda")

load("model/models/vote_model.rda")


msoas$pred_leave <- NA
msoas$pred_leave <- predict(vote_model,
                              newdata=msoas,
                              type="response",
                              allow.new.levels=TRUE)

validate_vote(vote_model)
   