for (package in c("lme4","readxl")){
  if (!package %in% installed.packages()[,1]){
    install.packages(package)
  }
}

library(lme4)
library(readxl)

#Ward Poststratification Frame

wards1 <- ps_frame %>% group_by(ward) %>%
  summarise_at(vars(la:rgn), funs(first(.)))

wards2 <- ps_frame %>% group_by(ward,msoa,pcon,name) %>%
  summarise_at(vars(agegroup,edlevel), funs(first(.)))

wards3 <- ps_frame %>% group_by(ward,msoa,pcon,name) %>%
  summarise_at(vars(value), funs(sum(.)))

wards4 <- ps_frame %>% group_by(ward,msoa,pcon) %>%
  summarise_at(vars(female_pct:private_rented_pct), funs(weighted.mean(.,w=value)))

wards <- left_join(wards2,wards3,by=c("ward","msoa","pcon","name"))
wards <- left_join(wards,wards1,by=c("ward"))
wards <- left_join(wards,wards4,by=c("ward","msoa","pcon"))

#MSOA Poststratification Frame

msoas1 <- ps_frame %>% group_by(msoa) %>%
  summarise_at(vars(la:rgn), funs(first(.)))

msoas2 <- ps_frame %>% group_by(msoa,pcon,name) %>%
  summarise_at(vars(agegroup,edlevel), funs(first(.)))

msoas3 <- ps_frame %>% group_by(msoa,pcon,name) %>%
  summarise_at(vars(value), funs(sum(.)))

msoas4 <- ps_frame %>% group_by(msoa,pcon) %>%
  summarise_at(vars(female_pct:private_rented_pct), funs(weighted.mean(.,w=value)))

msoas <- left_join(msoas2,msoas3,by=c("msoa","pcon","name"))
msoas <- left_join(msoas,msoas1,by=c("msoa"))
msoas <- left_join(msoas,msoas4,by=c("msoa","pcon"))
rm(msoas1,msoas2,msoas3,msoas4)

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

remove_one <- function(vote_model){
  
  df <- data.frame(global=numeric(),
                   rgn=numeric(),
                   edlevel=numeric(),
                   agegroup=numeric())
  
  i <- 0
  
  for (variable in variables_included[2:length(variables_included)]){
    
    i <- i+1
    
    global <- abs(coef(vote_model)[["la"]][[variable]][1]*sd(unlist(wards4[,variable])))
    
    rgn <- (max(coef(vote_model)[["rgn"]][[variable]])-min(coef(vote_model)[["rgn"]][[variable]]))*sd(unlist(wards4[,variable]))
    
    edlevel <- (max(coef(vote_model)[["edlevel"]][[variable]])-min(coef(vote_model)[["edlevel"]][[variable]]))*sd(unlist(wards4[,variable]))
    
    agegroup <- (max(coef(vote_model)[["agegroup"]][[variable]])-min(coef(vote_model)[["agegroup"]][[variable]]))*sd(unlist(wards4[,variable]))
    
    df[i,] <- c(global,rgn,edlevel,agegroup)
    
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

# Choose starting variables

# variables_included <- c("1 ",colnames(bes)[c(407,413,423,426,431,436,439,442,444,446,451,469,475,477,483:484)])
# 
# fe <- paste(variables_included[2:length(variables_included)],collapse="+")
# 
# formula <- paste("p_eurefturnout ~ as.factor(agegroup) +
#                   as.factor(rgn) +
#                   as.factor(edlevel) +",
#                  fe,sep="")
# 
# summary(test_model <- glm(formula,data=bes[which(bes$p_eurefturnout<999),],
#                        family="binomial"))
# 
# colnames(bes)


# Loop

# variables_included <- c("1 ",colnames(bes)[c(407,413,423,426,431,436,439,442,444,446,451,469,475,477,483:484)])
# 
# formulas <- c()
# aic_scores <- c()
# cor_scores <- c()
# model_no <- c()
# 
# for (i in 1:17){
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
#   formula <- paste("p_eurefturnout ~ (1|pcon) + (1|la) +
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
#   print(summary(turnout_model))
# 
# 
#   cor_aic <- validate(turnout_model)
# 
#   cor_scores <- append(cor_scores,cor_aic[1])
#   aic_scores <- append(aic_scores,cor_aic[2])
# 
#   variable_remove <- remove_one(turnout_model)
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
# 
# results <- read.csv("model/turnout_models.csv")
# 
# formula <- results$formulas[which(results$cor_scores==max(results$cor_scores))]
# 
# summary(turnout_model <- glmer(formula,data=bes[which(bes$p_eurefturnout<999),],
#                                family="binomial",
#                                control = glmerControl(calc.derivs = FALSE,
#                                                       optimizer="bobyqa")))
# 
# save(turnout_model, file="model/models/turnout_model.rda")

load("model/models/turnout_model.rda")

msoas$pred_turnout <- NA
msoas$pred_turnout <- predict(turnout_model,
                               newdata=msoas,
                               type="response",
                               allow.new.levels=TRUE)

wards$pred_turnout <- NA
wards$pred_turnout <- predict(turnout_model,
                              newdata=wards,
                              type="response",
                              allow.new.levels=TRUE)

rm(wards1)
rm(wards2)
rm(wards3)

