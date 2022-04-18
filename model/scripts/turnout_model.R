for (package in c("lme4","readxl","margins")){
  if (!package %in% installed.packages()[,1]){
    install.packages(package)
  }
}

library(lme4)
library(readxl)
library(margins)

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

variables_included <- c("1 ")

formulas <- c()
aic_scores <- c()
cor_scores <- c()
model_no <- c()

for (i in 0:60){
  
  model_no <- append(model_no, i)
  print(paste(" MODEL NUMBER:",i,""))
    
  slopes <- paste(variables_included,collapse="+")
  
  fes_included <- variables_included
  fes_included[1] <- " "
  
  fe <- paste(fes_included,collapse="+")
    
  formula <- paste("p_eurefturnout ~ (1|msoa) + (1|pcon) + (1|la) +
                  (1|agegroup:edlevel:rgn) +
                  (1|la:agegroup) + (1|la:edlevel) +
                  (",slopes,"|rgn) +
                  (",slopes,"|edlevel) +
                  (",slopes,"|agegroup)",
                   fe,sep="")
    
  formulas <- append(formulas,formula)
    
  turnout_model <- glmer(formula,data=bes[which(bes$p_eurefturnout<999),],
                          family="binomial",
                         control = glmerControl(calc.derivs = FALSE,
                                                optimizer="bobyqa"))
  
  summary(turnout_model)
  
    
  cor_aic <- validate(turnout_model)
    
  cor_scores <- append(cor_scores,cor_aic[1])
  aic_scores <- append(aic_scores,cor_aic[2])
    
  new_variable <- find_next(turnout_model)
    
  variables_included <- append(variables_included,new_variable)
    
  print(paste(" NEW VARIABLE ADDED:",new_variable,""))

  if (i > 0){
    
    par(mar = c(5, 4, 4, 4) + 0.3)
    plot(model_no,aic_scores)
    lines(model_no,aic_scores,col="red")
    par(new=TRUE)
    plot(model_no,cor_scores,axes=FALSE,xlab="",ylab="")
    lines(model_no,cor_scores,col="green")
    axis(side=4,at=pretty(range(cor_scores)))
    abline(v=model_no[which(aic_scores==min(aic_scores))],lty=2)
    abline(v=model_no[which(cor_scores==max(cor_scores))],lty=3)
    
  }
  
  results <- data.frame(model_no,
             formulas,
             aic_scores,
             cor_scores)
  
  write.csv(results,"model/turnout_models.csv")
  
}

results <- read.csv("model/turnout_models.csv")

formula <- results$formulas[which(results$cor_scores==max(results$cor_scores))]

summary(turnout_model <- glmer(formula,data=bes[which(bes$p_eurefturnout<999),],
                       family="binomial",
                       control = glmerControl(calc.derivs = FALSE,
                                              optimizer="bobyqa")))


# summary(turnout_model <- glmer(p_eurefturnout ~ (1|msoa) + (1|pcon) + (1|la) +
#                                  (1|edlevel) + (1|agegroup) + (1|agegroup:edlevel:rgn) +
#                                  (1|la:agegroup) + (1|la:edlevel) +
#                                  (1+female_pct+detachedhouse_pct+flat_apartment_pct+
#                                     bornUK_pct+bornEU_pct+fulltimestudent_pct+retired_pct+
#                                     unemployed_pct+white_pct+black_pct+health_mean+
#                                     dep_child_pct+ind_professional_pct+english_only_pct+
#                                     deprivation_mean+routine_pct+noreligion_pct+christian_pct+
#                                     owned_outright_pct+social_rented_pct|rgn) +
#                                  (1+female_pct+detachedhouse_pct+flat_apartment_pct+
#                                     bornUK_pct+bornEU_pct+fulltimestudent_pct+retired_pct+
#                                     unemployed_pct+white_pct+black_pct+health_mean+
#                                     dep_child_pct+ind_professional_pct+english_only_pct+
#                                     deprivation_mean+routine_pct+noreligion_pct+christian_pct+
#                                     owned_outright_pct+social_rented_pct|agegroup:edlevel),
#                                data=bes[which(bes$p_eurefturnout<999),], family="binomial"))

#ps_frame$pred_turnout <- predict(turnout_model, newdata=ps_frame, type="response",allow.new.levels=TRUE)


View(error_cor <- data.frame(cor(wards_sum[,c(4:86,88)])[,84]))

oa_turnout <- ps_frame %>% group_by(GEO_CODE) %>% summarise(turnout = weighted.mean(pred_turnout,w=value),
                                                            easting = mean(easting),
                                                            northing = mean(northing),
                                                            la = la[[1]])

summary(euref_model <- glmer(p_eurefvote ~ (1|msoa) + (1|pcon) + (1|la) +
                               (1|edlevel) + (1|agegroup) + (1|agegroup:edlevel:rgn) +
                               (1+white_pct+deprivation_mean+health_mean|rgn) +
                               dep_child_pct + higher_manager_pct + routine_pct + 
                               owned_outright_pct + english_only_pct + welshlang_pct +
                               ind_agriculture_pct + scottish_only_pct + black_pct +
                               social_rented_pct,
                             data=bes[which(bes$p_eurefvote<999),], family="binomial"))

ps_frame$pred_vote <- NA
ps_frame$pred_vote[1:10000] <- predict(euref_model, newdata=ps_frame[1:10000,], type="response",allow.new.levels=TRUE)

ps_frame$pred_vote[which(ps_frame$la=="E06000052")] <- predict(euref_model, newdata=ps_frame[which(ps_frame$la=="E06000052"),], type="response",allow.new.levels=TRUE)


ps_frame$pred_vote[which(ps_frame$ward %in% validation_wards)] <- predict(euref_model, newdata=ps_frame[which(ps_frame$ward %in% validation_wards),], type="response",allow.new.levels=TRUE)
in_wards <- ps_frame[which(ps_frame$ward %in% validation_wards),]
in_wards$voters <- in_wards$pred_turnout*in_wards$value

wards <- in_wards %>% group_by(ward) %>% summarise(turnout_pct = weighted.mean(pred_turnout,value),
                                                   turnout = sum(voters),
                                                   vote_leave = weighted.mean(pred_vote, voters),
                                                   northing=mean(northing),
                                                   easting=mean(easting))

valid$pred_turnout <- wards$turnout[match(valid$WardCode,wards$ward)]
valid$pred_leave <- wards$vote_leave[match(valid$WardCode,wards$ward)]
valid$error <- valid$pred_leave - valid$`Leave%`/100
valid$turnout_error <- (valid$pred_turnout-valid$turnout)/valid$turnout

cornwall_oas <- cornwall %>% group_by(GEO_CODE) %>% summarise(turnout = weighted.mean(pred_turnout,value),
                                                              vote_leave = weighted.mean(pred_vote, voters),
                                                              northing=mean(northing),
                                                              easting=mean(easting))

ggplot(cornwall_oas, aes(x=easting,y=northing,col=vote_leave)) + geom_point(alpha=1) +
  scale_color_steps2(low="orange",mid="white",high="dark blue",midpoint=0.5)

library(sp)
library(leaflet)
library(rgdal)

xy <- cornwall_oas[,c("easting","northing")]

points <- SpatialPointsDataFrame(coords = xy, data = cornwall_oas,
                                 proj4string = CRS("+init=epsg:27700"))

points <- spTransform(points, CRS("+proj=longlat"))
points$lat <- unlist(points@coords)[,2]
points$long <- unlist(points@coords)[,1]

getColor <- function(points) {
  sapply(points$vote_leave, function(vote_leave) {
    if(vote_leave <= 0.4) {
      "darkgreen"
    } else if(vote_leave <= 0.5) {
      "green"
    } else if(vote_leave <= 0.6) {
      "lightblue"
    } else {
      "darkblue"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(points)
)

leaflet_map <- leaflet(points) %>% addTiles() %>% addAwesomeMarkers(~long,~lat,icon=icons,popup = ~as.character(vote_leave), label = ~as.character(vote_leave))

leaflet_map
