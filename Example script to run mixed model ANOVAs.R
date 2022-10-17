#Prepping data file for proportion as response variable

vegdf$WIS_GP <- as.factor(vegdf$WIS_GP)

df.props_WIS_veg <- vegdf %>%
  group_by(sample.bout, baseline, baseline.rep, Transect, Hit.Number, Bank_Location, Longevity, Habit, FunctionalGroup, Nativity, Cvalue, WIS_GP) %>%
  filter(sample.bout =="1") %>%
  drop_na(Species.Hit, WIS_GP) %>%  ## drops rows with missing values NA in either Species.Hit or WIS_GP
  summarise(Species.Hit = n()) %>%  ## from the group_by groupings, count the number 
  group_by(baseline, Transect, Bank_Location, WIS_GP) %>%  
  summarise(Species.Hit = sum(Species.Hit)) %>% ## groups by baseline and Bank_location to sum species.Hit
  mutate(prop = Species.Hit/sum(Species.Hit)) %>% ## mutate the df by adding prop that is each #sp/total
  complete(WIS_GP, fill = list(Species.Hit = 0, prop = 0)) %>% ## keep all levels of factor and replace NA with zeros for both Species.Hit and prop
  mutate(prop_log = log(prop+1))

# Check it adds to one with the 5 groups  
sum(df.props_WIS_veg$prop[1:5])

View(df.props_WIS_veg)

#Stats model for proportion of species upper bank location
#First select out the upper bank location only

df_UpperBank <- subset(df.props_WIS_veg, Bank_Location =="upper")
View(df_UpperBank)

#Now create mixed models with an intercept only, intercept + WIS then do AIC
#to confirm that WIS explains enough variation to substantially increase model fit
#Fit a gaussian distribution (no transformation)

lmer_gaus_null <- lmer(prop ~ 1 + (1|baseline) + (1|Transect:baseline), data = df_UpperBank) #intercept only, just models variation across baselines and transects, no accounting for variation explained by WIS
lmer_gaus_upper <- lmer(prop ~ WIS_GP + (1|baseline) + (1|Transect:baseline), data = df_UpperBank) #WIS added
lmer_gaus_upper2 <- lmer(prop ~ WIS_GP + (1|baseline), data = df_UpperBank) #removing transect nested within baseline b/c it does not explain any additional variation

summary(lmer_gaus_null) #see what model output looks like
summary(lmer_gaus_upper) #see what model output looks like
summary(lmer_gaus_upper2) #see what model output looks like

AIC(lmer_gaus_null,lmer_gaus_upper, lmer_gaus_upper2) #Compare which model is more informative; smaller numbers are better; including WIS greatly improves model

qqnorm(resid(lmer_gaus_upper2)) #Use to visualize whether normally distributed

proportion_wis_gauss <- df_UpperBank$prop #Histogram showing lots of zeroes, positive skew 
hist(proportion_wis_gauss)

#Choose lmer_gaus_upper2 based on AIC and now below run diagnostics, output, pairwise comparisons

effectsize::standardize(lmer_gaus_upper2) %>% summary()
plot_residuals(lmer_gaus_upper2)
plot_model(lmer_gaus_upper2, type = "diag")
sjPlot::plot_model(lmer_gaus_upper2, type = "est", show.values = T)

sjPlot::tab_model(lmer_gaus_upper2, df.method = "satterthwaite", title = "WIS Proportions in Upper Bank", dv.labels = "Proportion Cover")

emmeans(lmer_gaus_upper2, pairwise ~ WIS_GP, adjust = "tukey")


#Not normally distributed: Try log transformed response variable and visualize fit; log transformation did not improve in this case; 

lm_gaus_log_upper <- lmer(prop_log ~ WIS_GP + (1|baseline) + (1|Transect:baseline), data = df_UpperBank)
summary(lm_gaus_log_upper)

qqnorm(resid(lm_gaus_log_upper)) #Use to visualize whether normally distributed
qqline(resid(lm_gaus_log_upper)) #Use to visualize whether normally distributed

proportion_wis_log <- df_UpperBank$prop_log
hist(proportion_wis_log)


effectsize::standardize(lm_gaus_log_upper) %>% summary()
plot_residuals(lm_gaus_log_upper)
plot_model(lm_gaus_log_upper, type = "diag")
sjPlot::plot_model(lm_gaus_log_upper, type = "est", show.values = T)

sjPlot::tab_model(lm_gaus_log_upper, title = "WIS Proportions in Upper Bank", dv.labels = "Proportion Cover")

emmeans(lm_gaus_log_upper, pairwise ~ WIS_GP, adjust = "tukey")