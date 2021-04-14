
load('pnas_light_ineq.RData')

#Table 1

mod_1 <- lmer(Gini ~ Light_Gini + 
                (1 | Year), 
              data = df_nat)
summary(mod_1)
r.squaredGLMM(mod_1)[2]
AIC(mod_1)
AICc(mod_1)

mod_2 <- lmer(Gini ~ Light_Gini +  
                log(POP) +
                (1 | Year), 
              data = df_nat)
summary(mod_2)
r.squaredGLMM(mod_2)[2]
AIC(mod_2)
AICc(mod_2)


mod_3 <- lmer(Gini ~ Light_Gini +  
                  log(POP) +
                  log(GDP) + 
                  (1 | Year), 
                data = df_nat)
summary(mod_3)
r.squaredGLMM(mod_3)[2]
AIC(mod_3)
AICc(mod_3)

mod_4 <- lm(Gini ~ Light_Gini +  
                log(POP) +
                log(GDP), 
              data = df_nat)
summary(mod_4)


#Fig 1
visreg(mod_3, 'Light_Gini', gg = T, overlay = T, band = F) + 
  geom_point(size = 2, colour = 'grey') + labs(x = 'Light Gini', y = 'Income Gini')

spplot(world_2010, c("gini_disp"), col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd'))(20), 
       cuts = 19, col = "grey")

spplot(world_2010, c("light_gini_lpp"), col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd'))(20), 
       cuts = 19, col = "grey")


#Fig 2
ggplot(data = swiid_df, aes(income_cat, light_gini_lpp)) + 
  geom_boxplot(colour = 'darkblue', fill = 'lightblue', size = 1) +
  labs(x = 'Income Group', y = 'Light Gini') +
  theme_minimal(base_size = 20) + xlim(c('High Income', 'Low Income', 'Middle Income'))

ggplot(data = swiid_df, aes(income_cat, gini_disp)) + 
  geom_boxplot(colour = 'darkblue', fill = 'lightblue', size = 1) +
  labs(x = 'Income Group', y = 'Income Gini') +
  theme_minimal(base_size = 20) + xlim(c('High Income', 'Low Income', 'Middle Income'))

ggplot(data = swiid_df, aes(region, light_gini_lpp)) + 
  geom_boxplot(colour = 'red4', fill = 'indianred1', size = 1) +
  labs(x = 'Regions', y = 'Light Gini') +
  theme_minimal(base_size = 20) 

ggplot(data = swiid_df, aes(region, gini_disp)) + 
  geom_boxplot(colour = 'red4', fill = 'indianred1', size = 1) +
  labs(x = 'Regions', y = 'Income Gini') +
  theme_minimal(base_size = 20) 


#Fig 3
spplot(US_states_10, c("Estimate..Gini.Index"), par.settings=list(fontsize=list(text=20)), 
       col.regions = colorRampPalette(brewer.pal(9, 'Reds'))(50), cuts = 49, col = "transparent")

spplot(US_states_10, c("light_gini"), par.settings=list(fontsize=list(text=20)),
       col.regions = colorRampPalette(brewer.pal(9, 'Reds'))(50), cuts = 49, col = "transparent")

spplot(US_county, c("Estimate..Gini.Index"), par.settings=list(fontsize=list(text=20)),
       col.regions = colorRampPalette(brewer.pal(9, 'Reds'))(50), cuts = 49, col = "transparent")

cor.test(~ light_gini + Gini, df_US_states[year != 2005])

ggplot(df_US_states[year != 2005], aes(light_gini, Gini)) + geom_point() + 
  geom_smooth(method = 'lm', se = F) + labs(x = 'Light Gini', y = 'Income Gini')


# Fig 4
# Use data at https://zenodo.org/record/4635734/files/gini_2010.tif?download=1 to plot




