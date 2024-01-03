#####  -------------------indices ~ Day 

ind_glmm_day <- glmer(Individuals ~ Day + (1 | Pitfall),
                      data = digg, family = poisson(link = "log"))

summary(ind_glmm_day)

even_glmm_day <- glmer(Evenness_e.H.S ~ Day +   (1|Pitfall),
                    data = digg, family = Gamma(link = "log"))
summary(even_glmm_day)

domi_glmm_day <- glmer(Dominance_D ~ Day +   (1|Pitfall),
                       data = digg, family = Gamma(link = "log"))
summary(domi_glmm_day)

simp_glmm_day <- glmer(Simpson_1.D ~ Day +   (1|Pitfall),
                    data = fdigg, family = Gamma(link = "log"))
summary(simp_glmm_day)

shan_glmm_day <- glmer(Shannon_H ~ Day +   (1|Pitfall),
                       data = fdigg, family = Gamma(link = "log"))
summary(shan_glmm_day)