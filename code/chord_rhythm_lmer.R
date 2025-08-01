setwd('D:\\Shan\\Rhythm\\code\\data')
library('lme4')
library('lmerTest')
library('tidyverse')
library('bruceR')
library('gridExtra')

Jan_new <- read.csv(file="chord_rhythm_data.csv", header=TRUE)
for (i in c(1:5,16)) {Jan_new[,i] <- factor(Jan_new[,i])}

Jan_new$Rhythm <- factor(Jan_new$Rhythm, levels = c(4, 3, 2, 1))
Jan_new$Harmony <- factor(Jan_new$Harmony, levels = c(4, 3, 2, 1))

Jan_new <- Jan_new %>%
  group_by(country) %>%
  mutate(standard_groove = scale(groove), standard_pleasure = scale(pleasure),
         standard_MT = scale(MT), standard_GMS = scale(GMS), standard_BMRQ = scale(BMRQ))
Jan_new[,c(8:15)]<-scale(Jan_new[,c(8:15)])

reg_lm <- lm(GMS~MT, data = Jan_new)
tmp <- as.matrix(reg_lm$residuals)

Jan_new$GMS_res <- tmp

contr = list(country='contr.sum', Rhythm='contr.sum', Harmony='contr.sum')

# cor(Jan_new[,c(8:15)], method = 'spearman')

### for groove
Jan_groove_model <- lmer(standard_groove ~ country*MT*Rhythm*Harmony+country*GMS_res*Rhythm*Harmony+country*BMRQ*Rhythm*Harmony + (1+Rhythm+Harmony|Participant.Private.ID), 
                         data = Jan_new, 
                         contrasts = contr,
                         REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
isSingular(Jan_groove_model)
rand(Jan_groove_model)
HLM_summary(Jan_groove_model)
Jan_groove_model_adjusted <- anova(Jan_groove_model)
Jan_groove_model_adjusted$q_value <- p.adjust(Jan_groove_model_adjusted[,c("Pr(>F)" )], method = "fdr")
Jan_groove_model_adjusted <- round(Jan_groove_model_adjusted,3)

## post-hoc
# rhythm * country
# groove_rhythm_poly <- summary(contrast(emmeans(Jan_groove_model, ~ Rhythm | country, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# groove_rhythm_poly_contrasts_country <- summary(contrast(groove_rhythm_poly_contrasts, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
groove_rhythm_poly <- emmeans(Jan_groove_model, ~ Rhythm | country, pbkrtest.limit = 10000)
groove_rhythm_country_contrasts <- emmeans(Jan_groove_model, ~ country | Rhythm , pbkrtest.limit = 10000)
groove_rhythm_poly_contrasts <- emmeans(Jan_groove_model, ~ Rhythm * country, pbkrtest.limit = 10000)
groove_rhythm_consec_eff <- summary(contrast(groove_rhythm_poly, method = "consec", adjust = "bonferroni"))
groove_rhythm_country_diff<-summary(contrast(groove_rhythm_country_contrasts, method = 'pairwise',adjust='bonferroni'))
groove_rhythm_consec_diff<-summary(contrast(groove_rhythm_poly_contrasts, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))

# harmony * country
# groove_harmony_poly <- summary(contrast(emmeans(Jan_groove_model, ~ Harmony | country, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# groove_harmony_poly_contrasts_country <- summary(contrast(groove_harmony_poly_contrasts, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
groove_harmony_poly_contrasts <- emmeans(Jan_groove_model, ~ Harmony * country, pbkrtest.limit = 10000)
groove_harmony_country_contrasts <- emmeans(Jan_groove_model, ~ country | Harmony , pbkrtest.limit = 10000)
groove_harmony_poly <- emmeans(Jan_groove_model, ~ Harmony | country, pbkrtest.limit = 10000)
groove_harmony_consec_eff <- summary(contrast(groove_harmony_poly, method = "consec", adjust = "bonferroni"))
groove_harmony_country_diff<-summary(contrast(groove_harmony_country_contrasts, method = 'pairwise',adjust='bonferroni'))
groove_harmony_consec_diff<-summary(contrast(groove_harmony_poly_contrasts, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))


# # rhythm * harmony
# groove_interaction_rhythm_poly<-summary(contrast(emmeans(Jan_groove_model, ~ Rhythm | Harmony, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# groove_interaction_rhythm_poly_contrast<-emmeans(Jan_groove_model, ~ Rhythm*Harmony, pbkrtest.limit = 10000)
# groove_interaction_rhythm_poly_contrast_harmony<-summary(contrast(groove_interaction_rhythm_poly_contrast, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
# groove_interaction_rhythm_pairwise<-summary(contrast(groove_interaction_rhythm_poly_contrast, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))
# 
# 
# groove_interaction_harmony_poly<-summary(contrast(emmeans(Jan_groove_model, ~ Harmony | Rhythm, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# groove_interaction_harmony_poly_contrast<-emmeans(Jan_groove_model, ~ Harmony*Rhythm, pbkrtest.limit = 10000)
# groove_interaction_harmony_poly_contrast_rhythm<-summary(contrast(groove_interaction_harmony_poly_contrast, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
# groove_interaction_harmony_pairwise<-summary(contrast(groove_interaction_harmony_poly_contrast, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))


## plot
# data_preparation
country_rhythm<-Jan_new %>%
  group_by(interaction(country, Rhythm)) %>%
  summarise(mean_groove = mean(standard_groove), groove_se = sd(standard_groove)/sqrt(length(standard_groove)), 
            mean_pleasure = mean(standard_pleasure), pleasure_se = sd(standard_pleasure)/sqrt(length(standard_pleasure)))%>%
  separate(`interaction(country, Rhythm)`, into = c("country", "Rhythm"), sep = "\\.")

country_harmony<-Jan_new %>%
  group_by(interaction(country, Harmony)) %>%
  summarise(mean_groove = mean(standard_groove), groove_se = sd(standard_groove)/sqrt(length(standard_groove)), 
            mean_pleasure = mean(standard_pleasure), pleasure_se = sd(standard_pleasure)/sqrt(length(standard_pleasure)))%>%
  separate(`interaction(country, Harmony)`, into = c("country", "Harmony"), sep = "\\.")

# country:Rhythm
groove_country_rhythm_plot<-ggplot()+
  geom_boxplot(data=Jan_new,aes(x=Rhythm, y=standard_groove, color=country, fill=country), alpha=0.2, position=position_dodge(0.8))+
  geom_point(data=country_rhythm,aes(x=Rhythm, group=country, y=mean_groove, color=country),position=position_dodge(0.8))+
  geom_line(data=country_rhythm,aes(x=Rhythm, group=country, y=mean_groove, color=country),position=position_dodge(0.8))+
  scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
  coord_cartesian(ylim = c(-3, 2.5))+
  scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
  # scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig2_groove_rhythm_by_country.pdf", plot = groove_country_rhythm_plot, width = 6, height = 8, device = "pdf")

# country:Harmony
groove_country_harmony_plot<-ggplot()+
  geom_boxplot(data=Jan_new,aes(x=Harmony, y=standard_groove, color=country, fill=country), alpha=0.2, position=position_dodge(0.8))+
  geom_point(data=country_harmony,aes(x=Harmony, group=country, y=mean_groove, color=country),position=position_dodge(0.8))+
  geom_line(data=country_harmony,aes(x=Harmony, group=country, y=mean_groove, color=country),position=position_dodge(0.8))+
  scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
  coord_cartesian(ylim = c(-3, 2.5))+
  scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
  # scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())  
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig2_groove_harmony_by_country.pdf", plot = groove_country_harmony_plot, width = 6, height = 8, device = "pdf")

# # rhythm:Harmony
# groove_harmony_rhythm_plot<-ggplot()+
#   geom_boxplot(data=Jan_new,aes(x=Rhythm, y=standard_groove, color=Harmony, fill=Harmony), alpha=0.2, position=position_dodge(0.8))+
#   geom_point(data=harmony_rhythm,aes(x=Rhythm, group=Harmony, y=mean_groove, color=Harmony),position=position_dodge(0.8))+
#   geom_line(data=harmony_rhythm,aes(x=Rhythm, group=Harmony, y=mean_groove, color=Harmony),position=position_dodge(0.8))+
#   # scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
#   # scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
#   coord_cartesian(ylim = c(-3, 2.5))+
#   scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
#   # scale_x_continuous(expand = c(0,0))+
#   theme_classic()+
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())  
# # ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig2_groove_harmony_by_rhythm.pdf", plot = groove_harmony_rhythm_plot, width = 10, height = 8, device = "pdf")

### for pleasure
Jan_pleasure_model <- lmer(standard_pleasure ~ country*MT*Rhythm*Harmony+country*GMS_res*Rhythm*Harmony+country*BMRQ*Rhythm*Harmony + (1+Rhythm+Harmony|Participant.Private.ID), 
                           data = Jan_new, 
                           contrasts = contr,
                           REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
isSingular(Jan_pleasure_model)
rand(Jan_pleasure_model)
HLM_summary(Jan_pleasure_model)
Jan_pleasure_model_adjusted <- anova(Jan_pleasure_model)
Jan_pleasure_model_adjusted$q_value <- p.adjust(Jan_pleasure_model_adjusted[,c("Pr(>F)" )], method = "fdr")
Jan_pleasure_model_adjusted <- round(Jan_pleasure_model_adjusted,3)

## post-hoc
# rhythm * country
# pleasure_rhythm_poly <- summary(contrast(emmeans(Jan_pleasure_model, ~ Rhythm | country, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# pleasure_rhythm_poly_contrasts_country <- summary(contrast(pleasure_rhythm_poly_contrasts, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
pleasure_rhythm_poly_contrasts <- emmeans(Jan_pleasure_model, ~ Rhythm * country, pbkrtest.limit = 10000)
pleasure_rhythm_country_contrasts <- emmeans(Jan_pleasure_model, ~ country | Rhythm , pbkrtest.limit = 10000)
pleasure_rhythm_poly <- emmeans(Jan_pleasure_model, ~ Rhythm | country, pbkrtest.limit = 10000)
pleasure_rhythm_consec_eff <- summary(contrast(pleasure_rhythm_poly, method = "consec", adjust = "bonferroni"))
pleasure_rhythm_country_diff<-summary(contrast(pleasure_rhythm_country_contrasts, method = 'pairwise',adjust='bonferroni'))
pleasure_rhythm_consec_diff<-summary(contrast(pleasure_rhythm_poly_contrasts, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))

# harmony * country
# pleasure_harmony_poly <- summary(contrast(emmeans(Jan_pleasure_model, ~ Harmony | country, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# pleasure_harmony_poly_contrasts_country <- summary(contrast(pleasure_harmony_poly_contrasts, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
pleasure_harmony_poly_contrasts <- emmeans(Jan_pleasure_model, ~ Harmony * country, pbkrtest.limit = 10000)
pleasure_harmony_country_contrasts <- emmeans(Jan_pleasure_model, ~ country | Harmony , pbkrtest.limit = 10000)
pleasure_harmony_poly <- emmeans(Jan_pleasure_model, ~ Harmony | country, pbkrtest.limit = 10000)
pleasure_harmony_consec_eff <- summary(contrast(pleasure_harmony_poly, method = "consec", adjust = "bonferroni"))
pleasure_harmony_country_diff<-summary(contrast(pleasure_harmony_country_contrasts, method = 'pairwise',adjust='bonferroni'))
pleasure_harmony_consec_diff<-summary(contrast(pleasure_harmony_poly_contrasts, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))


# rhythm * harmony
# pleasure_interaction_rhythm_poly<-summary(contrast(emmeans(Jan_pleasure_model, ~ Rhythm | Harmony, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# pleasure_interaction_rhythm_poly_contrast_harmony<-summary(contrast(pleasure_interaction_rhythm_poly_contrast, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
pleasure_interaction_rhythm_poly_contrast<-emmeans(Jan_pleasure_model, ~ Rhythm*Harmony, pbkrtest.limit = 10000)
pleasure_interaction_rhythm_harmony_contrasts <- emmeans(Jan_pleasure_model, ~ Harmony | Rhythm , pbkrtest.limit = 10000)
pleasure_interaction_rhythm_poly <- emmeans(Jan_pleasure_model, ~ Rhythm | Harmony, pbkrtest.limit = 10000)
pleasure_interaction_rhythm_consec_eff <- summary(contrast(pleasure_interaction_rhythm_poly, method = "consec", adjust = "bonferroni"))
pleasure_interaction_rhythm_harmony_diff<-summary(contrast(pleasure_interaction_rhythm_harmony_contrasts, method = 'consec',adjust='bonferroni'))
pleasure_interaction_rhythm_consec_diff<-summary(contrast(pleasure_interaction_rhythm_poly_contrast, interaction = c("consec", 'consec'), adjust = "bonferroni"))


# pleasure_interaction_harmony_poly<-summary(contrast(emmeans(Jan_pleasure_model, ~ Harmony | Rhythm, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# pleasure_interaction_harmony_poly_contrast_rhythm<-summary(contrast(pleasure_interaction_harmony_poly_contrast, interaction = c("poly", 'pairwise'), adjust = "bonferroni"))
pleasure_interaction_harmony_poly_contrast<-emmeans(Jan_pleasure_model, ~ Harmony*Rhythm, pbkrtest.limit = 10000)
pleasure_interaction_harmony_rhythm_contrasts <- emmeans(Jan_pleasure_model, ~ Rhythm | Harmony , pbkrtest.limit = 10000)
pleasure_interaction_harmony_poly <- emmeans(Jan_pleasure_model, ~ Harmony | Rhythm, pbkrtest.limit = 10000)
pleasure_interaction_harmony_consec_eff <- summary(contrast(pleasure_interaction_harmony_poly, method = "consec", adjust = "bonferroni"))
pleasure_interaction_harmony_rhythm_diff<-summary(contrast(pleasure_interaction_harmony_rhythm_contrasts, method = 'consec',adjust='bonferroni'))
pleasure_interaction_harmony_pairwise<-summary(contrast(pleasure_interaction_harmony_poly_contrast, interaction = c("consec", 'consec'), adjust = "bonferroni"))


JN_twoway <- function(model, predictor, moderator, interaction) {
  # beta0+beta1*X+beta2*M+beta3*M*X
  # M: GMS/MT
  # X: poly(wSI,2)[2]
  beta1 <- summary(model)$coefficients[paste0(predictor), "Estimate"]
  se_beta1 <- summary(model)$coefficients[paste0(predictor), "Std. Error"]
  
  beta3 <- summary(model)$coefficients[paste0(interaction), "Estimate"]
  se_beta3 <- summary(model)$coefficients[paste0(interaction), "Std. Error"]
  
  cov_beta1_beta3 <- vcov(model)[paste0(predictor), paste0(interaction)]
  
  se_omega <- function(M) {
    sqrt(
      se_beta1^2+
        2*M*cov_beta1_beta3+
        (M^2)*se_beta3^2
    )
  }
  
  solve_moderate_upper <- function() {
    f <- function(M) {
      y <- beta1+beta3*M
      se_y <- se_omega(M)
      return(y / se_y - 1.96)
    }
    result <- uniroot(f, lower = -1e+6, upper = 1e+6)$root
    return(result)
  }
  
  solve_moderate_lower <- function() {
    f <- function(M) {
      y <- beta1+beta3*M
      se_y <- se_omega(M)
      return(y / se_y + 1.96)
    }
    result <- uniroot(f, lower = -1e+6, upper = 1e+6)$root
    return(result)
  }
  
  moderate_solutions_upper <- solve_moderate_upper()
  moderate_solutions_lower <- solve_moderate_lower()
  
  return(list(
    moderate_solutions_upper,
    moderate_solutions_lower
  ))
}
# MT:Harmony(MT:Harmony.L)
# JN_twoway(Jan_pleasure_model, 'Harmony2', 'MT', 'MT:Harmony2')
# JN_twoway(Jan_pleasure_model, 'Harmony3', 'MT', 'MT:Harmony3')
# 
# pleasure_MT_harmony_contrast <- emtrends(Jan_pleasure_model, "Harmony", var = "MT", lmerTest.limit = 3568, pbkrtest.limit = 3568)
# pleasure_MT_harmony_eff <- summary(test(pleasure_MT_harmony_contrast))
# pleasure_MT_harmony_diff <- summary(contrast(pleasure_MT_harmony_contrast, method = 'consec', adjust = 'bonferroni'))


# Harmony:GMS(Harmony.L:GMS)
JN_twoway(Jan_pleasure_model, 'Harmony2', 'GMS_res', 'Harmony2:GMS_res')
JN_twoway(Jan_pleasure_model, 'Harmony3', 'GMS_res', 'Harmony3:GMS_res')

pleasure_GMS_harmony_contrast <- emtrends(Jan_pleasure_model, "Harmony", var = "GMS_res", lmerTest.limit = 3568, pbkrtest.limit = 3568)
pleasure_GMS_harmony_eff <- summary(test(pleasure_GMS_harmony_contrast))
pleasure_GMS_harmony_diff <- summary(contrast(pleasure_GMS_harmony_contrast, method = 'consec', adjust = 'bonferroni'))

# country:harmony:rhythm (country1:Rhythm.Q:Harmony.L)
# pleasure_3way_poly_marginal_mean <-summary(contrast(emmeans(Jan_pleasure_model, ~ Rhythm|Harmony*country, pbkrtest.limit = 10000),method='poly', adjust = "bonferroni"))
# pleasure_3wayinteraction_post<-emmeans(Jan_pleasure_model, ~ Harmony*Rhythm|country, pbkrtest.limit = 10000)
# pleasure_3wayinteraction_poly<-summary(contrast(pleasure_3wayinteraction_post,interaction = c('pairwise', "poly"), adjust = "bonferroni"))
# pleasure_3wayinteraction_post_2<-emmeans(Jan_pleasure_model, ~ country*Rhythm|Harmony, pbkrtest.limit = 10000)
# pleasure_3wayinteraction_poly_2<-summary(contrast(pleasure_3wayinteraction_post_2,interaction = c('pairwise', "poly"), adjust = "bonferroni"))

# data preparation
harmony_rhythm<-Jan_new %>%
  group_by(interaction(Rhythm, Harmony)) %>%
  summarise(mean_groove = mean(standard_groove), groove_se = sd(standard_groove)/sqrt(length(standard_groove)),
            mean_pleasure = mean(standard_pleasure), pleasure_se = sd(standard_pleasure)/sqrt(length(standard_pleasure)))%>%
  separate(`interaction(Rhythm, Harmony)`, into = c("Rhythm", "Harmony"), sep = "\\.")

## plot
# country:Rhythm
pleasure_country_rhythm_plot<-ggplot()+
  geom_boxplot(data=Jan_new,aes(x=Rhythm, y=standard_pleasure, color=country, fill=country), alpha=0.2, position=position_dodge(0.8))+
  geom_point(data=country_rhythm,aes(x=Rhythm, group=country, y=mean_pleasure, color=country),position=position_dodge(0.8))+
  geom_line(data=country_rhythm,aes(x=Rhythm, group=country, y=mean_pleasure, color=country),position=position_dodge(0.8))+
  scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
  coord_cartesian(ylim = c(-3, 2.5))+
  scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
  # scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig3_pleasure_rhythm_by_country.pdf", plot = pleasure_country_rhythm_plot, width = 6, height = 8, device = "pdf")

# country:Harmony
pleasure_country_harmony_plot<-ggplot()+
  geom_boxplot(data=Jan_new,aes(x=Harmony, y=standard_pleasure, color=country, fill=country), alpha=0.2, position=position_dodge(0.8))+
  geom_point(data=country_harmony,aes(x=Harmony, group=country, y=mean_pleasure, color=country),position=position_dodge(0.8))+
  geom_line(data=country_harmony,aes(x=Harmony, group=country, y=mean_pleasure, color=country),position=position_dodge(0.8))+
  scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
  coord_cartesian(ylim = c(-3, 2.5))+
  scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
  # scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())  
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig3_pleasure_harmony_by_country.pdf", plot = pleasure_country_harmony_plot, width = 6, height = 8, device = "pdf")

# rhythm:Harmony
pleasure_harmony_rhythm_plot<-ggplot()+
  geom_boxplot(data=Jan_new,aes(x=Rhythm, y=standard_pleasure, color=Harmony, fill=Harmony), alpha=0.2, position=position_dodge(0.8))+
  geom_point(data=harmony_rhythm,aes(x=Rhythm, group=Harmony, y=mean_pleasure, color=Harmony),position=position_dodge(0.8))+
  geom_line(data=harmony_rhythm,aes(x=Rhythm, group=Harmony, y=mean_pleasure, color=Harmony),position=position_dodge(0.8))+
  # scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  # scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
  coord_cartesian(ylim = c(-3, 2.5))+
  scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
  # scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())  
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig3_pleasure_harmony_by_rhythm.pdf", plot = pleasure_harmony_rhythm_plot, width = 10, height = 8, device = "pdf")

mt_per <- quantile(Jan_new$MT, probs=c(0.1, 0.45, 0.55, 0.9))
gms_per <- quantile(Jan_new$GMS_res, probs=c(0.1, 0.45, 0.55, 0.9))
Jan_new <- Jan_new %>%
  mutate(
    # MT_group = ifelse(MT < mt_per[1], 1,
    #                        ifelse(MT < mt_per[3] & MT > mt_per[2], 2,
    #                               ifelse(MT > mt_per[4], 3, NA))),
         GMS_group = ifelse(GMS_res < gms_per[1], 1,
                           ifelse(GMS_res < gms_per[3] & GMS_res > gms_per[2], 2,
                                  ifelse(GMS_res > gms_per[4], 3, NA)))
         )
Jan_new$MT_group <- factor(Jan_new$MT_group)
Jan_new$GMS_group <- factor(Jan_new$GMS_group)



# MT:Harmony
# mt_harmony<-Jan_new %>%
#   group_by(interaction(MT_group, Harmony)) %>%
#   summarise(mean_groove = mean(standard_groove), groove_se = sd(standard_groove)/sqrt(length(standard_groove)), 
#             mean_pleasure = mean(standard_pleasure), pleasure_se = sd(standard_pleasure)/sqrt(length(standard_pleasure)))%>%
#   separate(`interaction(MT_group, Harmony)`, into = c("MT_group", "Harmony"), sep = "\\.")
# 
# df_mt_harmony <- as.data.frame(summary(pleasure_MT_harmony_contrast))
# 
# pleasure_MT_harmony_plot<-ggplot()+
#   geom_boxplot(data=Jan_new[!(is.na(Jan_new$MT_group) == TRUE),],aes(x=Harmony, y=standard_pleasure, color=MT_group, fill=MT_group), alpha=0.5, position=position_dodge(0.8))+
#   geom_point(data=mt_harmony,aes(x=Harmony, group=MT_group, y=mean_pleasure, color=MT_group),position=position_dodge(0.8))+
#   geom_line(data=mt_harmony,aes(x=Harmony, group=MT_group, y=mean_pleasure, color=MT_group),position=position_dodge(0.8))+
#   scale_colour_manual(values = c("3"="#1e5a8a","2"="#16a286", "1"="#57b2aa")) +
#   scale_fill_manual(values = c("3"="#1e5a8a","2"="#16a286", "1"="#57b2aa"))+
#   coord_cartesian(ylim = c(-3, 2.5))+
#   scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
#   # scale_x_continuous(expand = c(0,0))+
#   theme_classic()+
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank()) 
# 
# pleasure_harmony_MT_plot<-ggplot(df_mt_harmony, aes(x = Harmony, y = MT.trend, color=Harmony, fill=Harmony)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,linewidth = 1) +
#   geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5)+
#   theme_classic()+
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())+
#   coord_flip()
# pleasure_harmony_MT_plot<-ggplot(data = Jan_new)+
#   geom_smooth(aes(x=MT, y=standard_pleasure, group=Harmony, color=Harmony, fill=Harmony), method='lm',formula=y~x,se=FALSE)+
#   theme_classic()+
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())

# GMS:Harmony
gms_harmony<-Jan_new %>%
  group_by(interaction(GMS_group, Harmony)) %>%
  summarise(mean_groove = mean(standard_groove), groove_se = sd(standard_groove)/sqrt(length(standard_groove)), 
            mean_pleasure = mean(standard_pleasure), pleasure_se = sd(standard_pleasure)/sqrt(length(standard_pleasure)))%>%
  separate(`interaction(GMS_group, Harmony)`, into = c("GMS_group", "Harmony"), sep = "\\.")

df_gms_harmony <- as.data.frame(summary(pleasure_GMS_harmony_contrast))
df_gms_harmony$Harmony <- factor(df_gms_harmony$Harmony, levels = c("1","2","3","4"))

pleasure_GMS_harmony_plot<-ggplot()+
  geom_boxplot(data=Jan_new[!(is.na(Jan_new$GMS_group) == TRUE),],aes(x=Harmony, y=standard_pleasure, color=GMS_group, fill=GMS_group), alpha=0.5, position=position_dodge(0.8))+
  geom_point(data=gms_harmony,aes(x=Harmony, group=GMS_group, y=mean_pleasure, color=GMS_group),position=position_dodge(0.8))+
  geom_line(data=gms_harmony,aes(x=Harmony, group=GMS_group, y=mean_pleasure, color=GMS_group),position=position_dodge(0.8))+
  scale_colour_manual(values = c("3"="#c70038","2"="#ff2d1a", "1"="#ff713d")) +
  scale_fill_manual(values = c("3"="#c70038","2"="#ff2d1a", "1"="#ff713d"))+
  coord_cartesian(ylim = c(-3, 2.5))+
  scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
  # scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

pleasure_harmony_GMS_plot<-ggplot(df_gms_harmony, aes(x = Harmony, y = GMS_res.trend, color=Harmony, fill=Harmony)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5)+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_flip()
# pleasure_harmony_GMS_plot<-ggplot(data = Jan_new)+
#   geom_smooth(aes(x=GMS, y=standard_pleasure, group=Harmony, color=Harmony, fill=Harmony), method='lm',formula=y~x,se=FALSE)+
#   theme_classic()+
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())

complex_interaction <- grid.arrange(pleasure_harmony_GMS_plot,pleasure_GMS_harmony_plot,
                                    ncol = 2, nrow = 1)
ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig3_GMS_harmony_2.pdf", plot = complex_interaction, width = 20, height = 8, device = "pdf")

JN_plot <- function(model, predictor, moderator, interaction, range_moderator, moderate_solutions_upper, moderate_solutions_lower) {
  beta1 <- summary(model)$coefficients[paste0(predictor), "Estimate"]
  se_beta1 <- summary(model)$coefficients[paste0(predictor), "Std. Error"]
  beta3 <- summary(model)$coefficients[paste0(interaction), "Estimate"]
  se_beta3 <- summary(model)$coefficients[paste0(interaction), "Std. Error"]
  cov_beta1_beta3 <- vcov(model)[paste0(predictor), paste0(interaction)]
  se_omega <- function(M) {
    sqrt(
      se_beta1^2+
        2*M*cov_beta1_beta3+
        (M^2)*se_beta3^2
    )
  }
  x <- seq(range_moderator[1], range_moderator[2], 0.01)
  y_fit <- beta1+beta3*x
  y_upper <- y_fit+1.96*se_omega(x)
  y_lower <- y_fit-1.96*se_omega(x)

  data<-data.frame(x = x, y_fit = y_fit, y_upper = y_upper, y_lower = y_lower)
  ggplot(data, aes(x = x)) +
    geom_line(aes(y = y_fit), color = "black", size = 1) +     # 回归线
    geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2, fill = "red") + # 置信区间
    geom_vline(xintercept = moderate_solutions_upper, color = "blue", linetype = "dashed", size = 1) +
    geom_vline(xintercept = moderate_solutions_lower, color = "blue", linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
    coord_cartesian(xlim = range_moderator)+
    scale_x_continuous(expand = c(0,0))+
    labs(x = '',
         y = '') +
    theme_classic()
}
# # MT:Harmony
# MT_linearHarmony_plot<-JN_plot(Jan_pleasure_model, 'Harmony2', 'MT', 'MT:Harmony2', range_moderator=range(Jan_new$MT), moderate_solutions_upper=-0.9034151, moderate_solutions_lower=0.2005712)
# # ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig3_pleasure_harmony_by_MT.pdf", plot = MT_linearHarmony_plot, width = 10, height = 8, device = "pdf")
# # GMS:Harmony
# GMS_linearHarmony_plot<-JN_plot(Jan_pleasure_model, 'Harmony2', 'GMS', 'Harmony2:GMS', range_moderator=range(Jan_new$GMS), moderate_solutions_upper=81.74849, moderate_solutions_lower=1.321659)
# # ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig3_pleasure_harmony_by_GMS.pdf", plot = GMS_linearHarmony_plot, width = 10, height = 8, device = "pdf")
# # country:Harmony:Rhythm
# 
# Jan_summary<-Jan_new %>%
#   group_by(interaction(country, Rhythm, Harmony)) %>%
#   summarise(mean_groove = mean(standard_groove), groove_se = sd(standard_groove)/sqrt(length(standard_groove)),
#             mean_pleasure = mean(standard_pleasure), pleasure_se = sd(standard_pleasure)/sqrt(length(standard_pleasure)))%>%
#   separate(`interaction(country, Rhythm, Harmony)`, into = c("country", "Rhythm", "Harmony"), sep = "\\.")
# 
# pleasure_3way<-ggplot()+
#   geom_boxplot(data=Jan_new,aes(x=Rhythm, y=standard_pleasure, color=Harmony, fill=Harmony), alpha=0.2, position=position_dodge(0.8))+
#   geom_point(data=Jan_summary,aes(x=Rhythm, group=Harmony, y=mean_pleasure, color=Harmony),position=position_dodge(0.8))+
#   geom_line(data=Jan_summary,aes(x=Rhythm, group=Harmony, y=mean_pleasure, color=Harmony),position=position_dodge(0.8))+
#   # scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
#   # scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
#   coord_cartesian(ylim = c(-3, 2.5))+
#   scale_y_continuous(breaks = seq(-3, 2.5, by = 1), expand = c(0,0))+
#   # scale_x_continuous(expand = c(0,0))+
#   theme_classic()+
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank())+
#   facet_wrap(~country)
# # ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig3_3-way.pdf", plot = pleasure_3way, width = 10, height = 8, device = "pdf")


# save.image('D:\\Shan\\Rhythm\\code\\result\\chord_rhythm_lmer_250330.RData')

pleasure_GMS <- lmer(standard_pleasure ~ country*MT*Rhythm*Harmony+country*GMS_group*Rhythm*Harmony+country*BMRQ*Rhythm*Harmony + (1+Rhythm+Harmony|Participant.Private.ID), 
                     data = Jan_new, 
                     contrasts = contr,
                     REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

pleasure_GMS_contrasts <- emmeans(pleasure_GMS, ~ Harmony * GMS_group, pbkrtest.limit = 10000)
pleasure_GMS_contrasts_2 <- emmeans(pleasure_GMS, ~ GMS_group | Harmony , pbkrtest.limit = 10000)
pleasure_GMS_poly <- emmeans(pleasure_GMS, ~ Harmony | GMS_group, pbkrtest.limit = 10000)
pleasure_GMS_consec_eff <- summary(contrast(pleasure_GMS_contrasts_2, method = "pairwise", adjust = "bonferroni"))
pleasure_GMS_2_diff<-summary(contrast(pleasure_GMS_poly, method = 'consec',adjust='bonferroni'))
pleasure_GMS_consec_diff<-summary(contrast(pleasure_GMS_contrasts, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))
pleasure_GMS_2_diff_2<-summary(contrast(pleasure_GMS_poly, method = 'pairwise',adjust='bonferroni'))
pleasure_GMS_consec_diff_2<-summary(contrast(pleasure_GMS_contrasts, interaction = c("pairwise", 'pairwise'), adjust = "bonferroni"))

# pleasure_MT <- lmer(standard_pleasure ~ country*MT_group*Rhythm*Harmony+country*GMS*Rhythm*Harmony+country*BMRQ*Rhythm*Harmony + (1+Rhythm+Harmony|Participant.Private.ID), 
#                      data = Jan_new, 
#                      contrasts = contr,
#                      REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# pleasure_MT_contrasts <- emmeans(pleasure_MT, ~ Harmony * MT_group, pbkrtest.limit = 10000)
# pleasure_MT_contrasts_2 <- emmeans(pleasure_MT, ~ MT_group | Harmony , pbkrtest.limit = 10000)
# pleasure_MT_poly <- emmeans(pleasure_MT, ~ Harmony | MT_group, pbkrtest.limit = 10000)
# pleasure_MT_consec_eff <- summary(contrast(pleasure_MT_contrasts_2, method = "pairwise", adjust = "bonferroni"))
# pleasure_MT_2_diff<-summary(contrast(pleasure_MT_poly, method = 'consec',adjust='bonferroni'))
# pleasure_MT_consec_diff<-summary(contrast(pleasure_MT_contrasts, interaction = c("consec", 'pairwise'), adjust = "bonferroni"))
# pleasure_MT_2_diff_2<-summary(contrast(pleasure_MT_poly, method = 'pairwise',adjust='bonferroni'))
# pleasure_MT_consec_diff_2<-summary(contrast(pleasure_MT_contrasts, interaction = c("pairwise", 'pairwise'), adjust = "bonferroni"))
# 
