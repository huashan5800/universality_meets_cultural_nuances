setwd('D:\\Shan\\Rhythm\\code\\data')
library('lme4')
library('lmerTest')
library('tidyverse')
library('bruceR')
library('gridExtra')

Tomas_new <- read.csv(file="drum_rhythm_data.csv", header=TRUE)

Tomas_new <- Tomas_new %>%
  group_by(country) %>%
  mutate(standard_groove = scale(groove), standard_pleasure = scale(pleasure),
         standard_MT = scale(MT), standard_GMS = scale(GMS), standard_BMRQ = scale(BMRQ))
Tomas_new[,c(8:15)]<-scale(Tomas_new[,c(8:15)])
Tomas_new$wSI_scaled <- scale(Tomas_new$wSI)
reg_lm <- lm(GMS~MT, data = Tomas_new)
tmp <- as.matrix(reg_lm$residuals)
Tomas_new$GMS_res <- tmp

contr = list(country='contr.sum')

### for groove
Tomas_groove_poly_model <- lmer(standard_groove ~ country*MT*poly(wSI,2)+country*GMS_res*poly(wSI,2)+country*BMRQ*poly(wSI,2)+(1+poly(wSI,2)|Participant.Private.ID),
                                data = Tomas_new,
                                contrasts = contr,
                                REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
isSingular(Tomas_groove_poly_model)
rand(Tomas_groove_poly_model)

Tomas_groove_linear_model <- lmer(standard_groove ~ country*MT*wSI_scaled+country*GMS_res*wSI_scaled+country*BMRQ*wSI_scaled+(1+wSI_scaled|Participant.Private.ID),
                                   data = Tomas_new,
                                   contrasts = contr,
                                   REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
isSingular(Tomas_groove_linear_model)
rand(Tomas_groove_linear_model)

anova(Tomas_groove_poly_model, Tomas_groove_linear_model)
HLM_summary(Tomas_groove_poly_model)
Tomas_groove_model_adjusted <- anova(Tomas_groove_poly_model)
Tomas_groove_model_adjusted$q_value <- p.adjust(Tomas_groove_model_adjusted[,c("Pr(>F)" )], method = "fdr")
Tomas_groove_model_adjusted <- round(Tomas_groove_model_adjusted,3)

## post-hoc (the poly() is not suitable for emtrends, so use (wSI_1+wSI_2) to represent poly(wSI))
poly_vars <- poly(Tomas_new$wSI, 2)
poly_df <- data.frame(wSI_1 = poly_vars[, 1], wSI_2 = poly_vars[, 2])
Tomas_new <- cbind(Tomas_new, poly_df)

Tomas_groove_simple_model<-lmer(standard_groove~country*MT*(wSI_1+wSI_2)+country*GMS_res*(wSI_1+wSI_2)+country*BMRQ*(wSI_1+wSI_2)+(1+wSI_1+wSI_2|Participant.Private.ID),
                                data = Tomas_new,
                                contrasts = contr,
                                REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
rand(Tomas_groove_simple_model)
HLM_summary(Tomas_groove_simple_model)
Tomas_groove_simple_model<-standardize(Tomas_groove_simple_model)
em_Tomas_groove_model_1 <- emtrends(Tomas_groove_simple_model, 'country', var='wSI_1', pbkrtest.limit = 10000)
print(em_Tomas_groove_model_1)
pairs(em_Tomas_groove_model_1)
em_Tomas_groove_model_2 <- emtrends(Tomas_groove_simple_model, 'country', var='wSI_2', pbkrtest.limit = 10000)
print(em_Tomas_groove_model_2)
pairs(em_Tomas_groove_model_2)


JN_threeway <- function(data, model, predictor, moderator, moderator2, interaction1, interaction2, interaction, moderate2_value) {
  # beta0+beta1*X+beta2*M1+beta3*M2+beta4*M1*X+beta5*M2*x+beta6*M1*M2+beta7*M1*M2*X
  # M1: GMS/MT
  # M2: country
  # X: poly(wSI,2)[2]
  # interaction: M1*M2*X
  # interaction1: M1*X
  # interaction2: M2*X
  beta1 <- summary(model)$coefficients[paste0(predictor), "Estimate"]
  se_beta1 <- summary(model)$coefficients[paste0(predictor), "Std. Error"]
  
  beta4 <- summary(model)$coefficients[paste0(interaction1), "Estimate"]
  se_beta4 <- summary(model)$coefficients[paste0(interaction1), "Std. Error"]
  
  beta5 <- summary(model)$coefficients[paste0(interaction2), "Estimate"]
  se_beta5 <- summary(model)$coefficients[paste0(interaction2), "Std. Error"]
  
  beta7 <- summary(model)$coefficients[paste0(interaction), "Estimate"]
  se_beta7 <- summary(model)$coefficients[paste0(interaction), "Std. Error"]
  
  cov_beta1_beta4 <- vcov(model)[paste0(predictor), paste0(interaction1)]
  cov_beta1_beta5 <- vcov(model)[paste0(predictor), paste0(interaction2)]
  cov_beta1_beta7 <- vcov(model)[paste0(predictor), paste0(interaction)]
  
  cov_beta4_beta5 <- vcov(model)[paste0(interaction1), paste0(interaction2)]
  cov_beta4_beta7 <- vcov(model)[paste0(interaction1), paste0(interaction)]
  
  cov_beta5_beta7 <- vcov(model)[paste0(interaction2), paste0(interaction)]
  
  se_omega <- function(M1,M2) {
    sqrt(
      se_beta1^2 +
      (M2^2) * se_beta5^2 +
      (M1^2) * se_beta4^2 +
      ((M2 * M1)^2) * se_beta7^2 +
      2 * M2 * cov_beta1_beta5 +
      2 * M1 * cov_beta1_beta4 +
      2 * M2 * M1 * cov_beta1_beta7 +
      2 * M2 * M1 * cov_beta4_beta5 +
      2 * (M2^2) * M1 * cov_beta5_beta7 +
      2 * (M1^2) * M2 * cov_beta4_beta7
    )
  }
  
  solve_moderate1_upper <- function(M2) {
    f <- function(M1) {
      y <- beta1 + beta5 * M2 + (beta4 + beta7 * M2) * M1
      se_y <- se_omega(M1, M2)
      return(y / se_y - 1.96)
    }
    result <- uniroot(f, lower = -1e+6, upper = 1e+6)$root
    return(result)
  }
  
  solve_moderate1_lower <- function(M2) {
    f <- function(M1) {
      y <- beta1 + beta5 * M2 + (beta4 + beta7 * M2) * M1
      se_y <- se_omega(M1, M2)
      return(y / se_y + 1.96)
    }
    result <- uniroot(f, lower = -1e+6, upper = 1e+6)$root
    return(result)
  }
  
  moderate1_solutions_upper <- sapply(moderate2_value, solve_moderate1_upper)
  moderate1_solutions_lower <- sapply(moderate2_value, solve_moderate1_lower)
  
  return(list(
    moderate1_solutions_upper,
    moderate1_solutions_lower
  ))
}

# country = Chinese
JN_threeway(Tomas_new, Tomas_groove_simple_model, 'wSI_2', 'MT', 'country1', 'MT:wSI_2', 'country1:wSI_2', 'country1:MT:wSI_2', moderate2_value=1)

# country = Denmark (no root, so the t > 1.96 or t < 1.96)
JN_threeway(Tomas_new, Tomas_groove_simple_model, 'wSI_2', 'MT', 'country1', 'MT:wSI_2', 'country1:wSI_2', 'country1:MT:wSI_2', moderate2_value=-1)


## plot
drum_groove_country<-ggplot()+
  geom_line(data=Tomas_new, aes(x=wSI, group = Participant.Private.ID,y = fitted(Tomas_groove_poly_model), color = country), alpha=0.1)+
  geom_smooth(data=Tomas_new, aes(x=wSI, y=standard_groove, group=country, colour=country, fill=country),method='lm',formula=y~poly(x,2),se=TRUE,show.legend=TRUE)+
# geom_smooth(data=Tomas_new, aes(x=wSI, y=standard_groove),method='lm',formula=y~poly(x,2),se=TRUE,colour='black', show.legend=FALSE)+
  scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  coord_cartesian(ylim = c(-1.5, 1))+
  scale_y_continuous(breaks = seq(-1.5, 1, by = 0.5), expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig4A_drum_groove.pdf", plot = drum_groove_country, width = 8, height = 8, device = "pdf")

JN_plot_3way <- function(model, predictor, moderator, moderator2, range_moderator, interaction1, interaction2, interaction, chinese_moderate_solutions_upper, chinese_moderate_solutions_lower) {
  beta1 <- summary(model)$coefficients[paste0(predictor), "Estimate"]
  se_beta1 <- summary(model)$coefficients[paste0(predictor), "Std. Error"]
  
  beta4 <- summary(model)$coefficients[paste0(interaction1), "Estimate"]
  se_beta4 <- summary(model)$coefficients[paste0(interaction1), "Std. Error"]
  
  beta5 <- summary(model)$coefficients[paste0(interaction2), "Estimate"]
  se_beta5 <- summary(model)$coefficients[paste0(interaction2), "Std. Error"]
  
  beta7 <- summary(model)$coefficients[paste0(interaction), "Estimate"]
  se_beta7 <- summary(model)$coefficients[paste0(interaction), "Std. Error"]
  
  cov_beta1_beta4 <- vcov(model)[paste0(predictor), paste0(interaction1)]
  cov_beta1_beta5 <- vcov(model)[paste0(predictor), paste0(interaction2)]
  cov_beta1_beta7 <- vcov(model)[paste0(predictor), paste0(interaction)]
  
  cov_beta4_beta5 <- vcov(model)[paste0(interaction1), paste0(interaction2)]
  cov_beta4_beta7 <- vcov(model)[paste0(interaction1), paste0(interaction)]
  
  cov_beta5_beta7 <- vcov(model)[paste0(interaction2), paste0(interaction)]
  
  se_omega <- function(M1,M2) {
    sqrt(
      se_beta1^2 +
        (M2^2) * se_beta5^2 +
        (M1^2) * se_beta4^2 +
        ((M2 * M1)^2) * se_beta7^2 +
        2 * M2 * cov_beta1_beta5 +
        2 * M1 * cov_beta1_beta4 +
        2 * M2 * M1 * cov_beta1_beta7 +
        2 * M2 * M1 * cov_beta4_beta5 +
        2 * (M2^2) * M1 * cov_beta5_beta7 +
        2 * (M1^2) * M2 * cov_beta4_beta7
    )
  }
  
  Mo1 <- seq(range_moderator[1], range_moderator[2], 0.01)
  y_fit_chinese <- beta1+beta5 + (beta4+beta7)*Mo1
  y_fit_denmark <- beta1-beta5 + (beta4-beta7)*Mo1
  y_upper_chinese <- y_fit_chinese+1.96*se_omega(Mo1, 1)
  y_lower_chinese <- y_fit_chinese-1.96*se_omega(Mo1, 1)
  y_upper_denmark <- y_fit_denmark+1.96*se_omega(Mo1, -1)
  y_lower_denmark <- y_fit_denmark-1.96*se_omega(Mo1, -1)
  
  data<-data.frame(Mo1 = rep(Mo1,2), 
                   y_fit = c(y_fit_chinese, y_fit_denmark), 
                   country= c(rep('chinese',length(y_fit_chinese)), rep('denmark',length(y_fit_denmark))), 
                   y_upper = c(y_upper_chinese, y_upper_denmark), 
                   y_lower = c(y_lower_chinese, y_lower_denmark))
  
  data$country <- factor(data$country)
  ggplot(data, aes(x = Mo1, group = country)) +
    geom_line(aes(y = y_fit, color = country), size = 1) +     # 回归线
    geom_ribbon(aes(ymin = y_lower, ymax = y_upper, fill = country), alpha = 0.2) + # 置信区间
    geom_vline(xintercept = chinese_moderate_solutions_upper, color = "#F7933B", linetype = "dashed", size = 1) +
    geom_vline(xintercept = chinese_moderate_solutions_lower, color = "#F7933B", linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
    scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
    scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD"))+
    coord_cartesian(xlim = range_moderator)+
    scale_x_continuous(expand = c(0,0))+
    labs(x = '',
         y = '') +
    theme_classic()
}
## JN_3way
library('ggplot2')
JN_3way_plot<-JN_plot_3way(Tomas_groove_simple_model, 'wSI_2', 'MT', 'country1', range_moderator=range(Tomas_new$MT), 'MT:wSI_2', 'country1:wSI_2', 'country1:MT:wSI_2', chinese_moderate_solutions_upper=3.217782, chinese_moderate_solutions_lower=0.5469966)
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig4_drum_3way.pdf", plot = JN_3way_plot, width = 10, height = 8, device = "pdf")

### for pleasure
Tomas_pleasure_poly_model <- lmer(standard_pleasure ~ country*MT*poly(wSI,2)+country*GMS_res*poly(wSI,2)+country*BMRQ*poly(wSI,2)+(1+poly(wSI,2)|Participant.Private.ID),
                                  data = Tomas_new,
                                  contrasts = contr,
                                  REML = TRUE,
                                  lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
isSingular(Tomas_pleasure_poly_model)
rand(Tomas_pleasure_poly_model)
HLM_summary(Tomas_pleasure_poly_model)
Tomas_pleasure_linear_model <- lmer(standard_pleasure ~ country*MT*wSI_scaled+country*GMS_res*wSI_scaled+country*BMRQ*wSI_scaled+(1+wSI_scaled|Participant.Private.ID),
                                  data = Tomas_new,
                                  contrasts = contr,
                                  REML = TRUE,
                                  lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
isSingular(Tomas_pleasure_linear_model)
rand(Tomas_pleasure_linear_model)

anova(Tomas_pleasure_poly_model, Tomas_pleasure_linear_model)

Tomas_pleasure_model_adjusted <- anova(Tomas_pleasure_poly_model)
Tomas_pleasure_model_adjusted$q_value <- p.adjust(Tomas_pleasure_model_adjusted[,c("Pr(>F)" )], method = "fdr")
Tomas_pleasure_model_adjusted <- round(Tomas_pleasure_model_adjusted,3)

## post-hoc
Tomas_pleasure_simple_model<-lmer(standard_pleasure~country*MT*(wSI_1+wSI_2)+country*GMS_res*(wSI_1+wSI_2)+country*BMRQ*(wSI_1+wSI_2)+(1+wSI_1+wSI_2|Participant.Private.ID),
                                  data = Tomas_new,
                                  contrasts = contr,
                                  REML = TRUE,
                                  lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
rand(Tomas_pleasure_simple_model)
HLM_summary(Tomas_pleasure_simple_model)
Tomas_pleasure_simple_model<-standardize(Tomas_pleasure_simple_model)
em_Tomas_pleasure_model_1 <- emtrends(Tomas_pleasure_simple_model, 'country', var='wSI_1', pbkrtest.limit = 10000)
print(em_Tomas_pleasure_model_1)
pairs(em_Tomas_pleasure_model_1)
em_Tomas_pleasure_model_2 <- emtrends(Tomas_pleasure_simple_model, 'country', var='wSI_2', pbkrtest.limit = 10000)
print(em_Tomas_pleasure_model_2)
pairs(em_Tomas_pleasure_model_2)

## plot
drum_pleasure<-ggplot()+
  geom_line(data=Tomas_new, aes(x=wSI, group = Participant.Private.ID,y = fitted(Tomas_pleasure_poly_model), color = country), alpha=0.1)+
  geom_smooth(data=Tomas_new, aes(x=wSI, y=standard_pleasure, group=country, colour=country, fill=country),method='lm',formula=y~poly(x,2),se=TRUE,show.legend=TRUE)+
  # geom_smooth(data=Tomas_new, aes(x=wSI, y=standard_pleasure),method='lm',formula=y~poly(x,2),se=TRUE,colour='black', show.legend=FALSE)+
  scale_colour_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  scale_fill_manual(values = c("chinese"="#F7933B","denmark"="#00BFBD")) +
  coord_cartesian(ylim = c(-1.5, 1))+
  scale_y_continuous(breaks = seq(-1.5, 1, by = 0.5), expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig5A_drum_pleasure.pdf", plot = drum_pleasure, width = 8, height = 8, device = "pdf")


# save.image('D:\\Shan\\Rhythm\\code\\result\\drum_rhythm_lmer.RData')