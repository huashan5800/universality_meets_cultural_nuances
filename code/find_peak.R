setwd('/home/huashan/cross_culture_rhythm') #run on server (not pc)
library('lme4')
library('lmerTest')
library('dplyr')
library('ggplot2')

Tomas_new <- read.csv(file="drum_rhythm_data.csv", header=TRUE)

for (i in c(1:4,16)) {Tomas_new[,i] <- factor(Tomas_new[,i])}

Tomas_new <- Tomas_new %>%
  group_by(country) %>%
  mutate(standard_groove = scale(groove), standard_pleasure = scale(pleasure))
Tomas_new[,c(8:15)]<-scale(Tomas_new[,c(8:15)])

contr = list(country='contr.sum')

### bootstrap for peak
# sampling function
bootstrap_sample <- function(data1, data2, max_repeats) {
  repeat {
    sample_1 <- sample(data1, length(data1), replace = TRUE)
    sample_2 <- sample(data2, length(data2), replace = TRUE)
    repeat_counts1 <- table(sample_1)
    repeat_counts2 <- table(sample_2)
    if (all(repeat_counts1 <= max_repeats) & all(repeat_counts2 <= max_repeats)) {
      sample<-c(sample_1, sample_2)
      return(sample)
    }
  }
}

target_iterations <- 1000
max_repeats <- 50

Chinese<-droplevels(Tomas_new[(Tomas_new$country=='chinese'),])
cp<-levels(Chinese$Participant.Private.ID)

Danish<-droplevels(Tomas_new[(Tomas_new$country=='denmark'),])
dp<-levels(Danish$Participant.Private.ID)

sample_id <- c()
unique_samples <- data.frame(matrix(NA, nrow = 1000, ncol = 1))
wSI_range <- seq(0,85,0.01)
MT_range <- seq(range(Tomas_new$MT)[1], range(Tomas_new$MT)[2], 0.1)
GMS_range <- seq(range(Tomas_new$GMS)[1], range(Tomas_new$GMS)[2], 0.1)
# groove_diff<-data.frame(matrix(NA, nrow = 1000, ncol = 1))
# pleasure_diff<-data.frame(matrix(NA, nrow = 1000, ncol = 1))

library('doParallel')
Sys.setenv("OPENBLAS_NUM_THREADS" = "1")
Sys.setenv("OMP_NUM_THREADS" = "1")
Sys.setenv("MKL_NUM_THREADS" = "1")
start_time <- Sys.time()
cl <- makeCluster(40, type = 'SOCK')
registerDoParallel(cl, cores=40)

library('foreach')
results <- foreach (i = 1:target_iterations, .combine = 'rbind', .packages = c('dplyr', 'lme4', 'lmerTest'), .verbose=TRUE) %dopar% {
  sample_id <- bootstrap_sample(cp,dp,50)
  # record sample using id
  unique_id <- paste(sort(sample_id), collapse = "-")
  sample_id <- as.data.frame(sample_id)
  colnames(sample_id)[1] <- 'Participant.Private.ID'
  sample_id$new_id <- c(1:223)
  sample_id$new_id <- factor(sample_id$new_id)
  unique_samples[i,1] <- unique_id
  sample_data <- merge(sample_id, Tomas_new, by = c('Participant.Private.ID'))
  model_groove<-lmer(standard_groove ~ country*MT*poly(wSI,2)+country*GMS*poly(wSI,2)+country*BMRQ*poly(wSI,2)+(1+poly(wSI,2)|new_id),
                     data = sample_data,
                     contrasts = contr,
                     REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  
  model_pleasure<-lmer(standard_pleasure ~ country*MT*poly(wSI,2)+country*GMS*poly(wSI,2)+country*BMRQ*poly(wSI,2)+(1+poly(wSI,2)|new_id),
                       data = sample_data,
                       contrasts = contr,
                       REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  
  # construct a new dataframe to recover the curve to find the peak
  groove_peak <- data.frame(matrix(ncol = 2, nrow = 1))
  pleasure_peak <- data.frame(matrix(ncol = 2, nrow = 1))
  colnames(groove_peak) <- c('chinese', 'denmark')
  colnames(pleasure_peak) <- c('chinese', 'denmark')
  new_data_chinese <- data.frame(wSI = wSI_range, 
                                 country = rep('chinese', length(wSI_range)), 
                                 MT = rep(mean(sample_data[c(sample_data$country == 'chinese'),]$MT), length(wSI_range)),
                                 GMS = rep(mean(sample_data[c(sample_data$country == 'chinese'),]$GMS), length(wSI_range)),
                                 BMRQ = rep(mean(sample_data[c(sample_data$country == 'chinese'),]$BMRQ), length(wSI_range)))
  new_data_denmark <- data.frame(wSI = wSI_range, 
                                 country = rep('denmark', length(wSI_range)),
                                 MT = rep(mean(sample_data[c(sample_data$country == 'denmark'),]$MT), length(wSI_range)),
                                 GMS = rep(mean(sample_data[c(sample_data$country == 'denmark'),]$GMS), length(wSI_range)),
                                 BMRQ = rep(mean(sample_data[c(sample_data$country == 'denmark'),]$BMRQ), length(wSI_range)))
  new_data_chinese_MT <- data.frame(wSI = sort(rep(wSI_range, length(MT_range))),
                                    country = rep('chinese', length(MT_range)*length(wSI_range)),
                                    MT = rep(MT_range, length(wSI_range)),
                                    GMS = rep(mean(sample_data[c(sample_data$country == 'chinese'),]$GMS), length(MT_range)*length(wSI_range)),
                                    BMRQ = rep(mean(sample_data[c(sample_data$country == 'chinese'),]$BMRQ), length(MT_range)*length(wSI_range)))
  new_data_chinese_GMS <- data.frame(wSI = sort(rep(wSI_range, length(GMS_range))),
                                     country = rep('chinese', length(GMS_range)*length(wSI_range)),
                                     MT = rep(mean(sample_data[c(sample_data$country == 'chinese'),]$MT), length(GMS_range)*length(wSI_range)),
                                     GMS = rep(GMS_range, length(wSI_range)),
                                     BMRQ = rep(mean(sample_data[c(sample_data$country == 'chinese'),]$BMRQ), length(GMS_range)*length(wSI_range)))
  new_data_denmark_MT <- data.frame(wSI = sort(rep(wSI_range, length(MT_range))),
                                    country = rep('denmark', length(MT_range)*length(wSI_range)),
                                    MT = rep(MT_range, length(wSI_range)),
                                    GMS = rep(mean(sample_data[c(sample_data$country == 'denmark'),]$GMS), length(MT_range)*length(wSI_range)),
                                    BMRQ = rep(mean(sample_data[c(sample_data$country == 'denmark'),]$BMRQ), length(MT_range)*length(wSI_range)))
  new_data_denmark_GMS <- data.frame(wSI = sort(rep(wSI_range, length(GMS_range))),
                                     country = rep('denmark', length(GMS_range)*length(wSI_range)),
                                     MT = rep(mean(sample_data[c(sample_data$country == 'denmark'),]$MT), length(GMS_range)*length(wSI_range)),
                                     GMS = rep(GMS_range, length(wSI_range)),
                                     BMRQ = rep(mean(sample_data[c(sample_data$country == 'denmark'),]$BMRQ), length(GMS_range)*length(wSI_range)))
  
  new_data_chinese$predicted_groove <- predict(model_groove, newdata = new_data_chinese, re.form = NA)
  new_data_chinese$predicted_pleasure <- predict(model_pleasure, newdata = new_data_chinese, re.form = NA)
  new_data_denmark$predicted_groove <- predict(model_groove, newdata = new_data_denmark, re.form = NA)
  new_data_denmark$predicted_pleasure <- predict(model_pleasure, newdata = new_data_denmark, re.form = NA)
  
  new_data_chinese_MT$predicted_groove <- predict(model_groove, newdata = new_data_chinese_MT, re.form = NA)
  new_data_denmark_MT$predicted_groove <- predict(model_groove, newdata = new_data_denmark_MT, re.form = NA)
  
  new_data_chinese_GMS$predicted_pleasure <- predict(model_pleasure, newdata = new_data_chinese_GMS, re.form = NA)
  new_data_denmark_GMS$predicted_pleasure <- predict(model_pleasure, newdata = new_data_denmark_GMS, re.form = NA)
  
  new_data_GMS <- merge(new_data_chinese_GMS, new_data_denmark_GMS, by=c('wSI', 'GMS'))
  new_data_GMS$predicted_pleasure <- (new_data_GMS$predicted_pleasure.x+new_data_GMS$predicted_pleasure.y)/2
  
  ### store the max_wSI
  groove_peak$chinese <- new_data_chinese$wSI[which.max(new_data_chinese$predicted_groove)]
  pleasure_peak$chinese <- new_data_chinese$wSI[which.max(new_data_chinese$predicted_pleasure)]
  groove_peak$denmark <- new_data_denmark$wSI[which.max(new_data_denmark$predicted_groove)]
  pleasure_peak$denmark <- new_data_denmark$wSI[which.max(new_data_denmark$predicted_pleasure)]
  
  m = 3
  for (i in c(MT_range)) {
    groove_peak[1,m] <- new_data_chinese_MT[c(new_data_chinese_MT$MT==i),]$wSI[which.max(new_data_chinese_MT[c(new_data_chinese_MT$MT==i),]$predicted_groove)]
    groove_peak[1,m+1] <- new_data_denmark_MT[c(new_data_denmark_MT$MT==i),]$wSI[which.max(new_data_denmark_MT[c(new_data_denmark_MT$MT==i),]$predicted_groove)]
    colnames(groove_peak)[m] <- paste('chinese_MT', i, sep='_')
    colnames(groove_peak)[m+1] <- paste('denmark_MT', i, sep='_')
    m <- m+2
  }
  
  m = 3
  for (i in c(GMS_range)) {
    pleasure_peak[1,m] <- new_data_GMS[c(new_data_GMS$GMS==i),]$wSI[which.max(new_data_GMS[c(new_data_GMS$GMS==i),]$predicted_pleasure)]
    colnames(pleasure_peak)[m] <- paste('GMS', i, sep='_')
    m <- m+1
  }
  
  list(sample=unique_id,
       groove=groove_peak, 
       pleasure=groove_peak)
}

end_time <- Sys.time()

# save.image('/home/huashan/cross_culture_rhythm/peak_result_20250328.RData')

# groove_diff<-do.call(rbind, results[,c('groove_diff')])
# groove_diff<-as.data.frame(groove_diff)
# 
# pleasure_diff<-do.call(rbind, results[,c('pleasure_diff')])
# pleasure_diff<-as.data.frame(pleasure_diff)
# 
# ci1_groove<-sort(groove_diff[,1])[50]
# ci2_groove<-sort(groove_diff[,1])[950]
# ci1_pleasure<-sort(pleasure_diff[,1])[50]
# ci2_pleasure<-sort(pleasure_diff[,1])[950]
# 
# groove_peak<-ggplot(data=groove_diff, aes(x = V1, y = after_stat(density))) +
#              geom_density(size=1)+
#              geom_histogram(fill='red',position = "identity", alpha=0.2)+
#              geom_vline(xintercept = ci1_groove, color = "blue", linetype = "dashed", size = 1) +
#              geom_vline(xintercept = ci2_groove, color = "blue", linetype = "dashed", size = 1) +
#              labs(x = 'vertex difference in wSI')+
#              coord_cartesian(ylim = c(0,0.4))+
#              scale_x_continuous(expand = c(0,0))+
#              scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), expand = c(0,0))+
#              theme_classic()
# # ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig4B_groove_peak.pdf", plot = groove_peak, width = 12, height = 8, device = "pdf")
# 
# pleasure_peak<-ggplot(data=pleasure_diff, aes(x = V1, y = after_stat(density))) +
#                geom_density(size=1)+
#                geom_histogram(fill='red',position = "identity", alpha=0.2)+
#                geom_vline(xintercept = ci1_pleasure, color = "blue", linetype = "dashed", size = 1) +
#                geom_vline(xintercept = ci2_pleasure, color = "blue", linetype = "dashed", size = 1) +
#                labs(x = 'vertex difference in wSI')+
#                coord_cartesian(ylim = c(0,0.4))+
#                scale_x_continuous(expand = c(0,0))+
#                scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), expand = c(0,0))+
#                theme_classic()
# # ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig5B_pleasure_peak.pdf", plot = pleasure_peak, width = 12, height = 8, device = "pdf")
# 
# final_end_time <- Sys.time()
# save.image('/home/huashan/cross_culture_rhythm/peak_result.RData')