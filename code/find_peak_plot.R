setwd('D:\\Shan\\Rhythm\\code\\data')
library('lme4')
library('lmerTest')
library('dplyr')
library('ggplot2')

load("D:/Shan/Rhythm/code/result/peak_result_20250328.RData")

groove_peak <- data.frame()
pleasure_peak <- data.frame()
for (i in c(1:1000)){
  groove_peak <- rbind(groove_peak, results[[i,2]])
  pleasure_peak <- rbind(pleasure_peak, results[[i,3]])
}

groove_peak$delta <- groove_peak$chinese-groove_peak$denmark

m<-ncol(groove_peak)
for (i in c(MT_range)) {
  name_sufix <- paste0('MT_', i)
  den_name <- paste0('denmark_', name_sufix)
  chn_name <- paste0('chinese_', name_sufix)
  groove_peak[,c(m+1)] <- groove_peak[,c(chn_name)] - groove_peak[,c(den_name)]
  colnames(groove_peak)[m+1] <- paste0('delta_',name_sufix)
  m<-m+1}

groove_peak_plot<-ggplot(data=groove_peak, aes(x = delta, y = after_stat(density))) +
  geom_density(size=1)+
  geom_histogram(fill='red',position = "identity", alpha=0.2)+
  geom_vline(xintercept = sort(groove_peak$delta)[25], color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = sort(groove_peak$delta)[975], color = "blue", linetype = "dashed", size = 1) +
  labs(x = 'vertex difference in wSI')+
  coord_cartesian(ylim = c(0,0.3))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.1), expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig4B_groove_peak.pdf", plot = groove_peak_plot, width = 10, height = 8, device = "pdf")


n<-1
groove_peak_plot<-list()
new_data <- data.frame()
new_MT_range<-MT_range[(MT_range<=0.5469966)]
for (i in c(new_MT_range)) {
  name <- paste0('delta_MT_', i)
  print(name)
  groove_peak_plot[[n]] <- ggplot(data = groove_peak, aes_string(x = paste0("`", name, "`"))) + 
    geom_density(size = 1, aes(y = ..density..)) +  
    geom_histogram(aes(y = ..density..), fill = 'red', position = "identity", alpha = 0.2) +
    geom_vline(xintercept = sort(groove_peak[,c(name)])[25], color = "blue", linetype = "dashed", size = 1) +
    geom_vline(xintercept = sort(groove_peak[,c(name)])[975], color = "blue", linetype = "dashed", size = 1) +
    labs(x = 'vertex difference in wSI') +
    coord_cartesian(ylim = c(0, 0.3), xlim = c(-30,20)) +
    scale_x_continuous(breaks = seq(-25, 25, by = 5),expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 0.3, by = 0.1), expand = c(0, 0)) +
    labs(title = paste0('MT = ', i))+
    theme_classic()
  temp_data <- as.data.frame(groove_peak[[name]])
  new_data <- rbind(new_data, temp_data)
  n <- n+1
}

library('gridExtra')
groove_peak_test<-grid.arrange(groove_peak_plot[[1]], groove_peak_plot[[2]], groove_peak_plot[[3]],
                               groove_peak_plot[[4]], groove_peak_plot[[5]], groove_peak_plot[[6]],
                               groove_peak_plot[[7]], groove_peak_plot[[8]], groove_peak_plot[[9]],
                               groove_peak_plot[[10]], groove_peak_plot[[11]], groove_peak_plot[[12]],
                               groove_peak_plot[[13]], groove_peak_plot[[14]], groove_peak_plot[[15]],
                               groove_peak_plot[[16]], groove_peak_plot[[17]], groove_peak_plot[[18]],
                               groove_peak_plot[[19]], groove_peak_plot[[20]], groove_peak_plot[[21]],
                               nrow=7,ncol=3)
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig4_groove_peak_MT_test.pdf", plot = groove_peak_test, width = 24, height = 42, device = "pdf", limitsize = FALSE)




colnames(new_data)[1]<-"delta"
new_data$MT <- sort(rep(new_MT_range, 1000))
new_data$iteration <- rep(c(1:1000), 21)

moderation <- lmer(delta ~ MT+(1+MT|iteration), 
                   data = new_data, 
                   REML = TRUE,lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
HLM_summary(moderation)

# 
# ggplot(data=new_data, aes(x=MT, y=delta, group=iteration))+
#   geom_line(aes(colour = iteration))

groove_peak_MT<-ggplot()+
  geom_line(data=new_data, aes(x=MT, group = iteration,y = fitted(moderation), colour = iteration), alpha=0.1)+
  geom_smooth(data=new_data, aes(x=MT, y=delta), method='lm',formula=y~x,colour='black', fill='red', alpha=0.2,se=TRUE)+
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig4_groove_peak_MT.pdf", plot = groove_peak_MT, width = 8, height = 8, device = "pdf")

pleasure_peak$delta <- pleasure_peak$chinese-pleasure_peak$denmark

pleasure_peak_plot<-ggplot(data=pleasure_peak, aes(x = delta, y = after_stat(density))) +
  geom_density(size=1)+
  geom_histogram(fill='red',position = "identity", alpha=0.2)+
  geom_vline(xintercept = sort(pleasure_peak$delta)[25], color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = sort(pleasure_peak$delta)[975], color = "blue", linetype = "dashed", size = 1) +
  # labs(x = 'vertex difference in wSI')+
  coord_cartesian(ylim = c(0,0.3))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.1), expand = c(0,0))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\fig5B_pleasure_peak.pdf", plot = pleasure_peak_plot, width = 10, height = 8, device = "pdf")
