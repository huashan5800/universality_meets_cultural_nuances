setwd('D:\\Shan\\Rhythm\\code\\data')
library('biotools')
library('ez')
library('car')
library('tidyverse')
library('bruceR')
library('gridExtra')

###step1 data preparation
#Jan block
Jan_C <- read.csv(file="Jan_C.csv", header=TRUE)
Jan_D <- read.csv(file="Jan_D.csv", header=TRUE)
Jan_C$country <- rep('chinese', length(Jan_C[[1]]))
Jan_D$country <- rep('denmark', length(Jan_D[[1]]))
Jan <- rbind(Jan_C, Jan_D)
#Tomas block
Tomas_C <- read.csv(file="Tomas_C.csv", header=TRUE)
Tomas_D <- read.csv(file="Tomas_D.csv", header=TRUE)
Tomas_C$country <- rep('chinese', length(Tomas_C[[1]]))
Tomas_D$country <- rep('denmark', length(Tomas_D[[1]]))
Tomas <- rbind(Tomas_C, Tomas_D)
colnames(Tomas)[6]<-'groove'
colnames(Tomas)[9]<-'pleasure'
#Tomas$Rhythm <- str_extract(Tomas$Spreadsheet..audio, "(?<=real_)[0-9]+")
Tomas$name2 <- str_extract(Tomas$Spreadsheet..audio, "(?<=real_).*?(?=_32bit)")
#Tomas$Rhythm <- as.numeric(Tomas$Rhythm)
complexity <- read.csv(file="24DrumRhythms.csv", header=TRUE)
Tomas <- merge(Tomas[,c(1,2,6,9,10,11)], complexity[,c(2:4)], by=c('name2'))
#questionnaire
Q <- read.csv(file="musicQue.csv", header=TRUE)
Q$goldmsi14.quantised <- 8-Q$goldmsi14.quantised
Q$goldmsi17.quantised <- 8-Q$goldmsi17.quantised
Q$goldmsi23.quantised <- 8-Q$goldmsi23.quantised
Q$goldmsi25.quantised <- 8-Q$goldmsi25.quantised
Q$goldmsi27.quantised <- 8-Q$goldmsi27.quantised
Q$q02.quantised <- 6-Q$q02.quantised
Q$q05.quantised <- 6-Q$q05.quantised
Q$MT <- rowSums(Q[, c(10, 17, 19:23)])-7
Q$GMS <- rowSums(Q[, c(4:20, 23)])-18
Q$MS <- rowSums(Q[, c(25, 30, 34, 40)])
Q$EE <- rowSums(Q[, c(27, 32, 37, 42)])
Q$MR <- rowSums(Q[, c(26, 31, 35, 41)])
Q$SM <- rowSums(Q[, c(28, 33, 38, 43)])
Q$SR <- rowSums(Q[, c(24, 29, 36, 39)])
Q$BMRQ <- rowSums(Q[, c(24:43)])
#black_list(not born in Denmark)
black_D <- read.csv(file="black_D.csv", header=TRUE)
black_list <- black_D$Participant.Private.ID
demo_D <- read.csv(file="demo_D.csv", header=TRUE)
demo_C <- read.csv(file="demo_C.csv", header=TRUE)
colnames(demo_D)[3] <- "gender"
colnames(demo_D)[4] <- "age"
colnames(demo_C)[3] <- "gender"
colnames(demo_C)[4] <- "age"
demo <- rbind(demo_C, demo_D[,c(1:4)])
Tomas <- Tomas[!(Tomas$Participant.Private.ID %in% black_list),]
Jan <- Jan[!(Jan$Participant.Private.ID %in% black_list),]
Q <- Q[!(Q$Participant.Private.ID %in% black_list),]
#merge all data together (Jan and Tomas block seperately)
Q <- merge(Q, demo, by=c('Participant.Public.ID','Participant.Private.ID'))
Tomas <- merge(Tomas[,c(2,3,6:8,4,5)], Q[,c(1:3,44:53)], by=c('Participant.Public.ID','Participant.Private.ID','country'))
Jan <- merge(Jan[,c(1,2,12,4,5,8,11)], Q[,c(1:3,44:53)], by=c('Participant.Public.ID','Participant.Private.ID','country'))
#merge all groove & pleasure data together (for further scamming)
total<-rbind(Jan[,c(1:3,6,7,16,17)], Tomas[,c(1:3,6,7,16,17)])
total <- merge(total, demo, by=c('Participant.Public.ID','Participant.Private.ID','gender','age'))
#further scam (age <= 40)
total<-subset(total, age<=40)
total<-droplevels(total)
Jan<-subset(Jan, age<=40)
Jan<-droplevels(Jan)
Tomas<-subset(Tomas, age<=40)
Tomas<-droplevels(Tomas)
demo_statistics<-Q[(Q$age<=40),c(1:3,44:53)]
#factorize several variables
for (i in c(1:3,5)) {total[,i] <- factor(total[,i])}
for (i in c(1:5,16)) {Jan[,i] <- factor(Jan[,i])}
for (i in c(1:4,16)) {Tomas[,i] <- factor(Tomas[,i])}
for (i in c(1:3,12)) {demo_statistics[,i] <- factor(demo_statistics[,i])}
# summary(demo_statistics)

###step2 scam data through the std of groove and pleasure data
#1 separate Jan & Tomas block
Jan_std <- Jan %>%
  group_by(Participant.Private.ID,country) %>%
  summarize(groove_sd=sd(groove), pleasure_sd=sd(pleasure))
Tomas_std <- Tomas %>%
  group_by(Participant.Private.ID,country) %>%
  summarize(groove_sd=sd(groove), pleasure_sd=sd(pleasure))

##single tail 97.5%
std_plot<-list()
m<-0
for (i in c('Tomas_std', 'Jan_std')) {
  data <- get(i)
  for (j in c('groove_sd', 'pleasure_sd')) {
    m <- m+1
    column <- data[[j]]
    mean_val <- mean(column, na.rm = TRUE)
    median_val <- median(column, na.rm = TRUE)
    sd_val <- sd(column, na.rm = TRUE)
    print(paste("For dataset", i, "and column", j, ":"))
    print(paste("Mean:", mean_val, "Median:", median_val, "SD:", sd_val))
    std_plot[[m]]<-ggplot(data, aes_string(x=j)) +
                   geom_histogram(aes(y = after_stat(density), fill = country), binwidth = 0.3, position = "stack", alpha = 0.5) +
                   geom_density(aes(y = after_stat(density)), color = "black") +
                   geom_vline(xintercept = mean_val, color = "blue", linetype = "dashed", size = 1) +
                   geom_vline(xintercept = median_val, color = "red", linetype = "dashed", size = 1) +
                   geom_vline(xintercept = mean_val-2*sd_val, color = "blue", linetype = "dashed", size = 1) +
                   labs(title = i, x = j)+
                   coord_cartesian(ylim = c(0,0.3),xlim = c(0,45))+
                   scale_x_continuous(breaks = seq(0, 45, by = 15), expand = c(0,0))+
                   scale_y_continuous(breaks = seq(0, 0.3, by = 0.1), expand = c(0,0))+
                   scale_fill_manual(values=c("chinese"="#F7933B","denmark"="#00BFBD"))+
                   theme_classic()
    
    outlier_col <- paste0(j, "_outlier")
    data[[outlier_col]] <- ifelse(column < (mean_val - 2 * sd_val), 1, 0)
    assign(i, data)
  }
}
std_fig <- grid.arrange(std_plot[[1]],std_plot[[3]],std_plot[[2]],std_plot[[4]],nrow=2,ncol=2)
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\sup_fig1A.pdf", plot = std_fig, width = 9, height = 5, device = "pdf")

std<-merge(Tomas_std, Jan_std, by=c('Participant.Private.ID', 'country'))
std$outliersum<-std$groove_sd_outlier.x+std$pleasure_sd_outlier.x+std$groove_sd_outlier.y+std$pleasure_sd_outlier.y
std_outlier<-subset(std, std$outliersum>=1)
std_outlier<-droplevels(std_outlier)

Tomas_outlier_single_tail <- Tomas[(Tomas$Participant.Private.ID %in% std_outlier$Participant.Private.ID),]
Jan_outlier_single_tail <- Jan[(Jan$Participant.Private.ID %in% std_outlier$Participant.Private.ID),]

#Tomas' distribution of the outliers
ggplot(data=Tomas_outlier_single_tail, aes(x=groove))+
  geom_density(aes(y = after_stat(density), color = country))+
  geom_histogram(aes(y = after_stat(density), fill = country))+
  scale_fill_manual(values=c("chinese"='red',"denmark"='blue'))+
  facet_wrap(~Participant.Private.ID)
ggplot(data=Tomas_outlier_single_tail, aes(x=pleasure))+
  geom_density(aes(y = after_stat(density), color = country))+
  geom_histogram(aes(y = after_stat(density), fill = country))+
  scale_fill_manual(values=c("chinese"='red',"denmark"='blue'))+
  facet_wrap(~Participant.Private.ID)
Tomas_data_long <- reshape2::melt(Tomas_outlier_single_tail, 
                                  id.vars = c("Participant.Private.ID", "country","rhythm"), 
                                  measure.vars = c("groove", "pleasure"), 
                                  variable.name = "score_type", 
                                  value.name = "score")
Tomas_outlier_plot<-ggplot(data=Tomas_data_long, aes(x=rhythm, y=score, group=score_type, linetype = score_type, colour=country))+
                    geom_line()+
                    coord_cartesian(ylim = c(0, 100))+
                    theme(legend.position = "none")+
                    scale_color_manual(values=c("chinese"="#F7933B","denmark"="#00BFBD"))+
                    scale_linetype_manual(values=c("groove"='solid', "pleasure"='dashed'))+
                    facet_wrap(~Participant.Private.ID, ncol=1, scales = "free")+
                    theme_classic()
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\sup_fig1B.pdf", plot = Tomas_outlier_plot, width = 5, height = 45, device = "pdf", limitsize = FALSE)

#Jan's distribution ……
ggplot(data=Jan_outlier_single_tail, aes(x=groove))+
  geom_density(aes(y = after_stat(density), color = country))+
  geom_histogram(aes(y = after_stat(density), fill = country))+
  scale_fill_manual(values=c("chinese"='red',"denmark"='blue'))+
  facet_wrap(~Participant.Private.ID)

ggplot(data=Jan_outlier_single_tail, aes(x=pleasure))+
  geom_density(aes(y = after_stat(density), color = country))+
  geom_histogram(aes(y = after_stat(density), fill = country))+
  scale_fill_manual(values=c("chinese"='red',"denmark"='blue'))+
  facet_wrap(~Participant.Private.ID)

Jan_data_long <- reshape2::melt(Jan_outlier_single_tail, 
                                id.vars = c("Participant.Private.ID", "country","Rhythm", "Harmony"), 
                                measure.vars = c("groove", "pleasure"), 
                                variable.name = "score_type", 
                                value.name = "score")
Jan_outlier_plot<-ggplot(data=Jan_data_long, aes(x=Rhythm, y=score, group=interaction(Harmony, score_type), shape = score_type, colour=country))+
                         geom_line(aes(linetype=Harmony))+
                         geom_point()+
                         coord_cartesian(ylim = c(0, 100))+
                         theme(legend.position = "none")+
                         scale_color_manual(values=c("chinese"="#F7933B","denmark"="#00BFBD"))+
                         scale_shape_manual(values = c(4, 20)) +
                         facet_wrap(~Participant.Private.ID, ncol=1, scales = "free")+
                         theme_classic()
# ggsave("D:\\Shan\\Rhythm\\code\\raw_fig\\sup_fig1C.pdf", plot = Jan_outlier_plot, width = 5, height = 45, device = "pdf", limitsize = FALSE)

###step3 apply the data refresh to the Jan & Tomas
Tomas_new <- Tomas[!(Tomas$Participant.Private.ID %in% std_outlier$Participant.Private.ID),]
Jan_new <- Jan[!(Jan$Participant.Private.ID %in% std_outlier$Participant.Private.ID),]
stats_new <- demo_statistics[!(demo_statistics$Participant.Private.ID %in% std_outlier$Participant.Private.ID),]
Tomas_new <- droplevels(Tomas_new)
Jan_new <- droplevels(Jan_new)
stats_new <- droplevels(stats_new)


# write_excel_csv(Tomas_new, 'D:\\Shan\\Rhythm\\code\\data\\drum_rhythm_data.csv')
# write_excel_csv(Jan_new, 'D:\\Shan\\Rhythm\\code\\data\\chord_rhythm_data.csv')
# write_excel_csv(stats_new, 'D:\\Shan\\Rhythm\\code\\data\\questionnaire_data.csv')
