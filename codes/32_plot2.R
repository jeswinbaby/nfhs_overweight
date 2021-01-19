nfhs_wm <- readRDS(paste0(data_path, "nfhs_wm.rds")) %>%
  dplyr::filter(!is.na(age_category))
nfhs2_st_map <- readxl::read_excel(paste0(data_path, "support_data\\state_map.xlsx")
                                   , sheet = "nfhs_2")
# nfhs2_st_map$nfhs_2 <- as.character(nfhs2_st_map$nfhs_2)
emp <- read.csv(paste0(data_path,"contextual_variables/employment.csv"))
ntr <- read.csv(paste0(data_path,"contextual_variables/consumption.csv"))
eco <- read.csv(paste0(data_path,"contextual_variables/economic.csv"))

ovrwt_national <- nfhs_wm  %>%
  dplyr::select(state,round,ovrwt,sample_weight) %>%
  group_by(state,round) %>%
  dplyr::summarize(p = wtd.mean(ovrwt, weights = sample_weight),
                   var = wtd.var(ovrwt,weights=sample_weight,
                                 na.rm = TRUE,normwt=TRUE),
                   wts = sum(sample_weight)) %>%
  mutate(residence_type = "National")

ovrwt_regional <- nfhs_wm %>%
  mutate(residence_type = case_when(residence_type == 1 ~ "Urban",
                                    residence_type == 2 ~ "Rural",
                                    TRUE ~ NA_character_)) %>%
  dplyr::select(state,round,residence_type,ovrwt,sample_weight) %>%
  group_by(state,round,residence_type) %>%
  dplyr::summarize(p = wtd.mean(ovrwt, weights = sample_weight),
                   var = wtd.var(ovrwt,weights=sample_weight,
                                 na.rm = TRUE,normwt=TRUE),
                   wts = sum(sample_weight))

fig1_df <- bind_rows(ovrwt_national,
                     ovrwt_regional) %>%
  dplyr::mutate(prevalence = p*100) %>%
  left_join(nfhs2_st_map %>%
              dplyr::select(-state),by=c("state"="nfhs_2")) %>%
  left_join(emp, by= c("state","round")) %>%
  left_join(ntr, by= c("state","round")) %>%
  left_join(eco, by= c("state","round"))  %>%
  dplyr::mutate(round_numeric= round,
                round = factor(round,levels=c(2:4),labels=c("NFHS-2 (1998-99)",
                                                            "NFHS-3 (2005-06)",
                                                            "NFHS-4 (2015-16)")))

point_size = 1
axis_text_size = 6
axis_title_size = 6
title_size = 8
legend_title_size = 6
legend_text_size = 6
cor_size = 2

group_name = fig1_df$round %>% levels %>% as.character() %>% substr(1,6)
f <- function(grp, rlab) sub("R", paste0("r[", grp, "]"), rlab)
fig1_df$round %<>% as.character

lgdp <- fig1_df %>%
  dplyr::filter(residence_type == "National") %>%
  ggplot(data=.,aes(y=prevalence,x=log(gdp),shape=round)) +
  geom_point(size=point_size) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) + stat_cor(aes(label = paste(Map(f,group_name,..r.label..))),
                                                                                                               size=cor_size) +
  ggtitle("A") +
  xlab("Log (GDP per capita)") +
  ylab("Prevalence (%)") +
  scale_shape_manual("",values=c(0,16,3)) +
  scale_x_continuous(limits=c(8,12)) +
  theme(axis.text=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        legend.text = element_text(size=legend_text_size),
        legend.title = element_text(size=legend_title_size),
        title = element_text(size=title_size),
        legend.key.size = unit(0.6, 'cm')
  )
literacy <- fig1_df %>%
  dplyr::filter(residence_type =="National") %>%
  ggplot(data=.,aes(y=prevalence,x=literacy,shape=round)) +
  geom_point(size=point_size) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))  + stat_cor(aes(label = paste(Map(f,group_name,..r.label..))),
                                                                                                                size=cor_size) +
  ggtitle("B") +
  xlab("Literacy Rate (%)") +
  ylab("Prevalence (%)") +
  scale_shape_manual("",values=c(0,16,3))  +
  scale_x_continuous(limits=c(0,100),breaks=seq(0,100,by=20)) +
  theme(axis.text=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        legend.text = element_text(size=legend_text_size),
        legend.title = element_text(size=legend_title_size),
        title = element_text(size=title_size),
        legend.key.size = unit(0.6, 'cm')
  )


sedocc <- fig1_df %>%
  dplyr::filter(residence_type =="National") %>%
  ggplot(data=.,aes(y=prevalence,x=physical_sedentary,shape=round)) +
  geom_point(size=point_size) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))  + stat_cor(aes(label = paste(Map(f,group_name,..r.label..))),
                                                                                                                size=cor_size,label.x = c(60,60,60),label.y = c(14,11,8)) +
  ggtitle("C") +
  xlab("Sedentary Occupation (%)") +
  ylab("Prevalence (%)") +
  scale_shape_manual("",values=c(0,16,3))  +
  scale_x_continuous(limits=c(0,100),breaks=seq(0,100,by=20))  +
  theme(axis.text=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        legend.text = element_text(size=legend_text_size),
        legend.title = element_text(size=legend_title_size),
        title = element_text(size=title_size),
        legend.key.size = unit(0.6, 'cm')
  )

oil <- fig1_df %>%
  dplyr::filter(residence_type =="National") %>%
  ggplot(data=.,aes(y=prevalence,x=oils,shape=round)) +
  geom_point(size=point_size) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))  + stat_cor(aes(label = paste(Map(f,group_name,..r.label..))),
                                                                                                                size=cor_size,label.x = c(10,10,10),label.y = c(14,11,8)) +
  ggtitle("D") +
  xlab("Oil Intake (g)") +
  ylab("Prevalence (%)") +
  scale_shape_manual("",values=c(0,16,3)) +
  scale_x_continuous(limits=c(0,16),breaks=seq(0,15,by=5)) +
  theme(axis.text=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        legend.text = element_text(size=legend_text_size),
        legend.title = element_text(size=legend_title_size),
        title = element_text(size=title_size),
        legend.key.size = unit(0.6, 'cm')
  )

sugar <- fig1_df %>%
  dplyr::filter(residence_type =="National") %>%
  ggplot(data=.,aes(y=prevalence,x=sugar,shape=round)) +
  geom_point(size=point_size) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))  + stat_cor(aes(label = paste(Map(f,group_name,..r.label..))),
                                                                                                                size=cor_size,label.x = c(17,17,17),label.y = c(16,13,10)) +
  ggtitle("E") +
  xlab("Sugar Intake (g)") +
  ylab("Prevalence (%)") +
  scale_shape_manual("",values=c(0,16,3))  +
  scale_x_continuous(limits=c(0,28),breaks=seq(0,25,by=5))  +
  theme(axis.text=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        legend.text = element_text(size=legend_text_size),
        legend.title = element_text(size=legend_title_size),
        title = element_text(size=title_size),
        legend.key.size = unit(0.6, 'cm')
  )

cereal <- fig1_df %>%
  dplyr::filter(residence_type =="National") %>%
  ggplot(data=.,aes(y=prevalence,x=cereal,shape=round)) +
  geom_point(size=point_size) +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  stat_cor(aes(label = paste(Map(f,group_name,..r.label..))),
           size=cor_size,label.x = c(50,50,50),label.y = c(11,8,5)) +
  ggtitle("F") +
  xlab("Cereal Intake (g)") +
  ylab("Prevalence (%)") +
  scale_shape_manual("",values=c(0,16,3))  +
  scale_x_continuous(limits=c(50,200),breaks=seq(50,250,by=50)) +
  theme(axis.text=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        legend.text = element_text(size=legend_text_size),
        legend.title = element_text(size=legend_title_size),
        title = element_text(size=title_size),
        legend.key.size = unit(0.6, 'cm')
  )

# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html

tiff(paste0(plots_path,"/fig21.tif"),width = 150,height=120,
     units = "mm",res = 300,pointsize = 1, compression = "lzw")
ggarrange(lgdp,
          literacy,
          sedocc,
          oil,
          sugar,
          cereal,
          nrow=2,
          ncol=3,
          legend="bottom",
          common.legend = TRUE)
dev.off()