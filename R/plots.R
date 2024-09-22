library(ggplot2)
library(scales)



# #birth_year diploma
# df%>%
#   mutate(year=year%>%as.character()%>%as.Date("%Y"))%>%
#   group_by(year, kid_univ_diploma)%>%
#   count()%>%
#   mutate(kid_univ_diploma=ifelse(kid_univ_diploma==1, "diploma", "no diploma"))%>%
#   ggplot(aes(x=year, y=n))+
#   geom_bar(stat="identity", aes(fill=kid_univ_diploma), color="black")+
#   geom_label(aes(label=n, group=kid_univ_diploma),
#              position = position_stack(vjust = 0.5),
#              size=6
#   )+
#   scale_x_date(breaks = "1 years", date_labels = "%Y", expand=c(0.01, 0.01))+
#   scale_y_continuous(expand = c(0, 3)) +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(size=14),
#         legend.text = element_text(size=22),
#         legend.title = element_blank(),
#         legend.position = c(0.92, 0.92)
#         
#   )
# 
# #diploma
# p <- 
#   df%>%
#   group_by(kid_univ_diploma)%>%
#   count()%>%
#   mutate(kid_univ_diploma=ifelse(kid_univ_diploma==1, "diploma", "no diploma")
#   )%>%
#   ggplot(aes(x="", y=n, fill=kid_univ_diploma))+
#   geom_bar(stat="identity", width=1, color="white")+
#   coord_polar("y", start=0)+
#   geom_text(aes(label = paste0(comma(n), " (", comma( (n/sum(n))*100), "%)")),
#             position = position_stack(vjust = 0.5),
#             size = 6)+
#   theme_void()+
#   theme(legend.title = element_blank(),
#         legend.position = "none",
#         plot.background = element_rect(fill = "transparent", colour = NA)
#   )
# p
# ggsave(p, file="t.png", bg="transparent")

#family_size and residence
df%>%
  group_by(kids_num, gender, residence)%>%
  count()%>%
  ggplot(aes(x=forcats::fct_inseq(kids_num%>%as.factor()), y=n))+
    lemon::facet_rep_grid(gender~residence, 
                          repeat.tick.labels = TRUE,
                          switch="y"                     
    )+
    geom_bar(stat="identity", fill="darkblue", color="black")+
    geom_label(aes(label=n))+
    scale_y_continuous(expand = c(0, 30))+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_text(size=14),
          axis.text.x = element_text(size=14, color="black")
    )

#residence family size diploma
df%>%
  group_by(kids_num, gender, residence, kid_univ_diploma)%>%
  count()%>%
  group_by(kids_num, residence, gender)%>%
  mutate(p=n/sum(n))%>%
  mutate(kid_univ_diploma=ifelse(kid_univ_diploma==1, "diploma", "no diploma"))%>%
  ungroup()%>%
  arrange(n)%>%
  ggplot(aes(x=kids_num%>%as.factor()%>%forcats::fct_inseq(), 
             y=p
  )
  )+ 
  lemon::facet_rep_grid(gender~residence, 
                        repeat.tick.labels = TRUE,
                        switch="y"                     
  )+
  geom_bar(aes(fill=kid_univ_diploma), 
           stat="identity", 
           color="black"
  )+
  scale_y_continuous(expand = c(0, 0.01))+
  geom_label(aes(label=p%>%
                   `*`(100)%>%
                   round(1)%>%
                   paste0("% (", comma(n), ")"),
                 group=kid_univ_diploma    
  ),
  position = position_stack(vjust = 0.5),
  size=3
  )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=14),
        axis.text.x = element_text(size=14, color="black")
  )

#parents diplomas
df%>%
  group_by(father_univ_diploma, mother_univ_diploma, kids_num)%>%
  count()%>%
  mutate(father_univ_diploma=ifelse(father_univ_diploma=="0", "father: no diploma", "father: diploma"),
         mother_univ_diploma=ifelse(mother_univ_diploma=="0", "mother: no diploma", "mother: diploma")
  )%>%
  arrange(desc(n))%>%
  ggplot(aes(x=kids_num, y=n))+
  lemon::facet_rep_grid(mother_univ_diploma~father_univ_diploma, 
                        repeat.tick.labels = TRUE,
                        switch="y"
  )+
  geom_bar(stat="identity", fill="darkblue", color="black")+
  geom_label(aes(label=n))+
  scale_y_continuous(expand = c(0, 40))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size=14),
        axis.text.x = element_text(size=14, color="black")
  )

df%>%
  group_by(father_univ_diploma, mother_univ_diploma, kids_num, kid_univ_diploma)%>%
  count()%>%
  group_by(father_univ_diploma, mother_univ_diploma, kids_num)%>%
  mutate(father_univ_diploma=ifelse(father_univ_diploma=="0", "father: no diploma", "father: diploma"),
         mother_univ_diploma=ifelse(mother_univ_diploma=="0", "mother: no diploma", "mother: diploma"),
         kid_univ_diploma=ifelse(kid_univ_diploma==1, "diploma", "no diploma"),
         p=n/sum(n)
         
  )%>%
  arrange(desc(n))%>%
  ggplot(aes(x=kids_num, y=p))+
  lemon::facet_rep_grid(mother_univ_diploma~father_univ_diploma, 
                        repeat.tick.labels = TRUE,
                        switch="y"
  )+
  geom_bar(stat="identity", aes(fill=kid_univ_diploma), color="black")+
  geom_label(aes(label=paste0(round(p*100, 2), "% (", n, ")"), 
                 group=kid_univ_diploma),
             position = position_stack(vjust = 0.5))+
  scale_y_continuous(expand = c(0, 0.05))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size=14),
        legend.title = element_blank(),
        axis.text.x = element_text(size=14, color="black")
  )


#prof_group
df%>%
  filter(profession_code!=0)%>%
  mutate(prof=ifelse(profession_code<6, "qualified", "not qualifed"))%>%
  group_by(kid_univ_diploma, gender, prof)%>%
  count()%>%
  group_by(gender, prof)%>%
  mutate(p=n/sum(n))%>%
  mutate(kid_univ_diploma=ifelse(kid_univ_diploma==1, "diploma", "no diploma"))%>%
  ungroup()%>%
  arrange(n)%>%
  ggplot(aes(x=prof%>%factor(levels = c("qualified", "not qualifed")), 
             y=n
  )
  )+ 
  lemon::facet_rep_grid(~gender, 
                        repeat.tick.labels = TRUE,
                        switch="y"                     
  )+
  geom_bar(aes(fill=kid_univ_diploma), 
           stat="identity", 
           color="black"
  )+
  scale_y_continuous(expand = c(0, 10))+
  geom_label(aes(label=p%>%
                   `*`(100)%>%
                   round(1)%>%
                   paste0("% (", comma(n), ")"),
                 group=kid_univ_diploma    
  ),
  position = position_stack(vjust = 0.5),
  size=3
  )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=14),
        axis.text.x = element_text(size=14, color="black")
  )





