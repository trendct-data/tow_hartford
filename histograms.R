# Time series by location
# Generates small histograms used in the map (only for locations with more than 4 tows)

library(extrafont)



tows$hour <- hour(tows$Time)

tow_hour <- tows %>%
  group_by(Tow_From_Address, hour) %>%
  summarise(count=n()) %>%
  filter(count > 4) %>%
  arrange(-count)

tow_hour2 <- spread(tow_hour, hour, count)
tow_hour2 <- gather(tow_hour2, hour, count, 2:25)
tow_hour2$hour <- as.numeric(tow_hour2$hour)
# Heatmap by hour

# Histograms of address by hour

address_list <- unique(tow_hour2$Tow_From_Address)

for (i in 1:length(address_list)) {
  addy <- address_list[i]
  addy_short <- gsub(" ", "", addy)
  
  histo <- subset(tow_hour2, Tow_From_Address==addy)
  
  tow_hist <- ggplot(histo, aes(x=hour, y=count)) 
  tow_hist <- tow_hist + geom_bar(stat="identity")
  #tow_hist <- tow_hist + facet_wrap(~Tow_From_Address)
  tow_hist <- tow_hist + theme(plot.title=element_text(face="bold", hjust=.4))
  tow_hist <- tow_hist + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
  tow_hist <- tow_hist + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
  tow_hist <- tow_hist + labs(x="Hour", y="Tows", title=addy)
  tow_hist <- tow_hist +   scale_x_continuous(limits=c(0,24),
                                              breaks=0:12*2,
                                              labels=c("12 am", paste(1:5*2,"am"),
                                              "12 pm",
                                              paste(7:11*2-12,"pm"), 
                                              "12 am")) 
  tow_hist <- tow_hist + theme_bw(base_family="Calibri")
  tow_hist <- tow_hist + theme(panel.border=element_blank())
  tow_hist <- tow_hist + theme(text = element_text(size=10))
  tow_hist <- tow_hist + theme(plot.title=element_text(face="bold", family="Lato Black", size=22))
  tow_hist <- tow_hist + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))
  tow_hist <- tow_hist + theme(plot.caption=element_text(size=12, margin=margin(t=10, r=80), color="#7a7d7e"))
  tow_hist <- tow_hist + theme(legend.position="none")
                              
   file_name <- paste0("hours/",addy_short, ".png")
   ggsave(tow_hist, file = file_name, width = 5, height = 2.5, type = "cairo-png")
   
}
