# # Graphing details, generic
# deets<-theme_bw() + #
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_blank(), #element_rect(fill=NA)
#         
#         axis.line = element_line(size=.5),
#         axis.ticks = element_line(size=.5),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=1), 
#         axis.text = element_text(size=rel(1.5), lineheight=.8,  colour="black"),
#         axis.title.y=element_text(vjust=1,lineheight=.8, size=rel(1.5)),
#         axis.title.x=element_text(vjust=.5, size=rel(1.5)), 
#         
#         plot.title = element_text(size=rel(2),hjust = 0.5),
#         
#         legend.title=element_text(size=rel(1.5)),
#         legend.text=element_text(size=rel(1.3)),
#         legend.position=c(.2,.25),
#         legend.background = element_rect(fill="transparent", colour = NA),
#         strip.text = element_text(size=rel(1.3)))
# 
# #############################
# deets2<-theme_bw() + #
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=.8), 
#         axis.text = element_text(size=rel(1.2), lineheight=.8,  colour="black"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         legend.title=element_text(size=rel(1.5)),
#         legend.text=element_text(size=rel(1.1)),
#         legend.position=c(.8,.25),
#         legend.background = element_rect(fill="transparent", colour = NA),
#         strip.text = element_text(size=rel(1.3)))
# 
# #############################
# deets3<-theme_bw() + #
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#         
#         # axis.line = element_line(size=.5),
#         # axis.ticks = element_line(size=.5),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=.8), #, face="bold"
#         axis.text = element_text(size=rel(1.2), lineheight=.8,  colour="black"),#, face="bold"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         
#         legend.title=element_text(size=rel(1.1)),
#         legend.text=element_text(size=rel(1)),
#         # legend.position=c(.8,.25),
#         legend.background = element_rect(fill="transparent", colour = NA),
#         strip.text = element_text(size=rel(1.3)))
# 
# #############################
# deets4<-theme_bw() + # No legend
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=.8),
#         axis.text = element_text(size=rel(1.1), lineheight=.8,  colour="black"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         
#         legend.title=element_text(size=rel(1.3)),
#         legend.text=element_text(size=rel(1.2)),
#         legend.position="none",
#         legend.background = element_rect(fill="transparent", colour = NA),
#         strip.text = element_text(size=rel(1.3)))
# 
# #############################
# # slightly bigger text
# deets5<-theme_bw() + #
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#                 # axis.line = element_line(size=.5),
#         # axis.ticks = element_line(size=.5),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=.8), 
#         axis.text = element_text(size=rel(1.2), lineheight=.8,  colour="black"),#, face="bold"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         
#         legend.title=element_text(size=rel(1.2)),
#         legend.text=element_text(size=rel(1.1)),
#         # legend.position=c(.8,.25),
#         legend.background = element_rect(colour='white'),
#         strip.text = element_text(size=rel(1.3)))

# #deets 5, internal legend
# deets6<-theme_bw() + # legend bottom right
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#         
#         # axis.line = element_line(size=.5),
#         # axis.ticks = element_line(size=.5),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=.8), #, face="bold"
#         axis.text = element_text(size=rel(1.2), lineheight=.8,  colour="black"),#, face="bold"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         
#         legend.title=element_text(size=rel(1.3)),
#         legend.text=element_text(size=rel(1.2)),
#         legend.position=c(.15,.17),
#         legend.background = element_rect(fill="transparent", colour = NA),
#         strip.text.y = element_text(size=rel(1.3)))
# 
# deets7<-theme_bw() + # legend inside
#   theme(panel.grid.minor=element_blank(), 
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=.8), 
#         axis.text = element_text(size=rel(1.2), lineheight=.8,  colour="black"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         legend.title=element_text(size=rel(1.2)),
#         legend.text=element_text(size=rel(1.1)),
#         legend.position=c(.84,.3),
#         legend.background = element_rect(fill="transparent", colour = NA),
#         strip.text.y = element_text(size=rel(1.3)))

#############################
# No legend # , angle = 45
deets8<-theme_bw() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        panel.background=element_rect(fill=NA),
        panel.border=element_rect(fill=NA),
        axis.title = element_text(size=rel(1.4),colour="black", lineheight=.8),
        axis.text = element_text(size=rel(1.3), lineheight=.8,  colour="black"),
        axis.text.x = element_text(angle = 90, hjust=1), #angle = 45, hjust=1
        axis.title.y=element_text(vjust=1,lineheight=.8),
        axis.title.x=element_text(vjust=.5),  
        legend.position="none",
        strip.text = element_text(size=rel(1.3)))


#############################
# deets5 bigger text
deets9<-theme_bw() + #
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        panel.background=element_rect(fill=NA),
        panel.border=element_rect(fill=NA),
        # axis.line = element_line(size=.5),
        # axis.ticks = element_line(size=.5),
        axis.title = element_text(size=rel(1.4),colour="black", lineheight=.8), 
        axis.text = element_text(size=rel(1.3), lineheight=.8,  colour="black"),#, face="bold"),
        axis.text.x = element_text(angle = 90, hjust=1), #angle = 45, hjust=1
        axis.title.y=element_text(vjust=1,lineheight=.8),
        axis.title.x=element_text(vjust=.5), 
        
        legend.title=element_text(size=rel(1.2)),
        legend.text=element_text(size=rel(1.1)),
        # legend.position=c(.8,.25),
        legend.background = element_rect(colour='white'),
        strip.text = element_text(size=rel(1.3)))

# deets5 bigger text
deets10<-theme_bw() + #
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        panel.background=element_rect(fill=NA),
        panel.border=element_rect(fill=NA),
        # axis.line = element_line(size=.5),
        # axis.ticks = element_line(size=.5),
        axis.title = element_text(size=rel(1.4),colour="black", lineheight=.8), 
        axis.text = element_text(size=rel(1.3), lineheight=.8,  colour="black"),#, face="bold"),
        # axis.text.x = element_text(angle = 90, hjust=1), #angle = 45, hjust=1
        axis.title.y=element_text(vjust=1,lineheight=.8),
        axis.title.x=element_text(vjust=.5), 
        
        legend.title=element_text(size=rel(1.2)),
        legend.text=element_text(size=rel(1.1)),
        # legend.position=c(.8,.25),
        legend.background = element_rect(colour='white'),
        strip.text = element_text(size=rel(1.3)))

# deets10<-theme_bw() + # legend inside, white box
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#         axis.title = element_text(size=rel(1.3),colour="black", lineheight=.8), 
#         axis.text = element_text(size=rel(1.2), lineheight=.8,  colour="black"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         legend.title=element_text(size=rel(1.2)),
#         legend.text=element_text(size=rel(1.1)),
#         legend.position=c(.83,.43),
#         legend.background = element_rect(fill="white", colour = "black"),
#         strip.text.y = element_text(size=rel(1.3)))

# #############################
# # No legend # , no x text
# deets11<-theme_bw() + 
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_rect(fill=NA),
#         panel.border=element_rect(fill=NA),
#         axis.title = element_text(size=rel(1.4),colour="black", lineheight=.8),
#         axis.text = element_text(size=rel(1.3), lineheight=.8),
#         axis.text.x = element_text(hjust=1, angle = 45,  colour="white"),
#         axis.title.y=element_text(vjust=1,lineheight=.8),
#         axis.title.x=element_text(vjust=.5), 
#         legend.position="none",
#         strip.text = element_text(size=rel(1.3)))
