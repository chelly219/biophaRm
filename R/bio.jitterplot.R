#' Data Visualization
#'@param input excel file path
#'@keywords jitter plot
#'@export
#'@examples x axis is categorical, y axis is numeric box plot
#'
############################################################
# Quality Range - geom_jitter or geom_point
############################################################

bio.jitterplot <- function(filename, start, end, ref.name, three.sd){

  library(ggplot2); library(readxl); library(data.table)

  for(i in start:end){

    excel.data <- read_excel(filename, sheet=i, col_names = F )
    test.name <- as.character(excel.data[grep("file name", excel.data$...1),2])
    y.axis <- as.character(excel.data[grep("y name", excel.data$...1),2])
    ymax <- as.numeric(as.character(excel.data[grep("y max", excel.data$...1),2]))
    ymin <- as.numeric(as.character(excel.data[grep("y min", excel.data$...1),2]))
    y_up <- as.numeric(as.character(excel.data[grep("self_up", excel.data$...1),2]))
    y_low <- as.numeric(as.character(excel.data[grep("self_low", excel.data$...1),2]))
    y_decimal <- as.numeric(as.character(excel.data[grep("decimal", excel.data$...1),2]))

    # raw.data<-data.frame(excel.data[(grep("Sample", excel.data$...1)+1):nrow(excel.data),1:ncol(excel.data)])
    r.data<-data.frame(excel.data[(grep("Sample", excel.data$...1)+1):nrow(excel.data),])
    colnames(r.data)<-as.character(unlist(excel.data[grep("Sample", excel.data$...1),1:ncol(excel.data)]))

    raw.data<-data.frame(melt(as.data.table(r.data),id.vars = c('Sample','Batch','Type')))
    raw.data$variable<-gsub(pattern = "\\.", " ", x=raw.data$variable)
    raw.data$value<-as.numeric(as.character(unlist(raw.data$value)))

    ymean<-mean(subset(raw.data, Sample==ref.name& raw.data$value!="NA")$value)
    ysd<-sd(subset(raw.data, Sample==ref.name & raw.data$value!="NA")$value)
    set.seed(1)

    Type.no<-length(unique(raw.data$Type))
    ifelse(Type.no>2,shapes<-sample(21:25, Type.no), shapes<-c(19,21))

    ifelse(three.sd==TRUE, y_up<-(ymean+3*ysd))
    ifelse(three.sd==TRUE, y_low<-(ymean-3*ysd))

    ggplot(raw.data, aes(x=Sample, y=value))+
      geom_jitter(width=0.2, aes(color=Sample, shape=Type),size=3)+
      # geom_hline(yintercept=ymean-3*ysd, linetype="dashed", color = "red")+
      # geom_hline(yintercept=ymean+3*ysd, linetype="dashed", color = "red")+
      geom_hline(yintercept=y_up, linetype="dashed", color = "red")+
      geom_text( mapping=aes(x=0.5, y=y_up, label="Upper Limit"),
                 size=3, angle=0, vjust=-0.4, hjust=0, color="red") +
      geom_hline(yintercept=y_low, linetype="dashed", color = "red")+
      geom_text( mapping=aes(x=0.5, y=y_low, label="Lower Limit"),
                 size=3, angle=0, vjust=1.2, hjust=0, color="red") +

      scale_color_manual(values=c('blue3', 'red'), guide="none")+
      scale_shape_manual(values = shapes)+
      scale_y_continuous(limits= c(ymin,ymax), labels=function(x) sprintf(paste0("%.",y_decimal,"f"), x))+
      xlab("") +
      ylab(y.axis)+
      ggtitle(test.name)+
      theme_bw()+
      theme(text=element_text(size=10, face="bold"),
            panel.grid.major.x = element_blank(),
            panel.grid.major = element_line(size=0.3),
            panel.grid.minor = element_line(size=0.3),
            panel.spacing = unit(1, "lines"),
            # panel.border = element_rect(fill=NA, size=0.1),
            legend.title = element_blank(),
            legend.text = element_text(size=6, margin=margin(0,0,0,0, unit="cm")),
            legend.key.size = unit(0.4, "cm"),
            # legend.position="bottom",
            # legend.box="horizontal",
            legend.justification="center",
            legend.margin=margin(0.2,0.25, -0.4,-0.3, unit="cm"),
            legend.spacing.y=unit(10,'pt'),
            legend.box.margin=margin(0,-1,0,0),
            plot.title = element_text(size=12, hjust = 0.5),
            axis.title = element_text(size=12, face= "bold")
      )
    ggsave(filename = paste0(getwd(),"/",i,"_", test.name, "_jitplot.png"), width=10, height=10, unit="cm")

  }
}
