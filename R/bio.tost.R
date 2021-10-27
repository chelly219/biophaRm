#' TOST Function
#'
#' This function allows Equivalence test.
#' @param two The equivalence test for biosimilar and reference product.
#' @keywords biosimilar TOST
#' @export
#' @examples
#' biotest.tost(test.name="Biosimilar", ref.name="Reference", method.name="WAX-HPLC", ci=0.95, paired = FALSE )
#'
#' test.name Write a test sample name.
#' ref.name Write a reference sample name.
#' method.name Write a test method name for title
#' ci Confidence interval
#' paired default is FALSE

############################################################
# Equivalence Test (TOST)
############################################################


biotest.tost <- function(filename, sheet.no=1, raw.data, test.name, ref.name, method.name,
                         ci=0.90, paired = FALSE, sample.n.show=TRUE)
  {
  library(equivalence);library(ggplot2); library(readxl)

  scaleFUN <- function(x) sprintf("%.2f", x)
  raw.data <- read_excel(filename, sheet=sheet.no, col_names = T )
  reference = subset(raw.data, Type==ref.name)$Value
  test = subset(raw.data, Type==test.name)$Value

  result<-
    tost(x = test, y= reference,
         paired = paired,
         conf.level = ci,
         epsilon = 1.5*(sd(reference))
    )
  print(result)

  if(result$tost.p.value<0.05){
    print("Equivalence")
  }else{
    print("non-equivalence")
  }

  ############################################################
  # Graph for CKD.TOST Result
  ############################################################
  conf.value <- as.numeric(attributes(result$tost.interval)$conf.level)*100
  d<-data.frame(Name=test.name, mean=c(0), lower=result$tost.interval[1], upper=result$tost.interval[2])

  ggplot() +
    geom_errorbar(data=d, mapping=aes(x=Name, ymin=upper, ymax=lower), size=1, color="darkred", width=0.2) +
    #geom_point(data=d, mapping=aes(x=Name, y=mean), size=4, shape=21, fill="darkred") +
    geom_hline(yintercept= -result$epsilon, color='darkgreen', linetype = "dashed" ,size=0.5)+
    geom_text(data=d, mapping=aes(x=1.25, y=-result$epsilon, label="Lower Limit"),
              size=4, angle=90, vjust=-0.4, hjust=0, color="darkgreen", fontface = "bold") +
    geom_hline(yintercept= result$epsilon, color='darkgreen', linetype = "dashed" ,size=0.4)+
    geom_text(data=d, mapping=aes(x=1.48, y=result$epsilon, label="Upper Limit"),
              size=4, angle=270, vjust=-0.4, hjust=0, color="darkgreen", fontface = "bold") +
    scale_y_continuous(limits = c(-1.2*(result$epsilon) ,1.6*(result$epsilon)),
                       breaks=c(seq(from=c(-round(result$epsilon,2)), to= c(round(result$epsilon,2)), by=4))) +

    xlab(paste0(conf.value,"% TOST Interval")) +
    ylab("Difference Between Means") +
    labs(
      # title = paste0("Equivalence Test Result: ", test.name, " vs ", ref.name),
      # title = "Equivalence Test Result",
      title = paste0("Equivalence Test Result : ", method.name),
      subtitle = paste0( conf.value,"% Confidence interval of Test [", scaleFUN(result$tost.interval[1]), ", ", scaleFUN(result$tost.interval[2]), "], n = ",length(test),
                         "\n Equivalence Lower & Upper Limit of ", ref.name, " is [-",scaleFUN(result$epsilon),", +",scaleFUN(result$epsilon),"], n = ", length(reference)),
                         #"\n No. of Reference = ",length(reference), " and ", "No. of Test = ",length(test)),
      caption = "tost {equivalence v0.7.2}")+
    theme_classic()+
    theme(
      plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "antiquewhite4", hjust = 0.5),
      text=element_text(size=12)
    )+
    coord_flip()
  ggsave(filename = paste0(getwd(),"/equivalent_test ", method.name, "_", test.name, " vs ", ref.name, ".png"), width = 10, height = 5 )

}
