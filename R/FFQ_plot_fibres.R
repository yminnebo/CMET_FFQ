#' Plot where the amount of fibres per food group
#'
#' @return Plot where the amount of fibres per food group.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @export
#' @examples
#' plot <- FFQ.plot.fibregroups(FFQobject)



FFQ.plot.fibregroups <- function (FFQobject){
  df_fibres <- data.frame(); plotlist <- list()

  functdata_diet <- FFQobject@subsections$diet
  # for(i in c(1:ncol(subset.data.frame(functdata_diet, select = -c(1,2,3,4,5,6))))){
  i = 1
  df1 <- subset.data.frame(functdata_diet, select = c(1,2,3,4,5,6,6+i))
  code <- colnames(df1)[7]

  df1 <- as.data.frame(rbind(FFQobject@total.fibres.per.group$fruit,
                             FFQobject@total.fibres.per.group$vegetable,
                             FFQobject@total.fibres.per.group$nut))

  df1$B_glucan_g <- NA
  df1 <- rbind(df1, FFQobject@total.fibres.per.group$bread,
               FFQobject@total.fibres.per.group$cereal,
               FFQobject@total.fibres.per.group$pasta)
  df2<-df1

  rownames(df2) <- c("Fruits", "Vegetables", "Nuts", "Bread", "Cereals", "Pasta")



  df2 <- df2 %>%
    mutate(Total = rowSums(., na.rm = T))
  df2<- rownames_to_column(df2); df2$Group <- df2$rowname; df2$Participant <- code
  df2$Percentage <- (df2$Total/sum(df2$Total)*100)
  df2 <- subset(df2, select = c(Group, Total, Percentage, Participant))
  df_fibres <- rbind(df_fibres, df2)


  # df3$code <- code; rownames(df3) <- df3$code; df3<-subset(df3, select = -c(code))


  title <- code
  p1 <- ggplot(df2, aes(x="", y=Total, fill= Group)) + ggtitle(title) +
    geom_bar(stat="identity", width=1, color="black") +
    coord_polar("y", start=0)+
    scale_fill_manual("Food group", values = c("#b68ef6", "#f6b68e","#8ef6b6", "#8ecef6", "#f68ece", "#cef68e")) +
    scale_color_manual("Food group", values = c("#b68ef6", "#f6b68e","#8ef6b6", "#8ecef6", "#f68ece", "#cef68e")) +
    theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 20),
                         legend.title = element_text(size = 20)) +
    guides(fill = guide_legend(ncol = 2))
  return(p1)

}
