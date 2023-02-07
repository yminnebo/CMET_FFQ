
#' Print the I-SHIME protocol
#'
#' @return Prints out the I-SHIME protocol.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @param show.as.plain.text show the results as plain text
#' @param show.seeds Show and incorporate seeds
#' @export
#' @examples
#' FFQ.print.protocol(FFQobject)

FFQ.print.protocol<- function (FFQobject, show.as.plain.text = FALSE, show.seeds = FALSE, divide = TRUE){
  i = 1
  functdata_diet <- FFQobject@subsections$diet
  df1 <- subset.data.frame(functdata_diet, select = c(1,2,3,4,5,6,6+i))
  code <- colnames(df1)[7]
  timing <- subset(FFQobject@subsections$timing, select = c(3,4))
  timing <- timing[complete.cases(timing[,2]),]
  row.names(timing) <- timing$Group; timing <- subset(timing, select=c(2))
  df_timing <- timing; colnames(df_timing) <- "Time"
  # Seeds
  if(show.seeds == TRUE){
    df_seeds_d1 <- subset(df1, Subcategory == "Seeds" &  #WEGLATEN WANNEER WE MEER DAN FRUIT HEBBEN
                            Group != "Frequency" & Group != "Meal_included" & Group != "Other")
    df_seeds_d1_1 <- subset(df_seeds_d1, df_seeds_d1$Base_unit == "pieces")
    df_seeds_d1_2 <- subset(df_seeds_d1, df_seeds_d1$Base_unit == "g")
    df_seeds_d1_3 <- subset(df_seeds_d1, df_seeds_d1$Subgroup == "Time")
    rownames(df_seeds_d1_1) <- df_seeds_d1_1$Group
    rownames(df_seeds_d1_2) <- df_seeds_d1_2$Group
    rownames(df_seeds_d1_3) <- df_seeds_d1_3$Group
    df_seeds_d1 <- as.data.frame(cbind(Category = df_seeds_d1_1$Category,
                                       Subcategory = df_seeds_d1_1$Subcategory,
                                       Group = df_seeds_d1_1$Group,
                                       Subgroup = df_seeds_d1_1$Subgroup,
                                       Base_unit = df_seeds_d1_1$Base_unit,
                                       Quantity_pieces = df_seeds_d1_1[,7],
                                       Quantity_g = df_seeds_d1_2[,7],
                                       Time  = df_seeds_d1_3[,7]))
    df_seeds_d1 <- subset(df_seeds_d1, select = -c(1,2,4,5))
    df_seeds_d1 <- df_seeds_d1[!with(df_seeds_d1, is.na(Time)),]
    if(nrow(df_seeds_d1) == 0) {
      df_seeds_d1 <- data.frame(OEI  = c("NO VALUE"))
    }
    print(df_seeds_d1)
  }


  df1 <- as.data.frame(rbind(FFQobject@total.fibres.per.group$fruit, FFQobject@total.fibres.per.group$vegetable,
                             FFQobject@total.fibres.per.group$nut))

  df1$B_glucan_g <- NA
  df1 <- rbind(df1, FFQobject@total.fibres.per.group$bread, FFQobject@total.fibres.per.group$cereal, FFQobject@total.fibres.per.group$pasta)
  df2<-df1

  df3 <- as.data.frame(
    t(
      as.data.frame(
        colSums(df2*(10/6)/10, na.rm = T)
      )
    )
  )
  df3 <- round(df3, digits = 4)
  df3 <- rename(df3,
                "Guar flour (SDF hemicellulose)"= SDF_Hemicellulose_g,
                "Xylan (ISD Hemicellulose)" = ISD_Hemicellulose_g,
                "Wheat starch (RS1)"=RS1_g,
                "Corn starch (RS2)" = RS2_g,
                "Potato starch (RS3)" = RS3_g,
                "Inulin (Fructan)" = Fructan_g,
                "SDF Pectin" = SDF_Pectin_g,
                "ISD Pectin" = ISD_Pectin_g,
                "ISD Cellulose"  = ISD_Cellulose_g,
                'ISD Lignin' = ISD_Lignin_g,
                'Chitin' = Chitin_g,
                'B glucan' = B_glucan_g
  )

  if(divide == T){
    df3 <- df3/2

  }

  if(divide == T){
    df3$Mucin_typeII <- 1; df3$Yeast_extract <- 1.5
    df3$Proteose_peptone <- 0.5}else{
      df3$Mucin_typeII <- 2; df3$Yeast_extract <- 3
      df3$Proteose_peptone <- 1
    }

  df3 <- rename(df3,
                "Mucin type II"= Mucin_typeII,
                "Proteose peptone" = Proteose_peptone,
                "Yeast extract" = Yeast_extract)

  name <- paste0("Composition (g/L)")
  df3 <- as.data.frame(t(df3)); names(df3) <- c(name)
  df3$Composition_2L <- df3[,1]*2; colnames(df3)[colnames(df3) == 'Composition_2L'] <- 'Composition (g/2L)'

  df3$Location <- c("Media G", "Sugars P",  "Media X", "C002", "Sugars P",
                    "L12", "Media S", "Media S", "Media S", "Media C", "I005",
                    "Media G" ,"Fridge", "Media Y", "Media P")

  # print(df3)
  if(i == 3){savedf <- df3; View(savedf)}






  # Transit time
  functdata_health <- FFQobject@subsections$health
  functdata_transit <- subset.data.frame(functdata_health,
                                         functdata_health$Subcategory == "Defecation_frequency"|
                                           functdata_health$Subcategory == "Defecation_regular_hours"|
                                           functdata_health$Subcategory == "Defecation_regular"|
                                           functdata_health$Subcategory == "Bristol_stool_chart")
  # Average bristol stool scale score per person
  ## Get data frame with only BSS and keep columns with donors only
  functdata_BSS <- subset.data.frame(functdata_transit,
                                     functdata_transit$Subcategory == "Bristol_stool_chart")
  row.names(functdata_BSS) <- functdata_BSS$Group
  functdata_BSS <- subset(functdata_BSS, select = -c(Category, Subcategory, Subgroup, Row, Base_unit))
  df_BSS <- subset.data.frame(functdata_BSS, select = c(1,2)); df_BSSall <- functdata_BSS
  df_BSS <- na.omit(df_BSS)
  df_BSS$Donor <- rep(colnames(df_BSS)[2] ,nrow(df_BSS))
  names(df_BSS)[2] <- "BSS"
  df_BSS$Group <- gsub("Type_","",df_BSS$Group); df_BSS <- rename(df_BSS, "Type" = "Group")
  # Create average BSS
  df_BSS$Type <- as.numeric(df_BSS$Type)
  suppressMessages(
    df_BSS_sum <- df_BSS %>% group_by(Donor) %>%
      summarise(SDType = sd(Type),meanType = mean(Type),Donor = Donor)
  )
  df_BSS_sum <- distinct(df_BSS_sum)

  # Average defecation activity per person
  ## Get data frame with only activity and keep columns with donors only
  functdata_Def <- subset.data.frame(functdata_transit,
                                     functdata_transit$Subcategory != "Bristol_stool_chart")
  # row.names(functdata_Def) <- functdata_Def$Group
  functdata_Def <- subset(functdata_Def, select = -c(Category, Subcategory, Subgroup, Row, Base_unit, Group))
  df_Def <- subset.data.frame(functdata_Def, select = c(1)); df_Def <- na.omit(df_Def)
  # take first row and rename codes

  dftest <-df1 <- df_Def[1,]; df1[df1 == "1"] <- "More than 5 times per day"
  df1[df1 == "2"] <- "3-5 times per day"; df1[df1 == "3"] <- "1-2 times per day"
  df1[df1 == "4"] <- "5-6 times per week"; df1[df1 == "5"] <- "3-4 times per week"
  df1[df1 == "6"] <- "1-2 times per week"; df1[df1 == "7"] <- "other"

  df2 <- df_Def[2,]; df2[df2 == "1"] <- "Yes"; df2[df2 == "2"] <- "No"

  df_Def <- rbind(df1, df2, df_Def[3, ])
  # Combine all in one kable
  timing <- subset(subset(functdata_diet, Subcategory == "Timing"), select = -c(1,2,4:6))
  df2 <- subset.data.frame(df_BSS_sum, select = c(3))
  df2 <- na.omit(df2); df1[df1 == "no"] <- "NA"; df1[df1 == ""] <- "NA"; df1 <- na.omit(df1)
  # grid.arrange(text_grob(label = "f", face = 'bold', size = 16), tableGrob(df2),
  # tableGrob(df_Def), nrow = 3, heights = c(1,0.5,1))
  dftest
  BSS <- df2
  if(BSS > 3 & dftest <4){
    transitadvice <- "short transit time"; trans <- "Short"
  }else{
    if(BSS < 4 & BSS > 2 & dftest >3 & dftest < 6){
      transitadvice <- "medium transit time"; trans <- "Medium"
    }else{if(BSS < 2.1 & dftest > 5){
      transitadvice <- "long transit time"; trans <- "Long"
    }else{
      transitadvice <- "problem"; trans <- "unknown"
    }

    }
  }



  if(transitadvice != "problem"){
    df_transit <- data.frame("Transit_time" = c("Short" , "Medium" , "Long"), "τ_.PC." = c(8,16,24), "V_.PC." = c(200,400,600),
                             "Faecal_inoculum_.PC." = c(10,20,30), "τ_.DC." = c(13,26,39), "V_.DC." = c(325,650,975),
                             "Faecal_inoculum_.DC." = c(16.25,32.50,48.75), "Amount" = c(24,48,72))
    df_transit <- subset(df_transit, Transit_time == trans)
    df_transit <- rename(df_transit, "Transit time"= Transit_time, "τ (PC)"= τ_.PC., "V (PC)" = V_.PC.,
                         "Faecal inoculum (PC)" = Faecal_inoculum_.PC., "τ (DC)"= τ_.DC., "V (DC)" = V_.DC.,
                         "Faecal inoculum (DC)" = Faecal_inoculum_.DC., "Amount of beads" = Amount)


    # tableGrob(df_Def), nrow = 3, heights = c(1,0.5,1))

    # message(paste0("\nYour donor has a mean bristol stool score of ", crayon::white(df2), " and defecates ", crayon::white(df1), ".\n", "We therefore categorise the donor as a ", crayon::white(transitadvice), " donor. This matches the following SHIME configuration"))
    #
    #
    #     df <- data.frame("Transit_time" = c("Short" , "Medium" , "Long"), "τ_.PC." = c(8,16,24), "V_.PC." = c(200,400,600), "Faecal_inoculum_.PC." = c(10,20,30),
    #                    "τ_.DC." = c(13,26,39), "V_.DC." = c(325,650,975), "Faecal_inoculum_.DC." = c(16.25,32.50,48.75), "Amount" = c(24,48,72))
    #     df <- subset(df, Transit_time == trans)
    #     df <- rename(df, "Transit time"= Transit_time, "τ (PC)"= τ_.PC., "V (PC)" = V_.PC., "Faecal inoculum (PC)" = Faecal_inoculum_.PC.,
    #                "τ (DC)"= τ_.DC., "V (DC)" = V_.DC., "Faecal inoculum (DC)" = Faecal_inoculum_.DC., "Amount of beads" = Amount)
    #     print(df)
    #     message(paste0("Type ", crayon::blue("FFQ.transit.info()"), " for more information on how to adapt your SHIME accordingly."))
  }else{
    df_transit <- data.frame("Error" = c("Values do not correspond to the strict classification rules", "Type FFQ.transit.info() for more info"))

    # message(paste0("\nYour donor has a mean bristol stool score of ", crayon::white(df2), " and defecates ", crayon::white(df1), ".\n", "These values do not correspond to the strict classification rules. You will have to estimate it yourself. Type ", crayon::blue("FFQ.transit.info()"), " for more information on how to adapt your SHIME accordingly."))
  }


tt3 <- ttheme_minimal(rowhead=list(fg_params=list(col="#1b1717", fontface=2L)))

  t1 <- tableGrob(df3, theme = tt3)
  title <- textGrob("Medium",gp=gpar(fontsize=20, fontface = "bold"))
  padding <- unit(5,"mm")
  table1 <- gtable::gtable_add_rows(
    t1,
    heights = grobHeight(title) + padding,
    pos = 0)
  table1 <- gtable::gtable_add_grob(
    table1,
    title,
    1, 1, 1, ncol(table1))
  t2 <- tableGrob(df_transit, theme = tt3)
  title2 <- textGrob("Transit time",gp=gpar(fontsize=20, fontface = "bold"))
  padding2 <- unit(5,"mm")
  table2 <- gtable::gtable_add_rows(
    t2,
    heights = grobHeight(title2) + padding2,
    pos = 0)
  table2 <- gtable::gtable_add_grob(
    table2,
    title2,
    1, 1, 1, ncol(table2))
  t3 <- tableGrob(df_timing, theme = tt3)
  title3 <- textGrob("Eating pattern",gp=gpar(fontsize=20, fontface = "bold"))
  padding3 <- unit(5,"mm")
  table3 <- gtable::gtable_add_rows(
    t3,
    heights = grobHeight(title3) + padding3,
    pos = 0)
  table3 <- gtable::gtable_add_grob(
    table3,
    title3,
    1, 1, 1, ncol(table3))
  # grid.arrange(table1)
  # grid.arrange(table2)
  # grid.arrange(table3)
  grid.arrange(table1, table2, table3, nrow= 3, heights = c(15,1,6))
}



#' Print the medium
#'
#' @return Prints out the I-SHIME medium separately as a data frame. Easier to copy like this!
#'
#' @param FFQobject FFQ object with all FFQ data
#' @export
#' @examples
#' FFQ.print.medium(FFQobject)


FFQ.print.medium<- function (FFQobject, show.as.plain.text = FALSE, show.seeds = FALSE, divide = TRUE){
  i = 1
  functdata_diet <- FFQobject@subsections$diet
  df1 <- subset.data.frame(functdata_diet, select = c(1,2,3,4,5,6,6+i))
  code <- colnames(df1)[7]
  timing <- subset(FFQobject@subsections$timing, select = c(3,4))
  timing <- timing[complete.cases(timing[,2]),]
  row.names(timing) <- timing$Group; timing <- subset(timing, select=c(2))

  # Seeds
  if(show.seeds == TRUE){
    df_seeds_d1 <- subset(df1, Subcategory == "Seeds" &  #WEGLATEN WANNEER WE MEER DAN FRUIT HEBBEN
                            Group != "Frequency" & Group != "Meal_included" & Group != "Other")
    df_seeds_d1_1 <- subset(df_seeds_d1, df_seeds_d1$Base_unit == "pieces")
    df_seeds_d1_2 <- subset(df_seeds_d1, df_seeds_d1$Base_unit == "g")
    df_seeds_d1_3 <- subset(df_seeds_d1, df_seeds_d1$Subgroup == "Time")
    rownames(df_seeds_d1_1) <- df_seeds_d1_1$Group
    rownames(df_seeds_d1_2) <- df_seeds_d1_2$Group
    rownames(df_seeds_d1_3) <- df_seeds_d1_3$Group
    df_seeds_d1 <- as.data.frame(cbind(Category = df_seeds_d1_1$Category,
                                       Subcategory = df_seeds_d1_1$Subcategory,
                                       Group = df_seeds_d1_1$Group,
                                       Subgroup = df_seeds_d1_1$Subgroup,
                                       Base_unit = df_seeds_d1_1$Base_unit,
                                       Quantity_pieces = df_seeds_d1_1[,7],
                                       Quantity_g = df_seeds_d1_2[,7],
                                       Time  = df_seeds_d1_3[,7]))
    df_seeds_d1 <- subset(df_seeds_d1, select = -c(1,2,4,5))
    df_seeds_d1 <- df_seeds_d1[!with(df_seeds_d1, is.na(Time)),]
    if(nrow(df_seeds_d1) == 0) {
      df_seeds_d1 <- data.frame(OEI  = c("NO VALUE"))
    }
    print(df_seeds_d1)
  }


  df1 <- as.data.frame(rbind(FFQobject@total.fibres.per.group$fruit, FFQobject@total.fibres.per.group$vegetable,
                             FFQobject@total.fibres.per.group$nut))

  df1$B_glucan_g <- NA
  df1 <- rbind(df1, FFQobject@total.fibres.per.group$bread, FFQobject@total.fibres.per.group$cereal, FFQobject@total.fibres.per.group$pasta)
  df2<-df1

  df3 <- as.data.frame(
    t(
      as.data.frame(
        colSums(df2*(10/6)/10, na.rm = T)
      )
    )
  )
  df3 <- round(df3, digits = 4)
  df3 <- rename(df3,
                "Guar flour (SDF hemicellulose)"= SDF_Hemicellulose_g,
                "Xylan (ISD Hemicellulose)" = ISD_Hemicellulose_g,
                "Wheat starch (RS1)"=RS1_g,
                "Corn starch (RS2)" = RS2_g,
                "Potato starch (RS3)" = RS3_g,
                "Inulin (Fructan)" = Fructan_g,
                "SDF Pectin" = SDF_Pectin_g,
                "ISD Pectin" = ISD_Pectin_g,
                "ISD Cellulose"  = ISD_Cellulose_g,
                'ISD Lignin' = ISD_Lignin_g,
                'Chitin' = Chitin_g,
                'B glucan' = B_glucan_g
  )

  if(divide == T){
    df3 <- df3/2

  }

  if(divide == T){
    df3$Mucin_typeII <- 1; df3$Yeast_extract <- 1.5
    df3$Proteose_peptone <- 0.5}else{
      df3$Mucin_typeII <- 2; df3$Yeast_extract <- 3
      df3$Proteose_peptone <- 1
    }

  df3 <- rename(df3,
                "Mucin type II"= Mucin_typeII,
                "Proteose peptone" = Proteose_peptone,
                "Yeast extract" = Yeast_extract)

  name <- paste0("Composition (g/L)")
  df3 <- as.data.frame(t(df3)); names(df3) <- c(name)
  df3$Composition_2L <- df3[,1]*2; colnames(df3)[colnames(df3) == 'Composition_2L'] <- 'Composition (g/2L)'

  df3$Location <- c("Media G", "Sugars P",  "Media X", "C002", "Sugars P",
                    "L12", "Media S", "Media S", "Media S", "Media C", "I005",
                    "Media G" ,"Fridge", "Media Y", "Media P")

  print(df3)
}


#' Print the transit time
#'
#' @return Prints out the transit time separately as a data frame. Easier to copy like this!
#'
#' @param FFQobject FFQ object with all FFQ data
#' @export
#' @examples
#' FFQ.print.transit(FFQobject)


FFQ.print.transit <- function (FFQobject) {
  functdata_health <- FFQobject@subsections$health
  functdata_transit <- subset.data.frame(functdata_health,
                                         functdata_health$Subcategory == "Defecation_frequency"|
                                           functdata_health$Subcategory == "Defecation_regular_hours"|
                                           functdata_health$Subcategory == "Defecation_regular"|
                                           functdata_health$Subcategory == "Bristol_stool_chart")
  # Average bristol stool scale score per person
  ## Get data frame with only BSS and keep columns with donors only
  functdata_BSS <- subset.data.frame(functdata_transit,
                                     functdata_transit$Subcategory == "Bristol_stool_chart")
  row.names(functdata_BSS) <- functdata_BSS$Group
  functdata_BSS <- subset(functdata_BSS, select = -c(Category, Subcategory, Subgroup, Row, Base_unit))
  df_BSS <- subset.data.frame(functdata_BSS, select = c(1,2)); df_BSSall <- functdata_BSS
  df_BSS <- na.omit(df_BSS)
  df_BSS$Donor <- rep(colnames(df_BSS)[2] ,nrow(df_BSS))
  names(df_BSS)[2] <- "BSS"
  df_BSS$Group <- gsub("Type_","",df_BSS$Group); df_BSS <- rename(df_BSS, "Type" = "Group")
  # Create average BSS
  df_BSS$Type <- as.numeric(df_BSS$Type)
  suppressMessages(
    df_BSS_sum <- df_BSS %>% group_by(Donor) %>%
      summarise(SDType = sd(Type),meanType = mean(Type),Donor = Donor)
  )
  df_BSS_sum <- distinct(df_BSS_sum)

  # Average defecation activity per person
  ## Get data frame with only activity and keep columns with donors only
  functdata_Def <- subset.data.frame(functdata_transit,
                                     functdata_transit$Subcategory != "Bristol_stool_chart")
  # row.names(functdata_Def) <- functdata_Def$Group
  functdata_Def <- subset(functdata_Def, select = -c(Category, Subcategory, Subgroup, Row, Base_unit, Group))
  df_Def <- subset.data.frame(functdata_Def, select = c(1)); df_Def <- na.omit(df_Def)
  # take first row and rename codes

  dftest <-df1 <- df_Def[1,]; df1[df1 == "1"] <- "More than 5 times per day"
  df1[df1 == "2"] <- "3-5 times per day"; df1[df1 == "3"] <- "1-2 times per day"
  df1[df1 == "4"] <- "5-6 times per week"; df1[df1 == "5"] <- "3-4 times per week"
  df1[df1 == "6"] <- "1-2 times per week"; df1[df1 == "7"] <- "other"

  df2 <- df_Def[2,]; df2[df2 == "1"] <- "Yes"; df2[df2 == "2"] <- "No"

  df_Def <- rbind(df1, df2, df_Def[3, ])
  # Combine all in one kable
  # timing <- subset(subset(functdata_diet, Subcategory == "Timing"), select = -c(1,2,4:6))
  df2 <- subset.data.frame(df_BSS_sum, select = c(3))
  df2 <- na.omit(df2); df1[df1 == "no"] <- "NA"; df1[df1 == ""] <- "NA"; df1 <- na.omit(df1)
  # grid.arrange(text_grob(label = "f", face = 'bold', size = 16), tableGrob(df2),
  # tableGrob(df_Def), nrow = 3, heights = c(1,0.5,1))
  dftest
  BSS <- df2
  if(BSS > 3 & dftest <4){
    transitadvice <- "short transit time"; trans <- "Short"
  }else{
    if(BSS < 4 & BSS > 2 & dftest >3 & dftest < 6){
      transitadvice <- "medium transit time"; trans <- "Medium"
    }else{if(BSS < 2.1 & dftest > 5){
      transitadvice <- "long transit time"; trans <- "Long"
    }else{
      transitadvice <- "problem"; trans <- "unknown"
    }

    }
  }

  if(transitadvice != "problem"){
    message(paste0("\nYour donor has a mean bristol stool score of ", crayon::white(df2), " and defecates ", crayon::white(df1), ".\n", "We therefore categorise the donor as a ", crayon::white(transitadvice), " donor. This matches the following SHIME configuration"))


    df <- data.frame("Transit_time" = c("Short" , "Medium" , "Long"), "τ_.PC." = c(8,16,24), "V_.PC." = c(200,400,600), "Faecal_inoculum_.PC." = c(10,20,30),
                     "τ_.DC." = c(13,26,39), "V_.DC." = c(325,650,975), "Faecal_inoculum_.DC." = c(16.25,32.50,48.75), "Amount" = c(24,48,72))
    df <- subset(df, Transit_time == trans)
    df <- rename(df, "Transit time"= Transit_time, "τ (PC)"= τ_.PC., "V (PC)" = V_.PC., "Faecal inoculum (PC)" = Faecal_inoculum_.PC.,
                 "τ (DC)"= τ_.DC., "V (DC)" = V_.DC., "Faecal inoculum (DC)" = Faecal_inoculum_.DC., "Amount of beads" = Amount)
    print(df)
    message(paste0("Type ", crayon::blue("FFQ.transit.info()"), " for more information on how to adapt your SHIME accordingly."))
  }else{
    message(paste0("\nYour donor has a mean bristol stool score of ", crayon::white(df2), " and defecates ", crayon::white(df1), ".\n", "These values do not correspond to the strict classification rules. You will have to estimate it yourself. Type ", crayon::blue("FFQ.transit.info()"), " for more information on how to adapt your SHIME accordingly."))
  }
}
