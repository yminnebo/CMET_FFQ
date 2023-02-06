#' Calculate the fibre consumptions of bread
#'
#' @return Calculate the fibre consumptions from bread out of the FFQ object.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @param ignore.groups Check if FFQ food items are present in reference list. Default = FALSE.
#' @seealso CMET.FFQ::FFQ.calculate.fibres() .
#' @export
#' @examples
#' FFQobject_bread_fibres <- FFQ.calculate.bread.fibres(FFQobject)


FFQ.calculate.bread.fibres <- function (FFQobject, ignore.groups = FALSE){
  transformationdatagrain <- FFQobject@references$grain

  # FFQ.calculate.fibres <- function (FFQobject) {
  functdata_diet <- FFQobject@subsections$diet
  withoutdiet <- subset.data.frame(functdata_diet, select = -c(1,2,3,4,5,6)); cols = ncol(withoutdiet)
  person_list <- list(); breadfibrelist <- list(); breadtiminglist <- list()
  i=1
  # Reorder diet data in new table without other
  df_diet_d1 <- subset.data.frame(functdata_diet, select = c(1,2,3,4,5,6,6+i))
  df_diet_d1$combined <- paste(df_diet_d1$Subgroup, df_diet_d1$Base_unit, sep = "_")
  df_diet_d1 <- subset(df_diet_d1, Subcategory == "Bread" &  #WEGLATEN WANNEER WE MEER DAN bread HEBBEN
                         Group != "Frequency" & Group != "Meal_included" & Group != "Other")
  df_diet_d1_1 <- subset(df_diet_d1, df_diet_d1$combined == "Quantity_pieces")
  df_diet_d1_2 <- subset(df_diet_d1, df_diet_d1$combined == "Quantity_g")
  df_diet_d1_3 <- subset(df_diet_d1, df_diet_d1$combined == "Time_")
  rownames(df_diet_d1_1) <- df_diet_d1_1$Group
  rownames(df_diet_d1_2) <- df_diet_d1_1$Group
  rownames(df_diet_d1_3) <- df_diet_d1_1$Group
  df_diet_d1 <- as.data.frame(cbind(Category = df_diet_d1_1$Category,
                                    Subcategory = df_diet_d1_1$Subcategory,
                                    Group = df_diet_d1_1$Group,
                                    Subgroup = df_diet_d1_1$Subgroup,
                                    Base_unit = df_diet_d1_1$Base_unit,
                                    Quantity_pieces = df_diet_d1_1[,7],
                                    Quantity_g = df_diet_d1_2[,7],
                                    Time  = df_diet_d1_3[,7]))
  # Remove NAs in the dataframe
  df_diet_d1 <- df_diet_d1[!with(df_diet_d1,
                                 is.na(Quantity_pieces)& is.na(Quantity_g)),]
  # Reorder diet data in new table with other
  df_diet_d2 <- subset.data.frame(functdata_diet, select = c(1,2,3,4,5,6,6+i))
  df_diet_d2$combined <- paste(df_diet_d2$Subgroup, df_diet_d2$Base_unit, sep = "_")
  df_diet_d2 <- subset(df_diet_d2, Subcategory == "Bread" &  #WEGLATEN WANNEER WE MEER DAN bread HEBBEN
                         Group != "Frequency" & Group != "Meal_included" & Group == "Other")
  df_diet_d2_1 <- subset(df_diet_d2, df_diet_d2$combined == "Quantity_pieces")
  df_diet_d2_2 <- subset(df_diet_d2, df_diet_d2$combined == "Quantity_g")
  df_diet_d2_3 <- subset(df_diet_d2, df_diet_d2$combined == "Time_")
  df_diet_d2_4 <- subset(df_diet_d2, df_diet_d2$combined == "Type_")
  df_diet_d2 <- as.data.frame(cbind(Category = df_diet_d2_1$Category,
                                    Subcategory = df_diet_d2_1$Subcategory,
                                    Group = df_diet_d2_4[,7],
                                    Subgroup = df_diet_d2_1$Subgroup,
                                    Base_unit = df_diet_d2_1$Base_unit,
                                    Quantity_pieces = df_diet_d2_1[,7],
                                    Quantity_g = df_diet_d2_2[,7],
                                    Time  = df_diet_d2_3[,7]))
  df_diet_d2 <- df_diet_d2[!with(df_diet_d2, is.na(Group)),]
  ## Remove NAs and 0s from groups
  df_diet_d2 <- df_diet_d2[!with(df_diet_d2, is.na(Group)),]
  df_diet_d2 <- subset(df_diet_d2, Group != 0)
  df_diet_d2 <- subset(df_diet_d2, Group != "na")
  df_diet_d2 <- subset(df_diet_d2, Group != "")

  rownames(df_diet_d2) <- df_diet_d2$Group
  # Join our sources with test persons own sources
  df_diet_d <- rbind(df_diet_d1, df_diet_d2)
  # Check if bread groups are present in our list
  if(ignore.groups == FALSE){
    x <- data.frame(Check = df_diet_d$Group %in% transformationdatagrain$Group, Group = df_diet_d$Group)
    x <- subset(x, Check == FALSE)
    if(length(unique(x$Check)) > 0){stop(message("Woops!"), paste0("The following groups are not included in the reference list: ", x$Group, "\nYou can add bread to the list with the FFQ.add.bread() function"))}
  }
  ## Join with diet data
  df_diet_d <- left_join(df_diet_d,transformationdatagrain, by = "Group")
  df_diet_d <- subset.data.frame(df_diet_d, select = -c(Category.y, Subcategory.y))
  df_diet_d <- rename(df_diet_d, Category = Category.x, Subcategory = Subcategory.x)
  ## Add time column and divide grams per time unit
  df_diet_d$Timed <- df_diet_d$Time
  df_diet_d$Timed[df_diet_d$Timed == "d"] <- 1
  df_diet_d$Timed[df_diet_d$Timed == "w"] <- 7
  df_diet_d$Timed[df_diet_d$Timed == "m"] <- 30
  df_diet_d$Timed[df_diet_d$Timed == "d/s"] <- 1
  df_diet_d$Timed[df_diet_d$Timed == "w/s"] <- 7
  df_diet_d$Timed[df_diet_d$Timed == "m/s"] <- 30
  ## Divide nutrient types per grams
  test2 <- data.frame()
  for(j in 1:nrow(df_diet_d)){
    tes <- df_diet_d[j,]
    if(is.na(tes$Quantity_g) ==T){
      tes$SDF_Hemicellulose_g<-as.numeric(tes$SDF_Hemicellulose)*as.numeric(tes$Quantity_pieces)
      tes$SDF_Pectin_g <- as.numeric(tes$SDF_Pectin) * as.numeric(tes$Quantity_pieces)
      tes$ISD_Hemicellulose_g <- as.numeric(tes$ISD_Hemicellulose) * as.numeric(tes$Quantity_pieces)
      tes$ISD_Cellulose_g <- as.numeric(tes$ISD_Cellulose) * as.numeric(tes$Quantity_pieces)
      tes$ISD_Pectin_g <- as.numeric(tes$ISD_Pectin) * as.numeric(tes$Quantity_pieces)
      tes$ISD_Lignin_g <- as.numeric(tes$ISD_lignin) * as.numeric(tes$Quantity_pieces)
      tes$RS1_g <- as.numeric(tes$RS1) * as.numeric(tes$Quantity_pieces)
      tes$RS2_g <- as.numeric(tes$RS2) * as.numeric(tes$Quantity_pieces)
      tes$RS3_g <- as.numeric(tes$RS3) * as.numeric(tes$Quantity_pieces)
      tes$Chitin_g <- as.numeric(tes$Chitin) * as.numeric(tes$Quantity_pieces)
      tes$Fructan_g <- as.numeric(tes$Fructan) * as.numeric(tes$Quantity_pieces)
      tes$B_glucan_g <- as.numeric(tes$B_glucan) * as.numeric(tes$Quantity_pieces)
    }else{
      tes$SDF_Hemicellulose_g<- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))*     as.numeric(tes$SDF_Hemicellulose)
      tes$SDF_Pectin_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$SDF_Pectin)
      tes$ISD_Hemicellulose_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$ISD_Hemicellulose)
      tes$ISD_Cellulose_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$ISD_Cellulose)
      tes$ISD_Pectin_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$ISD_Pectin)
      tes$ISD_Lignin_g <-(as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$ISD_lignin)
      tes$RS1_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$RS1)
      tes$RS2_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$RS2)
      tes$RS3_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$RS3)
      tes$Chitin_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$Chitin)
      tes$Fructan_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$Fructan)
      tes$B_glucan_g <- (as.numeric(tes$Quantity_g)/as.numeric(tes$average_weight))* as.numeric(tes$B_glucan)

    }
    test2 <- rbind(test2, tes)

  }
  df_diet_d <- test2
  df_diet_d$SDF_Hemicellulose_g<- as.numeric(df_diet_d$SDF_Hemicellulose_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$SDF_Pectin_g <- as.numeric(df_diet_d$SDF_Pectin_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$ISD_Hemicellulose_g <- as.numeric(df_diet_d$ISD_Hemicellulose_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$ISD_Cellulose_g <- as.numeric(df_diet_d$ISD_Cellulose_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$ISD_Pectin_g <- as.numeric(df_diet_d$ISD_Pectin_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$ISD_Lignin_g <- as.numeric(df_diet_d$ISD_Lignin_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$RS1_g <- as.numeric(df_diet_d$RS1_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$RS2_g <- as.numeric(df_diet_d$RS2_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$RS3_g <- as.numeric(df_diet_d$RS3_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$Chitin_g <- as.numeric(df_diet_d$Chitin_g)/as.numeric(df_diet_d$Timed)
  df_diet_d$B_glucan_g <- as.numeric(df_diet_d$B_glucan_g)/as.numeric(df_diet_d$Timed)
  df_diet_d <- subset(df_diet_d, select = -c(ISD_lignin,ISD_Pectin, ISD_Cellulose, SDF_Pectin, ISD_Hemicellulose, SDF_Hemicellulose, RS1, RS2, RS3, Chitin, Fructan, B_glucan))
  # Create it as separate data frame
  assign(paste0("functdata_bread_person_", i), df_diet_d)
  #Make list of data.frames
  assign(paste0("person_", i), person_list[[]])
  # Extract fibres
  df_diet_d1 <- subset.data.frame(df_diet_d, select = c(12:23))
  df_diet_d1 <- df_diet_d1[!with(df_diet_d1,is.na(Fructan_g)),]
  df_diet_d1 <- t(as.data.frame(colSums(df_diet_d1)))
  assign(paste0("functdata_bread_fibre_", i), df_diet_d1)
  breadfibrelist[[i]] <- df_diet_d1
  # Extract timing
  df_diet_d1 <- subset.data.frame(functdata_diet, select = c(1,2,3,4,5,6,6+i))
  df_diet_d1 <- subset(df_diet_d1, Subcategory == "Bread")
  df_diet_d1 <-subset(df_diet_d1, Group == "Frequency" | Group == "Meal_included")
  df_diet_d1 <- subset.data.frame(df_diet_d1, select = c(3,4,7))
  df_diet_d1<- df_diet_d1[complete.cases(df_diet_d1[,3]),]
  breadtiminglist[[i]] <- df_diet_d1
  FFQobject@total.fibres.per.group <- c(FFQobject@total.fibres.per.group, list(bread = breadfibrelist[[i]]))
  FFQobject@group.timing <- c(FFQobject@group.timing, list(bread = breadtiminglist[[i]]))
  FFQobject@fibres.per.item <- c(FFQobject@fibres.per.item, list(bread = df_diet_d))
  message("Bread: done")
  return(FFQobject)
}
