#' Add food items to reference list
#'
#' @return Add food items to the reference lists in the FFQ object.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @export
#' @examples
#' FFQobject <- FFQ.add.fruit(FFQobject = FFQobject, Fooditem = "Avocado", Average_weight = 170, Amount_of_fibre_g.100g = 6.7, SDF_Hemicellulose =0.5, SDF_Pectin=0.8, ISD_Hemicellulose=0.9, ISD_Cellulose=1.4, ISD_Pectin=0.2, ISD_Lignin=0,1, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0)



FFQ.add.fruit <- function (FFQobject, Fooditem, Average_weight, Amount_of_fibre_g.100g, SDF_Hemicellulose =0,
                           SDF_Pectin=0, ISD_Hemicellulose=0, ISD_Cellulose=0, ISD_Pectin=0, ISD_Lignin=0, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0) {
  df <- data.frame(ï..Fruits = Fooditem, Category = "Diet", Subcategory = "Fruit",
                   Group = Fooditem, Average_weight = Average_weight,
                   Amount_of_fibre_g.100g = Amount_of_fibre_g.100g, SDF_Hemicellulose = SDF_Hemicellulose,
                   SDF_Pectin = SDF_Pectin, ISD_Hemicellulose = ISD_Hemicellulose, ISD_Cellulose = ISD_Cellulose, ISD_Pectin = ISD_Pectin, ISD_Lignin = ISD_Lignin, RS1 =RS1, RS2 = RS2, RS3 = RS3, Chitin = Chitin, Fructan = Fructan)

  FFQobject@references$fruit <- rbind(FFQobject@references$fruit, df)

  return(FFQobject)
}



#' Add food items to reference list
#'
#' @return Add food items to the reference lists in the FFQ object.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @export
#' @examples
#' FFQobject <- FFQ.add.fruit(FFQobject = FFQobject, Fooditem = "Avocado", Average_weight = 170, Amount_of_fibre_g.100g = 6.7, SDF_Hemicellulose =0.5, SDF_Pectin=0.8, ISD_Hemicellulose=0.9, ISD_Cellulose=1.4, ISD_Pectin=0.2, ISD_Lignin=0,1, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0)
FFQ.add.vegetable <- function (FFQobject, Fooditem, Average_weight, Amount_of_fibre_g.100g, SDF_Hemicellulose =0,
                               SDF_Pectin=0, ISD_Hemicellulose=0, ISD_Cellulose=0, ISD_Pectin=0, ISD_Lignin=0, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0) {
  df <- data.frame(Vegetables = Fooditem, Category = "Diet", Subcategory = "Vegetable",
                   Group = Fooditem, Average_weight = Average_weight,
                   Amount_of_fibre_g.100g = Amount_of_fibre_g.100g, SDF_Hemicellulose = SDF_Hemicellulose,
                   SDF_Pectin = SDF_Pectin, ISD_Hemicellulose = ISD_Hemicellulose, ISD_Cellulose = ISD_Cellulose, ISD_Pectin = ISD_Pectin, ISD_Lignin = ISD_Lignin, RS1 =RS1, RS2 = RS2, RS3 = RS3, Chitin = Chitin, Fructan = Fructan)

  FFQobject@references$vegetable <- rbind(FFQobject@references$vegetable, df)

  return(FFQobject)
}


#' Add food items to reference list
#'
#' @return Add food items to the reference lists in the FFQ object.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @export
#' @examples
#' FFQobject <- FFQ.add.fruit(FFQobject = FFQobject, Fooditem = "Avocado", Average_weight = 170, Amount_of_fibre_g.100g = 6.7, SDF_Hemicellulose =0.5, SDF_Pectin=0.8, ISD_Hemicellulose=0.9, ISD_Cellulose=1.4, ISD_Pectin=0.2, ISD_Lignin=0,1, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0)
FFQ.add.nut <- function (FFQobject, Fooditem, Average_weight, Amount_of_fibre_g.100g, SDF_Hemicellulose =0,
                         SDF_Pectin=0, ISD_Hemicellulose=0, ISD_Cellulose=0, ISD_Pectin=0, ISD_Lignin=0, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0) {
  df <- data.frame(ï..Nuts = Fooditem, Category = "Diet", Subcategory = "Nut",
                   Group = Fooditem, Average_weight = Average_weight,
                   Amount_of_fibre_g.100g = Amount_of_fibre_g.100g, SDF_Hemicellulose = SDF_Hemicellulose,
                   SDF_Pectin = SDF_Pectin, ISD_Hemicellulose = ISD_Hemicellulose, ISD_Cellulose = ISD_Cellulose, ISD_Pectin = ISD_Pectin, ISD_Lignin = ISD_Lignin, RS1 =RS1, RS2 = RS2, RS3 = RS3, Chitin = Chitin, Fructan = Fructan)

  FFQobject@references$nut <- rbind(FFQobject@references$nut, df)

  return(FFQobject)
}


#' Add food items to reference list
#'
#' @return Add food items to the reference lists in the FFQ object.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @export
#' @examples
#' FFQobject <- FFQ.add.fruit(FFQobject = FFQobject, Fooditem = "Avocado", Average_weight = 170, Amount_of_fibre_g.100g = 6.7, SDF_Hemicellulose =0.5, SDF_Pectin=0.8, ISD_Hemicellulose=0.9, ISD_Cellulose=1.4, ISD_Pectin=0.2, ISD_Lignin=0,1, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0)
FFQ.add.grain <- function (FFQobject, Fooditem, Average_weight, SDF_Hemicellulose =0,
                           SDF_Pectin=0, ISD_Hemicellulose=0, ISD_Cellulose=0, ISD_Pectin=0, ISD_Lignin=0, RS1=0, RS2=0, RS3=0, Chitin=0, Fructan=0, B_glucan = 0) {
  df <- data.frame(Grain_products = Fooditem, Category = "Diet", Subcategory = "Grain",
                   Group = Fooditem, average_weight = Average_weight,
                   SDF_Hemicellulose = SDF_Hemicellulose,
                   SDF_Pectin = SDF_Pectin, ISD_Hemicellulose = ISD_Hemicellulose, ISD_Cellulose = ISD_Cellulose, ISD_Pectin = ISD_Pectin, ISD_Lignin = ISD_Lignin, RS1 =RS1, RS2 = RS2, RS3 = RS3, Chitin = Chitin, Fructan = Fructan, B_glucan = B_glucan)

  FFQobject@references$grain <- rbind(FFQobject@references$grain, df)

  return(FFQobject)
}
