#' Convert data frame into FFQ object
#'
#' @return A custom object that contains everything required for downstream processing
#' @param FFQ_dataframe Dataframe exported from QuestionPro
#' @param Object Transform .
#' @export
#' @examples
#' FFQobject <- FFQ.convert.dataframe(FFQ_dataframe)


FFQ.convert.dataframe <- function (dataframe, Object = "FFQ", datalist = "internal", datalist2 = datalist) {


  if(datalist == "internal"){datalist <- CMET.FFQ::datalist}else{datalist <- datalist}
  functdata <- dataframe

  if(ncol(functdata) > 1051) {functdata <- subset.data.frame(functdata, select = c(1:1051))}
  ## Remove blank rows
  functdata <- functdata[!with(functdata, is.na(Region)),]

  ## If more columns than questions were loaded, remove the unnecessary ones
  if(ncol(functdata) > 1051) {functdata <- subset.data.frame(functdata, select = c(1:1051))}
  ## Name code as header
  row.names(functdata) <- functdata$X2...Q71
  ## Transpose the data frame
  functdata <- as.data.frame(t(functdata))

  # Import question titles to convert FFQ questions
  # questiondata <- read.csv2("Question_conversion.csv", fileEncoding="latin1", header = T, sep=",")
  questiondata <- datalist$questiondata

  # Combine questions with functional data
  functdata <- tibble::rownames_to_column(functdata, "codes.pt.")
  functdata <- cbind(Question_conversion = questiondata$Question_conversion,
                     Question_codes.pt. = questiondata$Question_codes.pt.,
                     Category = questiondata$Category,
                     Subcategory = questiondata$Subcategory,
                     Group = questiondata$Group,
                     Subgroup = questiondata$Subgroup,
                     Row = questiondata$Row,
                     Base_unit = questiondata$Base_unit,
                     functdata)

  row.names(functdata) <- questiondata$Question_conversion

  # Manually check if questions in functdata allign with questiondata
  manual_alignment_check <- subset.data.frame(functdata,
                                              select = c(codes.pt., Question_conversion,
                                                         Question_codes.pt.))
  functdata <-subset.data.frame(functdata, select = -c(codes.pt., Question_conversion, Question_codes.pt.))
  if(Object == "FFQ"){

    # functdata1 <- as.matrix(functdata)
    functdata1 <- functdata

    # setClass("FFQobject", representation(functdata = "data.frame", subsections= "list",
    #                                      total.fibres.per.group = "list", fibres.per.item = "list",
    #                                      group.timing = "list", references = "list"))

    # Add fibre references to FFQ object
    transformationdatafruit <- datalist$transformationdatafruit
    # transformationdatafruit <- read.csv2("Transformation_fruit.csv", fileEncoding="latin1", header = T, sep=";")
    transformationdatafruit <- subset.data.frame(transformationdatafruit, Group != 0)

    transformationdataveg <- datalist$transformationdataveg
    # transformationdataveg <- read.csv2("Transformation_vegetable.csv", fileEncoding="latin1", header = T, sep=";")
    transformationdataveg <- subset.data.frame(transformationdataveg, Group != 0)

    transformationdatanut <- datalist$transformationdatanut
    # transformationdatanut <- read.csv2("Transformation_nut.csv", fileEncoding="latin1", header = T, sep=";")
    transformationdatanut <- subset.data.frame(transformationdatanut, Group != 0)

    transformationdatagrain <- datalist$transformationdatgrain
    # transformationdatagrain <- read.csv2("Transformation_grain.csv", fileEncoding="latin1", header = T, sep=";")
    transformationdatagrain <- subset.data.frame(transformationdatagrain, average_weight != 0)

    referencelist <- list(fruit = transformationdatafruit, vegetable = transformationdataveg,
                          nut = transformationdatanut, grain = transformationdatagrain )


    # functdata2@functdata <- functdata1
    # functdata2@subsections$general <- subset.data.frame(functdata, functdata$Category == "General")
    # functdata2@subsections$health <- subset.data.frame(functdata, functdata$Category == "Health")
    # functdata2@subsections$diet <- subset.data.frame(functdata, functdata$Category == "Diet")
    # functdata2@subsections$timing <- subset.data.frame(subset.data.frame(subset.data.frame(functdata,
    #                                             functdata$Category == "Diet"), select = -c(4,5,6)),
    #                                                                subset.data.frame(functdata, functdata$Category == "Diet")$Subcategory == "Timing")
    # functdata2@references <- referencelist

    functdata2 = new("FFQobject", functdata = functdata1,
                    subsections = list(general = subset.data.frame(functdata, functdata$Category == "General"),
                                       health = subset.data.frame(functdata, functdata$Category == "Health"),
                                       diet = subset.data.frame(functdata, functdata$Category == "Diet"),
                                       timing = subset.data.frame(subset.data.frame(subset.data.frame(functdata,
                                                                                                      functdata$Category == "Diet"), select = -c(4,5,6)),
                                                                  subset.data.frame(functdata, functdata$Category == "Diet")$Subcategory == "Timing")), references = referencelist)
  }else{if(Object == "list"){
    functdata2 <- list(functdata = functdata, general = subset.data.frame(functdata, functdata$Category == "General"),
                      health = subset.data.frame(functdata, functdata$Category == "Health"),
                      diet = subset.data.frame(functdata, functdata$Category == "Diet"),
                      timing = subset.data.frame(subset.data.frame(subset.data.frame(functdata, functdata$Category == "Diet"),
                                                                   select = -c(4,5,6)),
                                                 subset.data.frame(functdata, functdata$Category == "Diet")$Subcategory == "Timing"))
  }else{stop("Object type is not defined")}}
  return(functdata2)
}
