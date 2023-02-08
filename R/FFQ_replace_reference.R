#' Replace refences
#'
#' @return Replace fibre reference in FFQ object.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @param New.reference a dataframe of the new reference
#' @param Reference.type The type of reference. Possibilities are: "fruit", "vegetable", "nut" or "grain".
#' @export
#' @examples
#' FFQobject_updated <- FFQ.replace.reference(FFQobject, dataframe_fruits, "fruit")


FFQ.replace.reference <- function (FFQobject, New.reference,Reference.type){

  if(Reference.type == "fruit"){
    FFQobject@references$fruit <- New.reference
  }else{
    if(Reference.type == "vegetable"){
      FFQobject@references$vegetable <- New.reference
    }else{if(Reference.type == "nut"){
      FFQobject@references$nut <- New.reference
    }else{if(Reference.type == "fruit"){
      FFQobject@references$grain <- New.reference
          }else{stop("Define your reference type!")}
    }
      }
  }

  return(FFQobject)

}
