#' Calculate the fibre consumptions from all food groups
#'
#' @return Calculate the fibre consumptions from all food groups out of the FFQ object.
#'
#' @param FFQobject FFQ object with all FFQ data
#' @param ignore.groups Check if FFQ food items are present in reference list. Default = FALSE.
#' @export
#' @examples
#' FFQobject_all_fibres <- FFQ.calculate.fibres(FFQobject)



FFQ.calculate.fibres <- function (FFQobject, ignore.groups = FALSE){
  FFQobject <- CMET.FFQ::FFQ.calculate.fruit.fibres(FFQobject = FFQobject, ignore.groups = ignore.groups)
  FFQobject <- CMET.FFQ::FFQ.calculate.vegetable.fibres(FFQobject = FFQobject, ignore.groups = ignore.groups)
  FFQobject <- CMET.FFQ::FFQ.calculate.nut.fibres(FFQobject = FFQobject, ignore.groups = ignore.groups)
  FFQobject <- CMET.FFQ::FFQ.calculate.bread.fibres(FFQobject = FFQobject, ignore.groups = ignore.groups)
  FFQobject <- CMET.FFQ::FFQ.calculate.cereal.fibres(FFQobject = FFQobject, ignore.groups = ignore.groups)
  FFQobject <- CMET.FFQ::FFQ.calculate.pasta.fibres(FFQobject = FFQobject, ignore.groups = ignore.groups)

  cat(crayon::yellow(c("                                     ','. '. ; : ,','\n",
                       "                                       '..'.,',..'\n",
                       "                                          ';.'  ,'\n",
                       "                                           ;;\n",
                       "                                           ;'\n",
                       "                             :._   _.------------.___\n",
                       "                     __      :__:-'                  '--.\n",
                       "              __   ,' .'    .'             ______________'.\n",
                       "            /__ '.-  ____.'          0  .' .'  .'  _.-_.'\n",
                       "               '._YM                 .-': .' _.' _.'_.'\n",
                       "                '----'._____________.'_'._:_:_.-'--'\n",
                       "                             \n",
                       "                          HAPPY WHALE IS HAPPY"
  ) , sep = "\n"))


  return(FFQobject)
}
