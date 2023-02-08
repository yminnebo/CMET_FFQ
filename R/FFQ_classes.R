#' @keywords internal
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))
#' @keywords internal
setClassUnion("listOrNULL", c("list", "NULL"))



################################################################################
#' The main experiment-level class for FFQobject data

#' In the case of missing component data, the slots are set to \code{NULL}. As
#' soon as a \code{FFQobject-class} object is to be updated with new component
#' data (previously missing/\code{NULL} or not), the indices of all components
#' are re-checked for compatibility and trimmed if necessary. This is to ensure
#' by design that components describe the same taxa/samples, and also that these
#' trimming/validity checks do not need to be repeated in downstream analyses.



#' @name FFQobject-class
#' @rdname FFQobject-class
#' @exportClass FFQobject
#'

setClass("FFQobject", representation(functdata = "data.frameOrNULL", subsections= "listOrNULL",
                                     total.fibres.per.group = "listOrNULL", fibres.per.item = "listOrNULL",
                                     group.timing = "listOrNULL", references = "listOrNULL"),
         prototype = prototype(functdata = NULL, subsections= NULL,
                               total.fibres.per.group = NULL, fibres.per.item = NULL,
                               group.timing = NULL, references = NULL))
################################################################################
