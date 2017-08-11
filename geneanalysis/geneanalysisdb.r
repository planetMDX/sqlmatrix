
#' write a finished differential gene expression analysis to a database
#'
#' Write a finished differential gene expression analysis to a database, done with limma.
#' Can take a second or two if the data is large.
#' @param con a PostgreSQL connection (from DBI)
#' @param id Optional; An identifier for the analysis, if not given a pseudo-random 
#' alphanumeric of length 20 will be generated, the function won't work if it isn't unique
#' @param name the name of the analysis, has to be unique among the database, as it will be 
#' searched after
#' @param fit the resulting fit of the analysis
#' @param expid Optional; An additonal identifier for the analysis, if not given a pseudo-
#' random alphnumric string of length 10 will be generated 
#' @param exp the expression data, needs to be coercible to a data.frame
#' @param designid Optional; An additonal identifier for the analysis, if not given a pseudo-
#' random alphnumric string of length 10 will be generated 
#' @param design The design matrix, either the fit has one or one is given.
#' @param contrastid Optional; An additonal identifier for the analysis, if not given a 
#' pseudo-random alphnumric string of length 10 will be generated 
#' @param contrast The contrast matrix, either the fit has it, one is given or it is simple
#' enough to not be needed, in which case the design matrix is used
#' @param overwrite Logical; Wether to overwrite data if it has the same name, defaults to 
#' FALSE. 
writeanalysislimma <- function(con, id, name, fit, expid, exp, designid, design, 
                             contrastid, contrast, overwrite = FALSE){
  #set.seed(random::randomNumbers(n = 1, min = 1, max = 999))
  if ((missing(design))) {
    design <- fit$designs
  }
  if (missing(contrast) && !is.null(fit$contrasts)) {
     contrast <- fit$contrasts
  }
  if (missing(id)) {
    id <- stringi::stri_rand_strings(1, 20)
  }
  if (missing(expid)) {
    expid <- stringi::stri_rand_strings(1, 10)
  }
  if (missing(designid)) {
    designid <- stringi::stri_rand_strings(1, 10)
  }
  if (!missing(contrast)) {
    if (missing(contrastid)) {
      contrastid <- stringi::stri_rand_strings(1, 10)
    }
    df <- as.data.frame(list(ids = c(id, expid, designid, contrastid)), 
                        row.names = c("id", "expid", "designid", "contrastid"))
  } else {
    df <- as.data.frame(list(ids = c(id, expid, designid)), 
                        row.names = c("id", "expid", "designid"))
  }
  
  dbWriteTable(con, name, df, row.names = TRUE, overwrite = overwrite)
  
  
  dbWriteTable(con, paste0(id, "exp", expid), as.data.frame(as.matrix(exp)), row.names = 
                 TRUE, overwrite = overwrite)
  
  dbWriteTable(con, paste0(id, "des", designid), as.data.frame(design), row.names = TRUE, 
               overwrite = overwrite)
  
  if (missing(contrast)) {
    for (i in colnames(design)) {
      dbWriteTable(con, paste0(id, "tTb",  i), topTable(fit, coef = i, n = nrow(fit)),
                   row.names = TRUE, overwrite = overwrite)
    }
  } else {
    dbWriteTable(con, paste0(id, "con", contrastid), as.data.frame(contrast), row.names = 
                   TRUE, 
                 overwrite = overwrite)
    for (i in colnames(contrast)) {
      
      dbWriteTable(con, paste0(id, "tTb", i), topTable(fit, coef = i, n = nrow(fit)), 
                   row.names = TRUE, overwrite = overwrite)
    
    }
  }
}
#' read a differential gene expression analysis from a database
#'
#' Read a differential gene expression analysis (done with limma) from a database.
#' Can take a second or two depending on the size of the analysis
#' @param con a PostgreSQL connection (from DBI)
#' @param name the name of the analysis
#'
#' @return The function returns a list containing the id used for the analysis, a data.frame
#' of the expression data, the design-data.frame, the contrast-data.frame (if one was input
#' someway) and a list of the topTables.  
getanalysislimma <- function(con, name) {
  a <- dbReadTable(con, name, row.names = "row.names")
  exp <- dbReadTable(con, paste0(a["id", ], "exp", a["expid", ]), 
                               row.names = "row.names")
  
  design <- dbReadTable(con, paste0(a["id", ], "des", a["designid", ]), 
                                  row.names = "row.names")
  if (nrow(a) == 4) {
    contrast <- dbReadTable(con, paste0(a["id", ], "con", a["contrastid", ]), 
                                    row.names = "row.names")
    ttn <- paste0(a["id", ], "tTb", colnames(contrast))
    nam <- colnames(contrast)
    
  } else {
    ttn <- paste0(a["id", ], "tTb", colnames(design))
    nam <- colnames(design)
    contrast <- "There was no input contrast"
  }
  tt <- list()
  for (i in 1:length(ttn)) {

    tt[[i]] <- dbReadTable(con, ttn[i], row.names = "row.names")
  }
  names(tt)<- nam
  re <- list(id = a["id",], exp = exp, design = design, contrast = contrast, topTables = tt)
  return(re)
}
#' write a finished differential gene expression analysis to a database
#'
#' Write a finished differential gene expression analysis to a database, done with edgeR.
#' Can take a second or two if the data is large.
#' @param con a PostgreSQL connection (from DBI)
#' @param id Optional; An identifier for the analysis, if not given a pseudo-random 
#' alphanumeric of length 20 will be generated, the function won't work if it isn't unique
#' @param name the name of the analysis, has to be unique among the database, as it will be 
#' searched after
#' @param fit the resulting fit of the analysis
#' @param expid Optional; An additonal identifier for the analysis, if not given a pseudo-
#' random alphnumric string of length 10 will be generated 
#' @param exp the expression data, needs to be coercible to a data.frame
#' @param designid Optional; An additonal identifier for the analysis, if not given a pseudo-
#' random alphnumric string of length 10 will be generated 
#' @param design The design matrix, either the fit has one or one is given.
#' @param contrastid Optional; An additonal identifier for the analysis, if not given a 
#' pseudo-random alphnumric string of length 10 will be generated 
#' @param contrast The contrast matrix
#' @param overwrite Logical; Wether to overwrite data if it has the same name, defaults to 
#' FALSE. 
writeanalysisedgeR <- function(con, id, name, fit, expid, exp, designid, design, 
                               contrastid, contrast, overwrite  = FALSE) {
  #set.seed(random::randomNumbers(n = 1, min = 1, max = 999, col = 1))
  if ((missing(design))) {
    design <- fit$design
  }
  # if (missing(contrast)) {
  #   stop()
  # }
  if (missing(id)) {
    id <- stringi::stri_rand_strings(1, 20)
  }
  if (missing(expid)) {
    expid <- stringi::stri_rand_strings(1, 10)
  }
  if (missing(designid)) {
    designid <- stringi::stri_rand_strings(1, 10)
  }
  if (missing(contrastid)) {
    contrastid <- stringi::stri_rand_strings(1, 10)
  }
  df <- as.data.frame(list(ids = c(id, expid, designid, contrastid)), 
                   row.names = c("id", "expid", "designid", "contrastid"))
  dbWriteTable(con, name, df, row.names = TRUE, overwrite = overwrite)
  
  dbWriteTable(con, paste0(id, "exp", expid), as.data.frame(as.matrix(exp)), row.names = 
                 TRUE, overwrite = overwrite)
  
  
  dbWriteTable(con, paste0(id, "des", designid), as.data.frame(design), row.names = 
                 TRUE, 
               overwrite = overwrite)
  dbWriteTable(con, paste0(id, "con", contrastid), as.data.frame(contrast), row.names = 
                 TRUE, 
               overwrite = overwrite)
  for (i in colnames(contrast)) {
    fitb <- glmLRT(fit, contrast = contrast[, i])
    dbWriteTable(con, paste0(id, "tTg", i), as.data.frame(topTags(fitb, n = nrow(fit))), 
                 row.names = TRUE, overwrite = overwrite)
  }
  
}
#' read a differential gene expression analysis from a database
#'
#' Read a differential gene expression analysis (done with edgeR) from a database.
#' Can take a second or two depending on the size of the analysis
#' @param con a PostgreSQL connection (from DBI)
#' @param name the name of the analysis
#'
#' @return The function returns a list containing the id used for the analysis, a data.frame
#' of the expression data, the design-data.frame, the contrast-data.frame
#' and a list of the topTags. 
getanalysisedgeR <- function(con, name) {
  a <- dbReadTable(con, name, row.names = "row.names")
  exp <- as.matrix(dbReadTable(con, paste0(a["id", ], "exp", a["expid", ]), 
                               row.names = "row.names"))
  design <- as.matrix(dbReadTable(con, paste0(a["id", ], "des", a["designid", ]), 
                                  row.names = "row.names"))
  contrast <- as.matrix(dbReadTable(con, paste0(a["id", ], "con", a["contrastid", ]), 
                                    row.names = "row.names"))
  ttn <- paste0(a["id", ], "tTg", colnames(contrast))
  tt <- list()
  for (i in 1:ncol(contrast)) {
    
    tt[[i]] <- dbReadTable(con, ttn[i], row.names = "row.names")
  }
  names(tt)<- colnames(contrast)
  re <- list(id = a["id", ], exp = exp, design = design, contrast = contrast, topTags = tt)
  return(re)
  
}

