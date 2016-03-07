# debugging the yourpath() function:

# background:
# yourpath() works when input sample.frame = NULL, but throws error otherwise:
# Error in if (as.integer(rownames(data[[i]])[nrow(data[[i]])]) != sample.frame[4]) { : 
#     missing value where TRUE/FALSE needed
#   In addition: Warning message:
#     NAs introduced by coercion 

rm(list=ls())
load("to brant 01.RData")
# replicate environment from "to brant 01.R"

# replicate arguments from "to brant 01.R" lines 134:138

dpath 
tag
year.var
sample.frame
verbose

# function (
  # dpath = getwd(), tag = "csid", 
index.code = "ggggaa"; datalist = NULL; G.names = NULL; A.names = NULL; T.names = NULL; 
proximity = NULL; 
# year.var = FALSE; sample.frame = NULL
summary = FALSE 
# verbose = FALSE, 
lag = NULL; formula = NULL; vars.nolag = NULL
# ) 
{
  datanames <- dir(dpath)[grep(tag, dir(dpath), ignore.case = TRUE)]

# paste(tag, "\\.", sep="") would only match csid.csv
  if (length(grep(paste(tag, "\\.", sep = ""), datanames)) != 
  # if (length(grep(paste(tag, ".+\\.", sep = ""), datanames)) != 
      0) {
    datanames <- datanames[-grepl(paste(tag, "\\.", sep = ""), 
                                 datanames)]
  }
  if (is.na(datanames[1]) && is.null(datalist)) {
    stop(paste("'dpath'\ndoes not have properly labeled cross section objects; make to include\n'", 
               tag, "' before CSID code or use 'datalist' option to include objects\nin R workspace.", 
               sep = ""))
  }
  data <- vector("list", length(datanames))
  cat("Loading cross section files and checking for errors...\n")
  data <- lapply(datanames, reader, dpath, year.var, sample.frame, 
                 verbose)
  dnames <- lapply(datanames, splitter, tag, index.code)
  names(data) <- dnames
  if (!is.null(datalist)) {
    if (verbose) {
      cat("Loading objects in 'datalist'...\n")
    }
    if (!is.list(datalist)) {
      stop(cat("'datalist' object is not a list."))
    }
    listnames <- names(datalist)
    for (i in 1:length(datalist)) {
      if (nchar(listnames[i]) != nchar(index.code) || 
          is.na(as.numeric(listnames[i]))) {
        stop(paste("Cross section name '", listnames[i], 
                   "' not ", nchar(index.code), "-digit numeric CSID", 
                   sep = ""))
      }
      if (year.var) {
        if (is.null(rownames(datalist[[i]])) || rownames(datalist[[i]])[1] == 
            "1") {
          rownames(datalist[[i]]) <- datalist[[i]]$year
          datalist[[i]]$year <- NULL
        }
      }
      if (is.null(rownames(datalist[[i]])) || rownames(datalist[[i]])[1] == 
          "1") {
        stop(paste("Cross section '", listnames[i], 
                   "' does not have rownames or merely has index ('1'...'N') rownames. Please add rownames that reflect year of time series.", 
                   sep = ""))
      }
      if (verbose) {
        cat(listnames[i], "\n")
      }
    }
  }
  data <- c(data, datalist)
  nt <- nchar(rownames(data[[1]])[1])
  t <- paste(rep("t", nt), sep = "", collapse = "")
  index.code <- paste(index.code, t, sep = "", collapse = "")
  if (!is.null(lag)) {
    if (!is.numeric(lag)) {
      stop("'lag' must be numeric")
    }
    formula <- as.formula(formula)
    response <- all.vars(formula)[1]
    covars <- all.vars(formula)[-1]
    if (length(grep("^index$", covars)) > 0) {
      index.add <- TRUE
    }
    covars <- covars[-grep("index", covars)]
    if (!is.null(vars.nolag)) {
      pos.nolag <- unlist(sapply(vars.nolag, grep, x = covars))
      if (length(pos.nolag) > 0) {
        covars <- covars[-c(pos.nolag)]
      }
    }
    years.tot <- sample.frame[1]:sample.frame[4]
    years.insamp <- sample.frame[1]:sample.frame[2]
    years.pred <- sample.frame[3]:sample.frame[4]
    years.lag <- years.tot - lag
    split.index <- strsplit(index.code, "")[[1]]
    N.g <- length(split.index[split.index == "g"])
    csids <- names(data)
    geo.codes <- sapply(csids, substr, start = 1, stop = N.g)
    geolist <- unique(geo.codes)
    data.geo <- lapply(geolist, lag.gen, data = data, split.index = split.index, 
                       N.g = N.g, lag = lag, years.insamp = years.insamp, 
                       years.pred = years.pred, years.lag = years.lag, 
                       years.tot = years.tot, sample.frame = sample.frame, 
                       response = response, covars = covars, index.add = index.add, 
                       vars.nolag = vars.nolag)
    data <- data.geo[[1]]
    if (length(data.geo) > 1) {
      for (i in 2:length(data.geo)) {
        data <- append(data, data.geo[[i]])
      }
    }
  }
  
  #### works up to this point: CSIDs not found
  
  if (!is.null(sample.frame)) {
    csids <- names(data)
    i <- 1
    for (i in 1:length(data)) {
      if (as.integer(rownames(data[[i]])[nrow(data[[i]])]) != 
          sample.frame[4]) {
        message(paste("Cross section '", csids[i], "' (and possibly others) ends at year '", 
                      rownames(data[[i]])[nrow(data[[i]])], "' and not '", 
                      sample.frame[4], "'", ". Be sure to include all years up to last predicted year and no years past that year.", 
                      sep = ""))
      }
      if (as.integer(rownames(data[[i]])[nrow(data[[i]])]) - 
          as.integer(rownames(data[[i]])[1]) + 1 != nrow(data[[i]])) {
        stop(paste("Missing years in cross section '", 
                   csids[i], "'. Be sure to include all years from first observed year to last predicted year, even if NA.", 
                   sep = ""))
      }
    }
  }
  dataobj <- list(data = data, index.code = index.code)
  if (verbose) {
    if (any(length(grep(paste(tag, ".G.names", sep = ""), 
                        dir(dpath))) == 1, !is.null(G.names), length(grep(paste(tag, 
                                                                                ".A.names", sep = ""), dir(dpath))) == 1, !is.null(A.names), 
            length(grep(paste(tag, ".T.names", sep = ""), dir(dpath))) == 
            1, !is.null(T.names), length(grep(paste(tag, 
                                                    ".proximity", sep = ""), dir(dpath))) == 1, 
            !is.null(proximity))) {
      cat("Loading auxiliary files...\n")
    }
  }
  if (!is.null(G.names)) {
    dataobj$G.names <- readerlite(G.names, dpath, verbose)
  }
  else if (length(grep(paste(tag, ".G.names", sep = ""), dir(dpath))) == 
           1) {
    dataobj$G.names <- readerlite(dir(dpath)[grep(paste(tag, 
                                                        ".G.names", sep = ""), dir(dpath))], dpath, verbose)
  }
  if (!is.null(A.names)) {
    dataobj$A.names <- readerlite(A.names, dpath, verbose)
  }
  else if (length(grep(paste(tag, ".A.names", sep = ""), dir(dpath))) == 
           1) {
    dataobj$A.names <- readerlite(dir(dpath)[grep(paste(tag, 
                                                        ".A.names", sep = ""), dir(dpath))], dpath, verbose)
  }
  if (!is.null(T.names)) {
    dataobj$T.names <- readerlite(T.names, dpath, verbose)
  }
  else if (length(grep(paste(tag, ".T.names", sep = ""), dir(dpath))) == 
           1) {
    dataobj$T.names <- readerlite(dir(dpath)[grep(paste(tag, 
                                                        ".T.names", sep = ""), dir(dpath))], dpath, verbose)
  }
  if (!is.null(proximity)) {
    dataobj$proximity <- readerlite(proximity, dpath, verbose)
  }
  else if (length(grep(paste(tag, ".proximity", sep = ""), 
                       dir(dpath))) == 1) {
    dataobj$proximity <- readerlite(dir(dpath)[grep(paste(tag, 
                                                          ".proximity", sep = ""), dir(dpath))], dpath, verbose)
  }
  if (!is.null(dataobj$proximity)) {
    dataobj$proximity <- data.matrix(dataobj$proximity)
  }
  cat("...Finished\n")
  class(dataobj) <- "yourprep"
  if (summary) {
    cat("Variable Means Summary Output:\n")
    print(lapply(dataobj$data, mean, na.rm = TRUE))
  }
  cat("Total number of cross sections:", length(dataobj$data), 
      "\n")
  return(dataobj)
}

