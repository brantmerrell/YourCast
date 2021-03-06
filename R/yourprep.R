# Function to recognize and read data file and perform checks

reader <- function(filename,dpath,year.var,sample.frame,
                   verbose) {
  # What type of file is that data?
  if (length(grep(".txt",filename,ignore.case=TRUE))>0) {
    cs <- read.table(paste(dpath,"/",filename,sep=""),header=TRUE)
  }

  else if (length(grep(".csv",filename,ignore.case=TRUE))>0) {
    cs <- read.csv(paste(dpath,"/",filename,sep=""),header=TRUE)
  }

  else if (length(grep(".dta",filename,ignore.case=TRUE))>0) {
    cs <- read.dta(paste(dpath,"/",filename,sep=""))
  }
  
  else {stop(paste("File type for '",filename,"' not recognized. Please make sure your file is in '.txt', '.csv', or '.dta' format and has the extension in the filename. If using '.RData' format, please load objects into list on workspace and use 'datalist' option",
                   sep=""))}

  if(verbose) {cat(filename,"\n")}
  # if year is a variable rather than the row label, make it
  # the row label and drop the variable "year"
  if(year.var) {
    if(is.null(rownames(cs)) || rownames(cs)[1]=="1")
    {rownames(cs) <- cs$year
    cs$year <- NULL}
  }

  # Does cross section now have rownames?
  if(is.null(rownames(cs)) || rownames(cs)[1]=="1")
    {stop(paste("Cross section '",filename,
                "' does not have rownames or merely has index ('1'...'N') rownames. Please add rownames that reflect year of time series.",
                sep=""))}

#  # make sure cross section ends at last predicted year
#  if(!is.null(sample.frame)) {
#    if(as.integer(rownames(cs[nrow(cs),]))!=sample.frame[4])
#      {stop(paste("Cross section '",filename,"' ends at year '",
#                  as.integer(rownames(cs[nrow(cs),])),"' and not '",
#                  sample.frame[4],"'",
#                  ". Be sure to include all years up to last predicted year.",
#                  sep=""))
#     }
#                
#  # check whether all years after first observed year are included in
#  # dataframe
#  if(as.integer(rownames(cs[nrow(cs),]))-as.integer(rownames(cs[1,]))
#     +1!=nrow(cs))
#    {stop(paste("Missing years in cross section '",filename,
#                "'. Be sure to include all years from first observed year to last predicted year, even if NA.",
#                sep=""))}
#                               }
  return(cs)
}

################################################################

# Function for reading in auxillary files from 'dpath'
# Same as 'reader' function but far simpler

readerlite <- function(filename,dpath,verbose) {
  # What type of file is that data?
  if (length(grep(".txt",filename,ignore.case=TRUE))>0) {
    cs <- read.table(paste(dpath,"/",filename,sep=""),header=TRUE,
                     colClasses="character")
  }

  else if (length(grep(".csv",filename,ignore.case=TRUE))>0) {
    cs <- read.csv(paste(dpath,"/",filename,sep=""),header=TRUE,
                   colClasses="character")
  }

  else if (length(grep(".dta",filename,ignore.case=TRUE))>0) {
    cs <- read.dta(paste(dpath,"/",filename,sep=""))
  }
  
  else {stop(paste("File type for '",filename,"' not recognized. Please make sure your file is in '.txt', '.csv', or '.dta' format and has the extension in the filename.",
                   sep=""))}
  if(verbose) {cat(filename,"\n")}

  return(cs)
}

################################################################

# Function to separate CSID tag from the rest of the file name

splitter <- function(string,tag,index.code) {

  # splitting digits in center from rest of file name
  code <- gsub("[^[:digit:]^]","",string)
  code <- substr(code,nchar(code)-nchar(index.code)+1,nchar(code))

  # Check to see if have numeric code portion
  if(is.na(as.numeric(code))){stop(paste("Cross section name after '",tag,
                 "' tag and before extension is not numeric.",
                 sep=""))}
  
  
  # check whether cross section label is number with correct number
  # of digits
   if(nchar(code)!=nchar(index.code))
     {stop(paste("Cross section name after '",tag,
                 "' tag and before extension not ",
                 nchar(index.code),
                 "-digit numeric CSID for file '",string,"'.",
                 sep=""))}
  return(code)
}

################################################################

yourprep <- function(dpath=getwd(),tag="csid",index.code="ggggaa",
                     datalist=NULL,G.names=NULL,A.names=NULL,
                     T.names=NULL,proximity=NULL,year.var=FALSE,
                     sample.frame=NULL,summary=FALSE,verbose=FALSE,

                     #lagging utility
                     lag=NULL,formula=NULL,vars.nolag=NULL) {
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



####################################################
