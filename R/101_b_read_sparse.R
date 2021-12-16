read_sparse <- function(con = "", decode = FALSE, labels = NULL, transactions = NULL, class = NULL) {
  # Checks
    if (con == "")
      con <- stdin()
    else if (is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
      }
    if (!inherits(con, "connection"))
      stop("'con' must be a character string or connection.")

    n <- readLines(con, 1)
    if (!length(n))
      stop("the number of lines is zero")
  # read the output
    n <- as.integer(strsplit(n, " ")[[1]][5])

    x <- readLines(con)
    # control not implemented (see the -t option)
    if (FALSE) {
      k <- grep("^PRUNE", x)
      if (length(k))
        x <- x[-k]
    }
    if (!length(x))
      return(new("sequences", info = list(nsequences = n)))

    x <- strsplit(x, split = " -- ")
  # need to better define where the output is
  bin <- readBin("sparse", "raw", 10^6)
  
  numBin <- as.numeric(bin)
  numSeqs <- numBin[1] + numBin[2]*2^8 + numBin[3]*2^16 + numBin[4]*2^32
  numFreqPat <- numBin[5] + numBin[6]*2^8 + numBin[7]*2^16 + numBin[8]*2^32
  numNonZeroElem <- numBin[9] + numBin[10]*2^8 + numBin[11]*2^16 + numBin[12]*2^32
  
  # For loop 
  # ffrom 13th element to the end of the file in increments of 12
  
  rows <- vector(mode = "integer", length = numNonZeroElem)
  cols <- vector(mode = "integer", length = numNonZeroElem)
  vars <- vector(mode = "integer", length = numNonZeroElem)
  j <- 1
  for (i in seq(13, length(numBin), 12)) {
    rows[j] <- numBin[i] + numBin[i+1]*2^8 + numBin[i+2]*2^16 + numBin[i+3]*2^32
    cols[j] <- numBin[i+4] + numBin[i+5]*2^8 + numBin[i+6]*2^16 + numBin[i+7]*2^32 
    vars[j] <- numBin[i+8] + numBin[i+9]*2^8 + numBin[i+10]*2^16 + numBin[i+11]*2^32
    j <- j + 1
  }
  
  # df <- data.frame(rows = rows,
  #                  cols = cols, 
  #                  vars = vars)
  
  k_custom <- Matrix::sparseMatrix(i = rows, j = cols, x = vars)
  k_custom <- as(k_custom, "ngCMatrix")
  
    
    # NOTE 1) position 1 contains the support count.
    #      2) the following K positions contain the 
    #         support counts of a partition (see the 
    #         -c option).
    #      3) the following positions represent pairs
    #         of SID FRQ identifying the containing data 
    #         sequences and their support counts (see
    #         the -y option).
    
    c <- strsplit(sapply(x, "[", 2), split = " ")
    if (!is.null(transactions)) {
      # k <- lapply(c, function(x, i)
      #   ## see NOTE 3)
      #   x <- matrix(x[i], nrow = 2L)[1L, ],
      #   ## see NOTE 1) + 2)
      #   -seq_len(max(1L, length(levels(class))) + 1L)
      # )
      # k <- as(k, "tidLists")
      
      k <- as(k_custom, "customTidLists")
      s <- transactionInfo(k)[['labels']]
      t <- transactionInfo(transactions)[['sequenceID']]
      k@transactionInfo <- data.frame(sequenceID =
                                        if (is.factor(t))
                                          levels(t)[as.integer(s)]
                                      else
                                        s,
                                      stringsAsFactors = FALSE
      )
      transactions <- k
      rm(k, s, t)
    }
  
    c <- lapply(seq_len(length(levels(class)) + 1L), function(k)
      as.integer(sapply(c, "[", k)))
    
    # split into a list of lists (sequences) each 
    # containing a vector of character (itemsets)
    
    x <- lapply(strsplit(sapply(x, "[", 1), split = " -> "), strsplit, " ")
    if (decode)
      x <- lapply(x, lapply, as.integer)
    
    if (!length(x))
      stop("the number of sequences parsed is zero")
    
    x <- as(x, "sequences")
    names(c) <- c("support", levels(class))
    c <- mapply("/", c, c(n, if (!is.null(class)) table(class)), 
                SIMPLIFY = FALSE)
    x@quality <- data.frame(c, check.names = FALSE)
    x@info <- list(nsequences = n)
    
    k <- which(size(x) == 1)
    if (length(k) == length(x@elements)) {
      i <- x@data[,k]@i + 1L
      k[i] <- k
      quality(x@elements) <- x@quality[k,, drop = FALSE]
    } else
      stop("the data is incomplete")
    
    if (!is.null(labels)) {
      k <- as.integer(as.character(x@elements@items@itemInfo[['labels']]))
      itemLabels(x@elements@items) <- as.character(labels[k])
    }
    if (!is.null(transactions)) {
      transactions@itemInfo <- data.frame(labels = 
                                            labels(x), stringsAsFactors = FALSE)
      x@tidLists <- transactions
    }
    validObject(x)
    x
  }