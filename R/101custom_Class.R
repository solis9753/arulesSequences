

setClass("customTidLists", 
         contains =  "tidLists")

setAs("ngCMatrix", "customTidLists", 
      function(from){
      new("customTidLists", 
          data            = from,
          itemInfo        = data.frame(labels = NULL, 
                                       stringsAsFactors = FALSE),
          transactionInfo = data.frame(labels = (unique(from@i) +1), stringsAsFactors = FALSE)
      )
        }
      )
