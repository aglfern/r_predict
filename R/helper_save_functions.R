
gera_log <- function(texto,nivel=1) {
   if (nivel <= NIVEL_LOG )
      print(paste(Sys.time(), ":", texto))
}


# Grava resultados 
bfSaveResults <- function(aResult)
{
  
  ## Load the data
  lpList <- aResult$param
  
  funcName <- paste0("FLTWRAP-R-BF-r", num_max_itens_sel, "-k", lpList$k, "-n", 
                     lpList$n,"-f",length(lpList$asearch_fields),"-COR")
  filterResFileName <- paste0(funcName, "-",  format(Sys.time(), "%Y%m%d-%H%M%OS3"),".xlsx")
  
  Result1SList <- aResult$searched_list
  Result1SListDf <- NULL
  for (resRecord in Result1SList)
  {
    Result1SListDf <- rbind (Result1SListDf, 
                             data.frame(campos=toString(resRecord$fields),
                                        mape=resRecord$value, horizonte=resRecord$horiz))
  }
  
  Result1CList <- aResult$closed_list
  Result1CListDf <- NULL
  for (resRecordCl in Result1CList)
  {
    Result1CListDf <- rbind (Result1CListDf, 
                             data.frame(campos=toString(resRecordCl$fields),
                                        mape=resRecordCl$value, horizonte=resRecordCl$horiz))
  }
  
  output_file <- list(parameter = as.data.frame(unlist(lpList)), 
                      best_state = as.data.frame(unlist(aResult$best_state)),
                      closed_list = Result1CListDf,
                      open_list = Result1SListDf,
                      evaluation_frame = as.data.frame(aResult$eval_df),
                      grp_evaluation_frame = as.data.frame(aResult$grp_eval_df))
  
  write_xlsx(output_file,filterResFileName)
  
}

