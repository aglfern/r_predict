print_state <- function (aexpanded_state)
{
  sel_fields_str <- toString(aexpanded_state$fields)
  
  gera_log(paste("Estado - Campos:", sel_fields_str, "MAPE:", aexpanded_state$value,
              "Horiz:", aexpanded_state$horiz, sep = "     "))
}

eval_state_fn <- function(astate)
{
  # lstate <- astate
  # Result <- lstate$value
  # return(Result)
  return(astate$value)
}

put_state <- function(astate_list, astate)
{
  lstate_list <- astate_list
  Result = length(lstate_list) + 1
  # print(Result)
  lstate_list[[Result]] <- astate
  eval.parent(substitute(astate_list<-lstate_list))    
  return(Result)  
}

remove_state <- function(astate_list, astate_index)
{
  # pesquisa lista de estados    
  lstate_list <- astate_list[-c(astate_index)]
  eval.parent(substitute(astate_list<-lstate_list))    
}

# Problema de minimização 
# retorna o estado que possui o menor errro
# 
get_arg_min <- function(asrch_state_list)
{
  Result <- 1
  # pesquisa lista de estados o menor erro
  if (length(asrch_state_list) > 1)
  {
    for (i in 2:length(asrch_state_list))
    {
      if (asrch_state_list[[i]]$value < asrch_state_list[[Result]]$value) 
      {
        Result <- i
      }
    }
  }
  return(Result)  
}

