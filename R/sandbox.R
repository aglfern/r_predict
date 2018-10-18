xxlog <- read.table(file.path('C:/Mestrado/DadosClaudio','novo_log_sara.csv'), quote = "\"", sep = ",", dec = ",",
                    header = T, encoding = "utf-8", stringsAsFactors = T, nrow = 100)

xxlog


sum(is.na(xxlog$sys_mod_count))


xxnovo <- load_incidentevtlog('C:/Mestrado/DadosClaudio','novo_log_sara.csv',100)

xxnovo

xxdatas <- as.POSIXct(strptime(xxlog[, "sys_created_at"], "%d/%m/%Y %H:%M"))

xxdatas


print("teste")
print(paste("teste"))

gera_log <- function(texto) {
   print(paste(Sys.time(), ":", texto))
}

gera_log("testes")

trunc(3.323)

runif(10,1,10)

sample(20,10)


length(expanded_state_list_tmp)

foreach(i=1:length(expanded_state_list_tmp),
        .combine=rbind,
        .packages = c('sets','lsr', 'data.table','plyr','dplyr', 'Hmisc', 'MLmetrics')) %do%
{
   #gera_log("teste")
}

expanded_state <- expanded_state_list_tmp[[1]]

build_eval_state_fn(expanded_state, 1, 1, expanded_state$horiz, num_max_itens_sel, incevtlog_list, "none")

eval_stats_tp = foreach (i=1:length(expanded_state_list_tmp), .combine=rbind,
                         .packages = c('sets','lsr', 'data.table','plyr','dplyr', 'Hmisc', 'MLmetrics')) %dopar%
                         {
                            expanded_state <- expanded_state_list_tmp[[i]]
                            eval_stats_tp0 <- build_eval_state_fn(expanded_state, k, n,
                                                                  expanded_state$horiz, num_max_itens_sel,
                                                                  incevtlog_list, remove_outliers)
                            eval_stats_tp0
                            #put_state(open_list, expanded_state_evalued) # insere na lista open
                            #print_state(expanded_state_evalued)
                            #data.frame(i, expanded_state_evalued$value, expanded_state_evalued$horiz)
                         }



gera_log(paste("Estado - Campos:", 10, "MAPE:", 20, "Horiz:", 30, sep = "//t"))


z <- rep("Hi!",3)   #replicate


date_columns["created"]
names(date_columns) <- c("created","opened","updated","resolved","closed")


rm(date_columns)
date_columns <- rep("",5)
date_columns
names(date_columns) <- c("created","opened","updated","resolved","closed")
date_columns
date_columns = c(created = "sys_created_at", opened = "opened_at", updated = "sys_updated_at", resolved = "resolved_at", closed = "closed_at")


dateFormat <- "%d/%m/%Y %H:%M:%S"

dateFormat <- "%d/%m/%Y %H:%M"

for(i in 1:length(date_columns)) {
   xxlog[,date_columns[i]] <- as.POSIXct(strptime(xxlog[,date_columns[i]], dateFormat))
}


date_columns <- list()
date_columns <- rep("",5)
date_columns



xxinst <- build_instances_list_fn2(incevtlog_list$incidentevtlog_closed, 100, k = 5, n = 1, 'none')

write.csv2(xxinst,file="conjuntos-cod-orig.csv")

write.table(xxinst,file="conjuntos-cod-orig2.csv")

NIVEL_LOG <- 2
xxinstnew <- build_instances_list_fn2(incevtlog_list$incidentevtlog_closed, 100, k = 5, n = 1, 'none')

write.table(xxinstnew,file="conjuntos-cod-novo.csv")



write.table(incevtlog_list$incidentevtlog_closed,"tracesList.csv")

xxfolds <- build_datasets_and_folds(incevtlog_list$incidentevtlog_closed,100,5,1)

length(xxfolds[[1]])

write.table(xxfolds,file="folds-v1.csv")



# **************************************


build_datasets_and_folds <- function(tracesList, maxTraces, k, n)
{

   gera_log("Iniciando build_datasets_and_folds",2)

   # fixa item para garantir a reprodutibilidade
   set.seed(1973112111)
   maxTraces <- min(maxTraces, nrow(tracesList))
   lfold_len <- trunc(maxTraces/k)
   sel_instances_list <- NULL

   for (i in 1:n) {
      # Alexandre: substituição do runif pelo sample que já retorna inteiros
      #            problema adicional desse método: pode gerar números repetidos
      # sel_inc <- trunc(runif(num_inc_sel, 1, nrow(lincidentevtlog_closed)))
      # help sample(valor máximo, quantidade a retornar)
      sel_inc <- sample(nrow(tracesList),maxTraces)

      process_instances <- as.character(tracesList[sel_inc,]$number)
      sel_instances_row <- NULL
      for (j in 1:k)
      {
         # treinamento
         #j <-  2
         val_fold_ini_idx <- (j-1) * lfold_len + 1
         if (j == k)
            val_fold_end_idx <- length(process_instances)
         else
            val_fold_end_idx <- (j-1) * lfold_len  + lfold_len

         gera_log(paste("val_fold_ini_idx: ", val_fold_ini_idx, " val_fold_end_idx: ", val_fold_end_idx),2)

         proc_instances_val <- list(process_instances[c(val_fold_ini_idx:val_fold_end_idx)])
         proc_instances_tr <- list(process_instances[-c(val_fold_ini_idx:val_fold_end_idx)])
         sel_instances_row <- c(sel_instances_row, proc_instances_tr, proc_instances_val)
      }
      sel_instances_list <- c(sel_instances_list, list(sel_instances_row))
   }
   gera_log("Finalizando build_datasets_and_folds",2)

   return(sel_instances_list)
}
