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
