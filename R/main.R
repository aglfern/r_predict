
####
#
# Sequência de execução dos arquivos
# - load_library.r
# - data_handling.r
# - save_results.r
# - state_helpers.r
# - best_first.r
# - main.r
#
####

############ CONSTANTES
HIGH_ERROR_CONST <- 2^31

NIVEL_LOG <- 1

############ PARÂMETROS

num_max_itens_sel <- 1000

dirName <- 'C:/Mestrado/DadosClaudio'

fileVersion = 2;

paraleliza = TRUE;

remove_outliers <- 'none'

k <- 5
n <- 1

max_sel_fields <- 15
mtype <- c("T","V")

horiz_array <- c(5)
epsilon <- 0.01

MAX_K_EXP_COUNT <- 10




   if ( fileVersion == 1 ) {
      logFileName <- 'log_original_claudio.csv'
      correlFileName <- 'log_original_claudio_correl.csv'
   } else if ( fileVersion == 2 ) {
      logFileName <- 'novo_log_sara.csv'
      correlFileName <- 'novo_log_sara_correl.csv'
   }


   setwd(dirName)


   # chamada principal
   incevtlog_list <- load_incidentevtlog(dirName,logFileName,num_max_itens_sel,fileVersion)


   filter_result_df <- read.csv2(correlFileName)

   filter_result_df_sorted <- filter_result_df[
      order(filter_result_df$cor.val, na.last = T, decreasing = T),]

   #sel_fields <- c("incident_state", 'category')

   all_sel_fields <- as.character(filter_result_df_sorted$source)
   #all_sel_fields <- c("incident_state", "category")
   #all_sel_fields <- c("u_symptom")

   # horizonte com as medias e medianas
   #horiz_array <- c(1, 3, 5, 7, Inf)
   #horiz_array <- c(1, 3, 5, 6, 7, Inf)
   # baseado na distribuicao de eventos dos incidentes
   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
   #1.00    3.00    4.00    5.54    7.00   58.00
   #horiz_array <- c(1, 3, 5, 6, 7, Inf)
   #horiz_array <- c(1, 5, Inf)
   #horiz_array <- c(1,5,Inf)


   getOption("digits")
   options(digits = 4)


   eval_stats_t00 <- NULL

   chosen_fields <- NULL
   curr_field <- NULL
   perr_chosen_fields <- Inf
   perr_curr_fields <- -Inf
   chosen_horiz <- NULL
   search_fields <- all_sel_fields[1:max_sel_fields]
   eval_stats_t00 <- NULL
   eval_stats_t10 <- NULL



   # listas
   open_list <- list() # open vazia
   closed_list <- list() # closed vazia
   expanded_state_list <- list() #lista de expansÃµes
   initial_state <- list(fields = c(), value = HIGH_ERROR_CONST, horiz = Inf) # initial comeca com vazio
   best_state <- initial_state
   k_exp_count <- 0

   v <- put_state(open_list, initial_state) # insere na lista open

   # multicore
   if ( paraleliza == TRUE ) {
      require(doParallel)
      detectCores()
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      getDoParWorkers()
      cl
   }
   eval_stats_t1 <- NULL

   while ((k_exp_count <= MAX_K_EXP_COUNT) && (length(open_list) > 0))
   {
      gera_log("*****************************************************************")
      txtMsg <- paste("Parâmetros: k_exp_count: ", k_exp_count,
                      'Best fields:', toString(best_state$fields), 'Exp list count:', length(expanded_state_list),
                      'Open list count:', length(open_list))
      gera_log(txtMsg)
      # argumento que minimiza
      v <- get_arg_min(open_list)
      v_state <- open_list[[v]]
      remove_state(open_list, v)
      # passa para lista fechada
      put_state(closed_list, v_state)
      # avalia contra o melhor atual
      if(eval_state_fn(v_state) < (eval_state_fn(best_state) - epsilon))
      {
         best_state <- v_state
         k_exp_count <- 0 # reinicia contador de expansÃµes sem resultado
         gera_log("*** best_state changed start ***")
         print_state(best_state)
         gera_log("*** best_state changed end ***")
      }
      # expande
      k_exp_count <- k_exp_count + 1
      if (k_exp_count <= MAX_K_EXP_COUNT)
      {
         expanded_state_list <- expand_state_fn(v_state, search_fields)
         expanded_state_list_tmp <- list()
         for (expanded_state in expanded_state_list)
         {
            if (!(belong_to_list2(open_list, expanded_state) ||
                  belong_to_list2(closed_list, expanded_state)))
            {
               put_state(expanded_state_list_tmp, expanded_state)
            }
         }
         expanded_state_list <- expanded_state_list_tmp

         #ptime <- system.time({
         if  (length(expanded_state_list) > 0)
         {
            lcount <- 1
            expanded_state_list_tmp <- list()
            for (i in 1:length(expanded_state_list))
            {
               for (j in 1:length(horiz_array))
               {
                  lstate_tmp <- list(fields=expanded_state_list[[i]]$fields, horiz = horiz_array[j])
                  expanded_state_list_tmp[[lcount]] <- lstate_tmp
                  lcount <- lcount + 1
               }
            }

            if ( paraleliza == TRUE ) {
               # executa em paralelo e coleta resultados
               gera_log("Execução paralela inicio.")
               eval_stats_tp <- NULL
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
               #print(resultdf)
               gera_log("Execução paralela fim.")

            } else {
               gera_log("Execução sequencial início.")
               eval_stats_tp <- NULL
               for(i in 1 : length(expanded_state_list_tmp))
               {
                  expanded_state <- expanded_state_list_tmp[[i]]
                  eval_stats_tp0 <- build_eval_state_fn(expanded_state, k, n,
                                                        expanded_state$horiz, num_max_itens_sel,
                                                        incevtlog_list, remove_outliers)
                  eval_stats_tp0
                  eval_stats_tp <- rbind(eval_stats_tp, eval_stats_tp0)

               }
               gera_log("Execução sequencial fim.")
            }

            # agrupa resultados
            #eval_stats_tp <- rbind(eval_stats_tp, eval_stats_tp0)
            grp_eval_stats_tp <- summarise_all (
               group_by(eval_stats_tp, sel_fields, k_fold, n_repet, max_itens, rem_outl, horiz, mtype),
               funs(mean, sd), na.rm = TRUE
            )

            # acumula para salvar o final
            eval_stats_t1 <- rbind(eval_stats_t1, eval_stats_tp)
            grp_eval_stats_t1 <- summarise_all (
               group_by(eval_stats_t1, sel_fields, k_fold, n_repet, max_itens, rem_outl, horiz, mtype),
               funs(mean, sd), na.rm = TRUE
            )

            gera_log("Expanded state list inicio.")
            for (exp_state_evalued in expanded_state_list)
            {
               #exp_state_evalued <- expanded_state_list[[2]]
               exp_state_evaluedDf <- subset(grp_eval_stats_tp,
                                             sel_fields == toString(exp_state_evalued$fields) & mtype == 'V')
               #names(exp_state_evaluedDf)
               #names(val_mape_matrix)
               val_mape_matrix <- as.matrix(exp_state_evaluedDf[,
                                                                c("val_mape_pset_mean_mean","val_mape_pset_median_mean",
                                                                  "val_mape_pmset_mean_mean","val_mape_pmset_median_mean",
                                                                  "val_mape_pseq_mean_mean","val_mape_pseq_median_mean")])
               min_value_item <- which(val_mape_matrix == min(val_mape_matrix), arr.ind = TRUE)
               exp_state_evalued$value <-
                  round(min(val_mape_matrix), digits = getOption("digits"))
               exp_state_evalued$horiz <-
                  as.double(exp_state_evaluedDf[min_value_item[1,c('row')], c('horiz')])
               put_state(open_list, exp_state_evalued)
               print_state(exp_state_evalued)
            }
            gera_log("Expanded state list fim.")
         }
      }
      #})[3]
      #print(ptime)
   }

   stopCluster(cl)

   gera_log("*** best_state final start ***")
   print_state(best_state)
   gera_log("*** best_state final end ***")

   parm_list <- list("MAX_K_EXP_COUNT" = MAX_K_EXP_COUNT, asearch_fields = search_fields,
                     ahoriz_array = horiz_array, "k"=k, "n"=n, "remove_outliers" = remove_outliers)
   Result <- list(param=parm_list, best_state=best_state, searched_list=open_list,
                  closed_list=closed_list, eval_df=eval_stats_t1, grp_eval_df = grp_eval_stats_t1)

   bfSaveResults(Result)


   gera_log(paste("Campos selecionados final:", toString(chosen_fields),
                  "Horizonte: ",chosen_horiz,
                  "Perc erro: ",perr_chosen_fields))




