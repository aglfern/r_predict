# Limpa ambiente
#rm(list=ls())






# state function

NIL_STATE <- -1


state_fn2 <- function(astate_list, asigma, amode = "sequence")
{
  # pesquisa lista de estados    
  lstate_list <- astate_list
  # print(asigma)
  # print(lstate_list)
  Result <- match(asigma, lstate_list)
  if (is.na(Result))
  {
    Result = length(lstate_list) + 1
    # print(Result)
    lstate_list[Result] <- asigma
    eval.parent(substitute(astate_list<-lstate_list))    
  }
  return(Result)  
}


find_state_fn2 <- function(astate_list, asigma, amode = "sequence")
{
  # pesquisa lista de estados    
  Result <- match(asigma, astate_list)
  if (is.na(Result))
  {
    Result = NIL_STATE
  }
  return(Result)  
}


# função de construção dos MTA
# recebe o trace, lista de instancias
build_mta_fn <- function(asel_traces_lcl, process_instances, horiz = Inf, 
                         lcl_activity_flds = c("incident_state"), 
                         lcl_timestamp_fld = c("updated_at"))
{

   gera_log("Iniciando build_mta_fn",2)
   
  time0<-as.numeric(Sys.time())
  
  #sel_traces_lcl <- sel_traces_
  #horiz = 1
  sel_traces_lcl <- asel_traces_lcl
  # process_instances <- process_instances_
  # process_instances_states <- process_instances_states_trn
  process_instances_states <-list(
    number=process_instances, 
    set_states=list(), 
    mset_states=list(),
    seq_states=list() 
  )
  
  # horizonte
  #horiz <- Inf
  #horiz <- 5
  # listas com os estados de cada modelo
  seq_state_list <- list()
  set_state_list <- list()
  mset_state_list <- list()
  
  
  for(i in 1:length(process_instances))
  {
    # busca trace da instancia
    #i <- 1
    trace <- as.data.frame(sel_traces_lcl[sel_traces_lcl$number == process_instances[i],])
    trace$seq_state_id <- 0
    trace$set_state_id <- 0
    trace$mset_state_id <- 0
    trace$sojourn_set_stc <- 0
    trace$sojourn_mset_stc <- 0
    trace$sojourn_seq_stc <- 0
    
    #levels( incidentevtlog[,"close_code"])
    # tem que normalizar o close_code ()
    # seleciona campos para montagem do modelo 
    #activity_fields <- data.frame(trace[,c("incident_state","category")])
    
    # label utilizado na função de eventos (campos do modelo)
    #activity_fields <- data.frame(trace[,c("incident_state", "category")])
    #activity_fields <- do.call(paste, as.data.frame(trace[,c("incident_state")]))
    activity_fields <- do.call(paste, as.data.frame(trace[,lcl_activity_flds]))
    #activity_fields <- do.call(paste, as.data.frame(trace[,c("incident_state", "category")]))
    # tempo em que ocorreu o evento
    timestamp_field <-  as.vector(trace[, lcl_timestamp_fld])
    #timestamp_field <-  as.vector(trace[,c("sys_updated_on")])
    # listas com os modelos de abstração usados para representação dos eventos
    seq_list <- list()
    set_list <- list()
    mset_list <- list()
    for (j in 1:nrow(trace))
    {
      # calculo do horizonte
      horiz_index <- max(j - horiz + 1, 1)
      
      inc_seq <- as.character(activity_fields[horiz_index:j])
      
      # gera abstrações 
      # sequence
      #print(inc_seq)
      
      #seq <- activity_fields[,1][1:j]
      #cat(j, " - " , seq, "\n")
      #seq_list[[j]] <- inc_seq
      seq_list[[j]]  <- toString(inc_seq)
      
      # set
      #set <- as.character(as.set(seq))
      inc_set <- as.set(inc_seq)
      #cat(j, " - " , set, "\n")
      #set_list[[j]]  <- inc_set
      set_list[[j]]  <- toString(inc_set)
      
      # multi-set
      #mset_list[j] <- list(as.gset(seq))
      inc_gset <- as.gset(inc_seq)
      
      inc_gset_str <- toString(
          rbind(gset_support(inc_gset), gset_memberships(inc_gset))
      )
      
      #mset_list[[j]]  <- inc_gset
      mset_list[[j]]  <- inc_gset_str
      
      # chama função para avaliar os estados e gerar o estado atual
      # argumentos: lista de estados atual
      #             trace dos eventos anteriores + evento atual
      # retorna:    novo estado
      # modelo de abstração sequencia
      trace[j,"set_state_id"] <- state_fn2(set_state_list, set_list[[j]], "set")
      
      if (horiz==1) # horizonte 1, todos iguais 
      {
        # idem com modelo multiset (ainda não concluido)
        trace[j,"mset_state_id"] <- trace[j,"set_state_id"]
        # modelo de abstração sequencia
        trace[j,"seq_state_id"] <- trace[j,"set_state_id"]
      }
      else
      {
        # idem com modelo multiset
        trace[j,"mset_state_id"] <- state_fn2(mset_state_list, mset_list[[j]] , "mset")
        # modelo de abstração sequencia
        trace[j,"seq_state_id"] <- state_fn2(seq_state_list, seq_list[[j]], "sequence")
      }
      
      if (j==1) # primeiro registro guarda elapsed quando entrou no estado
      {
        curr_sojourn_set_stc <- trace[j,"elapsed_stc"]
        curr_sojourn_mset_stc <- trace[j,"elapsed_stc"]
        curr_sojourn_seq_stc <- trace[j,"elapsed_stc"]
        curr_sojourn_set_state <- trace[j,"set_state_id"]
        curr_sojourn_mset_state <- trace[j,"mset_state_id"]
        curr_sojourn_seq_state <- trace[j,"seq_state_id"]
      }
      else
      { # set
        if (curr_sojourn_set_state == trace[j,"set_state_id"])
          trace[j,"sojourn_set_stc"] <- trace[j,"elapsed_stc"] - curr_sojourn_set_stc
        else
        {
          curr_sojourn_set_stc <- trace[j,"elapsed_stc"]
          curr_sojourn_set_state <- trace[j,"set_state_id"]
        }
        # mset
        if (curr_sojourn_mset_state == trace[j,"mset_state_id"])
          trace[j,"sojourn_mset_stc"] <- trace[j,"elapsed_stc"] - curr_sojourn_mset_stc
        else
        {
          curr_sojourn_mset_stc <- trace[j,"elapsed_stc"]
          curr_sojourn_mset_state <- trace[j,"mset_state_id"]
        }
        #seq
        if (curr_sojourn_seq_state == trace[j,"seq_state_id"])
          trace[j,"sojourn_seq_stc"] <- trace[j,"elapsed_stc"] - curr_sojourn_seq_stc
        else
        {
          curr_sojourn_seq_stc <- trace[j,"elapsed_stc"]
          curr_sojourn_seq_state <- trace[j,"seq_state_id"]
        }
      }
      
    } # fim j
    # armazena resultado das transições de estado para instancia atual
    # modelo de abstração sequencia
    process_instances_states$seq_states[i] <- list(trace[,"seq_state_id"])
    
    # modelo de abstração set
    process_instances_states$set_states[i] <- list(trace[,"set_state_id"])
    
    # modelo de abstração mset
    process_instances_states$mset_states[i] <- list(trace[,"mset_state_id"])
    
    # guardo a estado no evento
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$seq_state_id <- trace$seq_state_id
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$set_state_id <- trace$set_state_id
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$mset_state_id <- trace$mset_state_id
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$sojourn_set_stc <- trace$sojourn_set_stc
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$sojourn_mset_stc <- trace$sojourn_mset_stc
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$sojourn_seq_stc <- trace$sojourn_seq_stc
    
    #print(trace)
  } # fim i
  #i <- 4
  # tempos de execução
  time1<-as.numeric(Sys.time())
  timetot.2var <- time1-time0
  #print(timetot.2var/60)
  #print(sel_traces_lcl)
  
  # retorna resultado 
  eval.parent(substitute(asel_traces_lcl<-sel_traces_lcl))
  mta_model <- list(process_instances_states=process_instances_states, 
                    seq_state_list=seq_state_list, 
                    set_state_list=set_state_list,
                    mset_state_list=mset_state_list, 
                    horiz=horiz, 
                    lcl_activity_flds=lcl_activity_flds, 
                    lcl_timestamp_fld=lcl_timestamp_fld
                  )
  
  gera_log("Finalizando build_mta_fn",2)
  
  return(mta_model)
  
}


# função de construção dos MTA
# recebe o trace, lista de instancias
predict_mta_fn <- function(asel_traces_lcl, process_instances, mta_in)
{

   gera_log("Iniciando predict_mta_fn",2)
   
  time0<-as.numeric(Sys.time())
  
  sel_traces_lcl <- asel_traces_lcl
  # process_instances <- process_instances_trn
  # process_instances_states <- process_instances_states_trn
  process_instances_states <-list(
    number=process_instances, 
    set_states=list(), 
    mset_states=list(),
    seq_states=list() 
  )
  
  # horizonte
  #horiz <- Inf
  #horiz <- 5
  # listas com os estados de cada modelo
  seq_state_list <- mta_in$seq_state_list
  set_state_list <- mta_in$set_state_list
  mset_state_list <- mta_in$mset_state_list
  
  
  for(i in 1:length(process_instances))
  {
    # busca trace da instancia
    trace <- as.data.frame(sel_traces_lcl[sel_traces_lcl$number == process_instances[i],])
    trace$seq_state_id <- 0
    trace$set_state_id <- 0
    trace$mset_state_id <- 0
    trace$sojourn_set_stc <- 0
    trace$sojourn_mset_stc <- 0
    trace$sojourn_seq_stc <- 0
    
    #levels( incidentevtlog[,"close_code"])
    # tem que normalizar o close_code ()
    # seleciona campos para montagem do modelo 
    #activity_fields <- data.frame(trace[,c("incident_state","category")])
    
    # label utilizado na função de eventos (campos do modelo)
    #activity_fields <- data.frame(trace[,c("incident_state", "category")])
    #activity_fields <- do.call(paste, as.data.frame(trace[,c("incident_state")]))
    activity_fields <- do.call(paste, as.data.frame(trace[,mta_in$lcl_activity_flds]))
    #activity_fields <- do.call(paste, as.data.frame(trace[,c("incident_state", "category")]))
    # tempo em que ocorreu o evento
    timestamp_field <-  as.vector(trace[, mta_in$lcl_timestamp_fld])
    #timestamp_field <-  as.vector(trace[,c("sys_updated_on")])
    # listas com os modelos de abstração usados para representação dos eventos
    seq_list <- list()
    set_list <- list()
    mset_list <- list()
    for (j in 1:nrow(trace))
    {
      # calculo do horizonte
      horiz_index <- max(j - mta_in$horiz + 1, 1)
      
      inc_seq <- as.character(activity_fields[horiz_index:j])
      
      # gera abstrações 
      # sequence
      #print(inc_seq)
      
      seq_list[[j]]  <- toString(inc_seq)
      
      # set
      #set <- as.character(as.set(seq))
      inc_set <- as.set(inc_seq)
      #cat(j, " - " , set, "\n")
      #set_list[[j]]  <- inc_set
      set_list[[j]]  <- toString(inc_set)
      
      # multi-set
      #mset_list[j] <- list(as.gset(seq))
      inc_gset <- as.gset(inc_seq)
      
      inc_gset_str <- toString(
        rbind(gset_support(inc_gset), gset_memberships(inc_gset))
      )
      
      #mset_list[[j]]  <- inc_gset
      mset_list[[j]]  <- inc_gset_str      
      
      # chama função para avaliar os estados e gerar o estado atual
      # argumentos: lista de estados atual
      #             trace dos eventos anteriores + evento atual
      # retorna:    novo estado
      
      # idem ao anterior com modelo de abstração set
      trace[j,"set_state_id"] <- find_state_fn2(set_state_list, set_list[[j]], "set")
      
      if (mta_in$horiz==1) # horizonte 1, todos iguais 
      {
        # idem com modelo multiset (ainda não concluido)
        trace[j,"mset_state_id"] <- trace[j,"set_state_id"]
        # modelo de abstração sequencia
        trace[j,"seq_state_id"] <- trace[j,"set_state_id"]
      }
      else
      {
        # idem com modelo multiset
        trace[j,"mset_state_id"] <- find_state_fn2(mset_state_list, mset_list[[j]] , "mset")
        # modelo de abstração sequencia
        trace[j,"seq_state_id"] <- find_state_fn2(seq_state_list, seq_list[[j]], "sequence")
      }
      
      if (j==1) # primeiro registro guarda elapsed quando entrou no estado
      {
        curr_sojourn_set_stc <- trace[j,"elapsed_stc"]
        curr_sojourn_mset_stc <- trace[j,"elapsed_stc"]
        curr_sojourn_seq_stc <- trace[j,"elapsed_stc"]
        curr_sojourn_set_state <- trace[j,"set_state_id"]
        curr_sojourn_mset_state <- trace[j,"mset_state_id"]
        curr_sojourn_seq_state <- trace[j,"seq_state_id"]
      }
      else
      { # set
        if (curr_sojourn_set_state == trace[j,"set_state_id"])
          trace[j,"sojourn_set_stc"] <- trace[j,"elapsed_stc"] - curr_sojourn_set_stc
        else
        {
          curr_sojourn_set_stc <- trace[j,"elapsed_stc"]
          curr_sojourn_set_state <- trace[j,"set_state_id"]
        }
        # mset
        if (curr_sojourn_mset_state == trace[j,"mset_state_id"])
          trace[j,"sojourn_mset_stc"] <- trace[j,"elapsed_stc"] - curr_sojourn_mset_stc
        else
        {
          curr_sojourn_mset_stc <- trace[j,"elapsed_stc"]
          curr_sojourn_mset_state <- trace[j,"mset_state_id"]
        }
        #seq
        if (curr_sojourn_seq_state == trace[j,"seq_state_id"])
          trace[j,"sojourn_seq_stc"] <- trace[j,"elapsed_stc"] - curr_sojourn_seq_stc
        else
        {
          curr_sojourn_seq_stc <- trace[j,"elapsed_stc"]
          curr_sojourn_seq_state <- trace[j,"seq_state_id"]
        }
      }
      
    } # fim j
    # armazena resultado das transições de estado para instancia atual
    # modelo de abstração sequencia
    process_instances_states$seq_states[i] <- list(trace[,"seq_state_id"])
    
    # modelo de abstração set
    process_instances_states$set_states[i] <- list(trace[,"set_state_id"])
    
    # modelo de abstração mset
    process_instances_states$mset_states[i] <- list(trace[,"mset_state_id"])
    
    # guardo a estado no evento
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$seq_state_id <- trace$seq_state_id
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$set_state_id <- trace$set_state_id
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$mset_state_id <- trace$mset_state_id
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$sojourn_set_stc <- trace$sojourn_set_stc
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$sojourn_mset_stc <- trace$sojourn_mset_stc
    sel_traces_lcl[sel_traces_lcl$number == process_instances[i],]$sojourn_seq_stc <- trace$sojourn_seq_stc
    
  } # fim i
  #i <- 4
  # tempos de execução
  time1<-as.numeric(Sys.time())
  timetot.2var <- time1-time0
  #print(timetot.2var/60)
  
  # retorna resultado 
  eval.parent(substitute(asel_traces_lcl<-sel_traces_lcl))
  mta_model <- list(process_instances_states=process_instances_states, 
                    seq_state_list=mta_in$seq_state_list, 
                    set_state_list=mta_in$set_state_list,
                    mset_state_list=mta_in$mset_state_list, 
                    horiz=mta_in$horiz, 
                    lcl_activity_flds=mta_in$lcl_activity_flds, 
                    lcl_timestamp_fld=mta_in$lcl_timestamp_fld
  )
  
  gera_log("Finalizando predict_mta_fn",2)
  
  return(mta_model)
}

gen_summary_pred_fn <- function(data=NULL, groupvars=NULL, measurevar,  na.rm=TRUE,
                                conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = ceil(mean   (xx[[col]], na.rm=na.rm)),
                     sd   = ceil(sd     (xx[[col]], na.rm=na.rm)),
                     median   = ceil(median (xx[[col]], na.rm=na.rm)),
                     min   = ceil(min     (xx[[col]], na.rm=na.rm)),
                     max   = ceil(max     (xx[[col]], na.rm=na.rm))
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)

  datac$ci <- datac$se * ciMult
  
  # registro para valores não encontrados #non_fitting
  datac <- rbind(datac, 
     c(NIL_STATE, sum(datac$N),  mean(datac$mean),  sd(datac$mean),
       median(datac$mean), min(datac$mean), max(datac$mean))
  )
  
  return(datac)
}

eval_model_gen_fn <- function(lsel_traces_list)
{
  summary_pred_stats <- NULL
  result <- NULL
  for (sel_trace_ in lsel_traces_list)
  {
    # anotaçoes por evento
    # sel_trace_ <- sel_traces_trn
    # sel_trace_ <- sel_traces_tst
    # sel_trace_ <- sel_traces_val
    
    # workaround para erro estimativa negativa
    # incidentevtlog_anot <- as.data.frame(
    #   sel_trace_[sel_trace_$remaining_stc > 0,
    #                c("number", "sys_updated_on", "incident_state", "seq_state_id","set_state_id", "mset_state_id",
    #                  "elapsed_stc", "remaining_stc")]
    # )
    
    incidentevtlog_anot <- as.data.frame(
      sel_trace_[, c("number", "updated_at", "incident_state", "seq_state_id","set_state_id", "mset_state_id",
                     "sojourn_set_stc","sojourn_mset_stc","sojourn_seq_stc","elapsed_stc", "remaining_stc")]
    )
    
    # teste estatistica convertida
    #incidentevtlog_anot$remaining_stc <- incidentevtlog_anot$remaining_stc 
    incidentevtlog_anot$remaining_stc <- incidentevtlog_anot$remaining_stc 
    
        # gerar as contagens e medias por estado
    # num_secs <- 1 * 60 * 60 # em horas
    # num_secs <- 1 # em segundos
    # inc.outlier <- T
    
    # Gera informações de predição por estado
    # TODO: Avaliar o calculo retirando os valores de outlier 1.5 * IQR
    # predição no primeiro conjunto treinamento - demais validação
    if (is.null(summary_pred_stats)) 
    {
      # filtrar os valores que são estados finais pois distorcem a media
      incidentevtlog_anot_st <- incidentevtlog_anot[incidentevtlog_anot$remaining_stc > 0,]
      
      summary_set <- gen_summary_pred_fn(incidentevtlog_anot_st, 'set_state_id','remaining_stc')
      summary_mset <- gen_summary_pred_fn(incidentevtlog_anot_st, 'mset_state_id','remaining_stc')
      summary_seq <- gen_summary_pred_fn(incidentevtlog_anot_st, 'seq_state_id','remaining_stc')

      summary_sj_set <- gen_summary_pred_fn(incidentevtlog_anot_st, 'set_state_id','sojourn_set_stc')
      summary_sj_mset <- gen_summary_pred_fn(incidentevtlog_anot_st, 'mset_state_id','sojourn_mset_stc')
      summary_sj_seq <- gen_summary_pred_fn(incidentevtlog_anot_st, 'seq_state_id','sojourn_seq_stc')
      
      #armazena totais
      summary_pred_stats <- list(summary_set, summary_mset, summary_seq,
                                 summary_sj_set, summary_sj_mset, summary_sj_seq)
    }
    
    # atualiza predited values media e mediana
    # set
    incidentevtlog_anot$remaining_stc_pset_mean <- 
      summary_set$mean[match(incidentevtlog_anot$set_state_id, summary_set$set_state_id)] + 
      summary_sj_set$mean[match(incidentevtlog_anot$set_state_id, summary_sj_set$set_state_id)] - 
      incidentevtlog_anot$sojourn_set_stc
    incidentevtlog_anot$remaining_stc_pset_median <- 
      summary_set$median[match(incidentevtlog_anot$set_state_id, summary_set$set_state_id)] +
      summary_sj_set$median[match(incidentevtlog_anot$set_state_id, summary_sj_set$set_state_id)] - 
      incidentevtlog_anot$sojourn_set_stc
    # multi set
    incidentevtlog_anot$remaining_stc_pmset_mean <- 
      summary_mset$mean[match(incidentevtlog_anot$mset_state_id, summary_mset$mset_state_id)] +
      summary_sj_mset$mean[match(incidentevtlog_anot$mset_state_id, summary_sj_mset$mset_state_id)] - 
      incidentevtlog_anot$sojourn_mset_stc
    incidentevtlog_anot$remaining_stc_pmset_median <- 
      summary_mset$median[match(incidentevtlog_anot$mset_state_id, summary_mset$mset_state_id)] +
      summary_sj_mset$median[match(incidentevtlog_anot$mset_state_id, summary_sj_mset$mset_state_id)] - 
      incidentevtlog_anot$sojourn_mset_stc
    # sequence
    incidentevtlog_anot$remaining_stc_pseq_mean <- 
      summary_seq$mean[match(incidentevtlog_anot$seq_state_id, summary_seq$seq_state_id)] +
      summary_sj_seq$mean[match(incidentevtlog_anot$seq_state_id, summary_sj_seq$seq_state_id)] - 
      incidentevtlog_anot$sojourn_seq_stc
    incidentevtlog_anot$remaining_stc_pseq_median <- 
      summary_seq$median[match(incidentevtlog_anot$seq_state_id, summary_seq$seq_state_id)] +
      summary_sj_seq$median[match(incidentevtlog_anot$seq_state_id, summary_sj_seq$seq_state_id)] - 
      incidentevtlog_anot$sojourn_seq_stc
    
    # remove valorers sem match para calculo erro
    incidentevtlog_anot_err <- na.omit(incidentevtlog_anot)
    # remove valores dos estados finais Target = 0 que distorcem a média
    # valores do ultimo estado serão sempre precisos
    incidentevtlog_anot_err <- incidentevtlog_anot_err[incidentevtlog_anot_err$remaining_stc > 0,]
    
    # calculo erro  MAPE e RMSPE todos os registros 
    
    #MAPE(y_pred, y_true)
    mape_val <- c(
      MAPE(incidentevtlog_anot_err$remaining_stc_pset_mean, incidentevtlog_anot_err$remaining_stc),
      MAPE(incidentevtlog_anot_err$remaining_stc_pset_median, incidentevtlog_anot_err$remaining_stc),
      MAPE(incidentevtlog_anot_err$remaining_stc_pmset_mean, incidentevtlog_anot_err$remaining_stc),
      MAPE(incidentevtlog_anot_err$remaining_stc_pmset_median, incidentevtlog_anot_err$remaining_stc),
      MAPE(incidentevtlog_anot_err$remaining_stc_pseq_mean, incidentevtlog_anot_err$remaining_stc),
      MAPE(incidentevtlog_anot_err$remaining_stc_pseq_median, incidentevtlog_anot_err$remaining_stc)
    )
    names(mape_val) <- c(
      "val_mape_pset_mean","val_mape_pset_median",
      "val_mape_pmset_mean","val_mape_pmset_median",
      "val_mape_pseq_mean","val_mape_pseq_median"
    )
    mape_val
    
    #RMSPE(y_pred, y_true)
    rmspe_val <- c(
      RMSPE(incidentevtlog_anot_err$remaining_stc_pset_mean, incidentevtlog_anot_err$remaining_stc),
      RMSPE(incidentevtlog_anot_err$remaining_stc_pset_median, incidentevtlog_anot_err$remaining_stc),
      RMSPE(incidentevtlog_anot_err$remaining_stc_pmset_mean, incidentevtlog_anot_err$remaining_stc),
      RMSPE(incidentevtlog_anot_err$remaining_stc_pmset_median, incidentevtlog_anot_err$remaining_stc),
      RMSPE(incidentevtlog_anot_err$remaining_stc_pseq_mean, incidentevtlog_anot_err$remaining_stc),
      RMSPE(incidentevtlog_anot_err$remaining_stc_pseq_median, incidentevtlog_anot_err$remaining_stc)
    )
    names(rmspe_val) <- c(
      "val_rmspe_pset_mean","val_rmspe_pset_median",
      "val_rmspe_pmset_mean","val_rmspe_pmset_median",
      "val_rmspe_pseq_mean","val_rmspe_pseq_median"
    )
    rmspe_val
    
    #non fitting
    non_fit_arr <- c(
      nrow(sel_trace_),
      nrow(incidentevtlog_anot),
      nrow(incidentevtlog_anot[incidentevtlog_anot$set_state_id == NIL_STATE,]),
      nrow(incidentevtlog_anot[incidentevtlog_anot$mset_state_id == NIL_STATE,]),
      nrow(incidentevtlog_anot[incidentevtlog_anot$seq_state_id == NIL_STATE,]),
      length(unique(incidentevtlog_anot$set_state_id)),
      length(unique(incidentevtlog_anot$mset_state_id)),
      length(unique(incidentevtlog_anot$seq_state_id))
    )
    names(non_fit_arr) <- c("num_evt_tot","num_evt_ok","num_evt_nf_set",
                            "num_evt_nf_mset","num_evt_nf_seq", "num_set_states",
                            "num_mset_states", "num_seq_states")
    #print(non_fit_arr)
    non_fit_per_arr <- c(
      non_fit_arr[c("num_evt_nf_set")] / non_fit_arr[c("num_evt_ok")],
      non_fit_arr[c("num_evt_nf_mset")] / non_fit_arr[c("num_evt_ok")],
      non_fit_arr[c("num_evt_nf_seq")] / non_fit_arr[c("num_evt_ok")]
    )
    names(non_fit_per_arr) <- c("perr_nf_set","perr_nf_mset","perr_nf_seq")   
    
    non_fit_per_arr <- non_fit_per_arr * 100
    
    # perr_tot_arr <- c(
    #   min(mape_val[c("val_mape_pset_mean")], mape_val[c("val_mape_pset_median")]) + non_fit_per_arr[c("perr_nf_set")],
    #   min(mape_val[c("val_mape_pmset_mean")], mape_val[c("val_mape_pmset_median")]) + non_fit_per_arr[c("perr_nf_mset")],
    #   min(mape_val[c("val_mape_pseq_mean")], mape_val[c("val_mape_pseq_median")]) + non_fit_per_arr[c("perr_nf_seq")]
    # )
    # names(perr_tot_arr) <- c(
    #   "perr_tot_set","perr_tot_mset","perr_tot_seq"
    # )
    # perr_tot_arr

    # retorna o menor erro - media ou mediana
    perr_tot_arr <- c(
      min(mape_val[c("val_mape_pset_mean")], mape_val[c("val_mape_pset_median")]),
      min(mape_val[c("val_mape_pmset_mean")], mape_val[c("val_mape_pmset_median")]),
      min(mape_val[c("val_mape_pseq_mean")], mape_val[c("val_mape_pseq_median")])
    )
    names(perr_tot_arr) <- c(
      "perr_tot_set","perr_tot_mset","perr_tot_seq"
    )
    perr_tot_arr
    
    # filtro para eventos com fit
    incidentevtlog_anot_err_set1 <- incidentevtlog_anot_err[incidentevtlog_anot_err$set_state_id != NIL_STATE,]
    incidentevtlog_anot_err_mset1 <- incidentevtlog_anot_err[incidentevtlog_anot_err$mset_state_id != NIL_STATE,]
    incidentevtlog_anot_err_seq1 <- incidentevtlog_anot_err[incidentevtlog_anot_err$seq_state_id != NIL_STATE,]
    #MAPE(y_pred, y_true)
    mape_val1 <- c(
      MAPE(incidentevtlog_anot_err_set1$remaining_stc_pset_mean, incidentevtlog_anot_err_set1$remaining_stc),
      MAPE(incidentevtlog_anot_err_set1$remaining_stc_pset_median, incidentevtlog_anot_err_set1$remaining_stc),
      MAPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_mean, incidentevtlog_anot_err_mset1$remaining_stc),
      MAPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_median, incidentevtlog_anot_err_mset1$remaining_stc),
      MAPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_mean, incidentevtlog_anot_err_seq1$remaining_stc),
      MAPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_median, incidentevtlog_anot_err_seq1$remaining_stc)
    )
    names(mape_val1) <- c(
      "val_mape_pset_mean1","val_mape_pset_median1",
      "val_mape_pmset_mean1","val_mape_pmset_median1",
      "val_mape_pseq_mean1","val_mape_pseq_median1"
    )
    mape_val1
    
    rmspe_val1 <- c(
      RMSPE(incidentevtlog_anot_err_set1$remaining_stc_pset_mean, incidentevtlog_anot_err_set1$remaining_stc),
      RMSPE(incidentevtlog_anot_err_set1$remaining_stc_pset_median, incidentevtlog_anot_err_set1$remaining_stc),
      RMSPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_mean, incidentevtlog_anot_err_mset1$remaining_stc),
      RMSPE(incidentevtlog_anot_err_mset1$remaining_stc_pmset_median, incidentevtlog_anot_err_mset1$remaining_stc),
      RMSPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_mean, incidentevtlog_anot_err_seq1$remaining_stc),
      RMSPE(incidentevtlog_anot_err_seq1$remaining_stc_pseq_median, incidentevtlog_anot_err_seq1$remaining_stc)
    )
    names(rmspe_val1) <- c(
      "val_rmspe_pset_mean1","val_rmspe_pset_median1",
      "val_rmspe_pmset_mean1","val_rmspe_pmset_median1",
      "val_rmspe_pseq_mean1","val_rmspe_pseq_median1"
    )
    rmspe_val1
    
    #non_fit_arr
    result <- rbind(
      result, 
      c(mape_val, rmspe_val, non_fit_arr, non_fit_per_arr, perr_tot_arr, 
        mape_val1, rmspe_val1)
    )
  }
  return(result)
}


get_outlier_limits <- function(DSColumn, conv_factor = (60*60*24))
{
  
  DSSummary <- summary(as.numeric(DSColumn)/conv_factor)
  print(DSSummary)
  
  DSIQR <- DSSummary[c("3rd Qu.")] - DSSummary[c("1st Qu.")]
  DSOutliersLimits <- 
    c(DSSummary[c("1st Qu.")] - DSIQR * 1.5,
      DSSummary[c("3rd Qu.")] + DSIQR * 1.5,
      DSSummary[c("1st Qu.")] - DSIQR * 3,
      DSSummary[c("3rd Qu.")] + DSIQR * 3)
  names(DSOutliersLimits) <- c("inner_fence_min", "inner_fence_max", 
                               "outer_fence_min", "outer_fence_max")
  return (DSOutliersLimits)
}


build_instances_list_fn2 <- function(
  aincidentevtlog_closed,
  num_max_itens_sel = 100, k = 2, n = 5, rem_outlier = 'none'
)
{
   
   gera_log("Iniciando build_instances_list_fn2",2)
   
  # fixa item para garantir a reprodutibilidade
  set.seed(1973112111)
  
  # seleção numero de itens
  # num_max_itens_sel <- 1000
  # k <- 2
  # n <- 5
  #k <- 3
  #num_max_itens_sel <- 120
  aincidentevtlog_closed <- incevtlog_list$incidentevtlog_closed
  num_inc_sel <- min(num_max_itens_sel, nrow(aincidentevtlog_closed))
  lfold_len <- trunc(num_inc_sel/k)
  lincidentevtlog_closed <- aincidentevtlog_closed
  sel_instances_list <- NULL
  for (i in 1:n)
  {
    #i <- 1
    sel_inc <- trunc(runif(num_inc_sel, 1, nrow(lincidentevtlog_closed)))
    # calcula outliers
    # if (rem_outlier != 'none')
    # {
    #   # calculate outliers
    #   
    #   #Alexandre: essa parte de remo��o de outliers n�o funciona mais porque o
    #   # atributo calendar_stc foi removido - teria que entender essa f�rmula e mudar.
    #   
    #   lds_tr_out <- lincidentevtlog_closed[sel_inc,]
    #   ds_tr_out_lim <- get_outlier_limits(lds_tr_out$calendar_stc, 1)
    #   lds_tr_out$calendar_stc <- as.numeric(lds_tr_out$calendar_stc)
    #   print(ds_tr_out_lim)
    #   if (rem_outlier == 'inner')
    #   {        
    #     max_out_lim <- ds_tr_out_lim[c("inner_fence_max")]
    #   }
    #   else 
    #   {
    #     max_out_lim <- ds_tr_out_lim[c("outer_fence_max")]
    #   }
    #   process_instances_out <- subset(lds_tr_out, calendar_stc > max_out_lim)
    #   print(paste("# outliers records process_instances_out: ", length(process_instances_out)))
    #   
    #   process_instances_out <- as.character(process_instances_out$number)
    #   print(paste("Remover: ", rem_outlier, "Len process_instances_out column: ", length(process_instances_out)))
    # }
    
    
    process_instances <- as.character(lincidentevtlog_closed[sel_inc,]$number)
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
      proc_instances_val <- list(process_instances[c(val_fold_ini_idx:val_fold_end_idx)])
      proc_instances_tr <- list(process_instances[-c(val_fold_ini_idx:val_fold_end_idx)])
      if (rem_outlier != 'none')
      {
        proc_instances_tr <- as.character(unlist(proc_instances_tr))
        print(paste("Len character instances trein sel: ", length(proc_instances_tr)))
        #head(proc_instances_tr)
        #head(process_instances_out)
        #var1 [-which(var1 %in% var2)]
        proc_instances_tr <- proc_instances_tr[-which(proc_instances_tr %in% process_instances_out)]
        
        print(paste("Len character instances trein sel no outlier: ", length(proc_instances_tr)))
        
        proc_instances_tr <- list(proc_instances_tr)
      }
      sel_instances_row <- c(sel_instances_row, proc_instances_tr, proc_instances_val)
      # remove itens usados
      #lincidentevtlog_closed <- lincidentevtlog_closed[-sel_inc, ]
    }
    sel_instances_list <- c(sel_instances_list, list(sel_instances_row))
  }
  gera_log("Finalizando build_instances_list_fn2",2)
  
  return(sel_instances_list)
}



build_and_run_mta_fn2 <- function(
  aincevtlog_list,
  ahorizonte = Inf,
  anum_max_itens_sel,
  asel_fields = c("incident_state"),
  k = 2,
  n = 1,
  rem_outlier = "none" # values: none, inner, outer
)
{
   
   gera_log("Iniciando build_and_run_mta_fn2",2)
   
  #aincevtlog_list <- incevtlog_list
  lincidentevtlog_closed <- aincevtlog_list$incidentevtlog_closed
  lincidentevtlog <- aincevtlog_list$incidentevtlog
  lhorizonte <- ahorizonte
  lnum_max_itens_sel <- anum_max_itens_sel
  lsel_fields <- asel_fields
  
  sel_traces_list <- NULL
  mta_model_list <- NULL
  eval_stats_arr <- NULL
  
  sel_instances_list <- build_instances_list_fn2(lincidentevtlog_closed, 
                                       lnum_max_itens_sel, k, n, rem_outlier)
  
  for (i in 1:length(sel_instances_list)) # n
  {
    sel_instances_row <- sel_instances_list[[i]]
    sel_traces_row <- NULL
    mta_model_row <- NULL
    mta_model_trn <- NULL
    for (j in 1:length(sel_instances_row)) # k * 2
    {
      process_instances_ <- sel_instances_row[[j]]
      sel_traces_ <- as.data.frame(lincidentevtlog[lincidentevtlog$number %in% process_instances_,])
      # chama função de construção 
      if (is.null(mta_model_trn))
      {
        mta_model_ <- build_mta_fn(sel_traces_, process_instances_, lhorizonte, lsel_fields)
        mta_model_trn <- mta_model_
      }
      else # modelo criado predicao
      {
        mta_model_ <- predict_mta_fn(sel_traces_, process_instances_, mta_model_trn)
        mta_model_trn <- NULL
      }
      sel_traces_row <- c(sel_traces_row, list(sel_traces_))
      mta_model_row <- c(mta_model_row, list(mta_model_))
    }
    sel_traces_list <- c(sel_traces_list, list(sel_traces_row))
    mta_model_list <- c(mta_model_list, list(mta_model_row))
    # estatisticas de avaliação
    eval_stats_arr <- rbind(eval_stats_arr, eval_model_gen_fn(sel_traces_row))
  }
  #return(eval_stats_arr)
  Result <- list(eval_stats_arr, sel_traces_list, mta_model_list)
  
  gera_log("Finalizando build_and_run_mta_fn2",2)
  
  
  return(Result)
}

#
# metodo de busca Best First 
#






belong_to_list <- function(astate_list, astate)
{
  #astate_list <- open_list
  #astate <- expanded_state
  #rm(astate_list, astate)
  #state_exists <- F
  scount <- 1
  lstate_fields <- as.set(astate$fields)
  while (scount <= length(astate_list))
  {
    lstate_list_fields <- as.set(astate_list[[scount]]$fields)
    if (length(lstate_list_fields) == length(lstate_fields))
    {
      if (lstate_list_fields == lstate_fields)
      {
        break
      }
    }
    scount <- scount + 1
  }
  if (scount > length(astate_list))
    scount <- 0
  return(scount)
}

belong_to_list2 <- function(astate_list, astate)
{
  #astate_list <- open_list
  #astate <- expanded_state
  #rm(astate_list, astate)
  #state_exists <- F
  scount <- 1
  lstate_fields <- as.set(astate$fields)
  while (scount <= length(astate_list))
  {
    lstate_list_fields <- as.set(astate_list[[scount]]$fields)
    if (gset_is_equal(lstate_list_fields, lstate_fields))
    {
        break
    }
    scount <- scount + 1
  }
  if (scount > length(astate_list))
    scount <- 0
  return(scount)
}


expand_state_fn <- function(astate, asearch_fields)
{
  lstate_fields <- astate$fields
  lavailable_fields <- asearch_fields[!(asearch_fields %in% lstate_fields)]
  lexpanded_state_list <- list()
  if (length(lavailable_fields) > 0)
  {
    for (i in 1:length(lavailable_fields))
    {
      lstate <- list(fields=c(lstate_fields, lavailable_fields[i]), value=0, horiz = 1) 
      lexpanded_state_list[[i]] <- lstate
    }
  }
  return(lexpanded_state_list)
}



# calcula valor do estado
build_eval_state_fn <- function (aexpanded_state, k=2, n=1, horiz, num_max_itens_sel, 
                                 aincevtlog_list, remove_outliers = "none")
{
  #workaround
  #eval_stats_t00 <- NULL
  # eval_stats_t10 <- NULL
  gera_log("Iniciando build_eval_state_fn",2)
   
  aexpanded_state <- expanded_state
  sel_fields <- aexpanded_state$fields
  sel_fields_str <- toString(sel_fields)
  eval_stats_t <- NULL
  # horiz_sel_perr <- Inf
  # for (i in horiz_array)
  # {
  exp_list  <- build_and_run_mta_fn2(aincevtlog_list, horiz, num_max_itens_sel, 
                                    sel_fields, k, n, remove_outliers)
  # sel_traces_list_ <- exp_list$sel_traces_list
  # mta_model_list_ <- exp_list$mta_model_list
  # eval_stats_arr_ <- exp_list$eval_stats_arr
  eval_stats_arr_ <- data.frame(exp_list[1])
  #sel_traces_list_ <- exp_list[2]
  #mta_model_list_ <- exp_list[3]
  eval_stats_t <- rbind(eval_stats_t, cbind(horiz=horiz, eval_stats_arr_))
  
  #seleciona apenas os registros de validação pares
  #eval_stats_arr_val <- eval_stats_arr_[c(FALSE,TRUE),]
  #print(eval_stats_arr_val)
  
  eval_stats_t00 <- data.frame(
    sel_fields = sel_fields_str, k_fold = k, n_repet=n, max_itens = num_max_itens_sel, 
    rem_outl=remove_outliers, mtype, eval_stats_t)
  
  gera_log("Finalizando build_eval_state_fn",2)
  

  return(eval_stats_t00)
  
  # seleciona o erro percentual com o menor valor de treinamento
  # horiz_perr <- min(
  #   c(mean(na.omit(eval_stats_arr_val$perr_tot_set)),
  #     mean(na.omit(eval_stats_arr_val$perr_tot_mset)),
  #     mean(na.omit(eval_stats_arr_val$perr_tot_seq))
  #   )
  # )
  
  # usand set cycle
  # verifica se o erro é menor e atualiza horizonte caso seja
  # if (horiz_perr < horiz_sel_perr)
  # {
  #   horiz_sel <- i
  #   horiz_sel_perr <- horiz_perr
  # }
  #print(i)
  #print(eval_stats_arr_)
  # } 
  # avaliar percentual de erro e guardar campos e horizonte
  #perr_curr_fields <- horiz_sel_perr
  # curr_horiz <- horiz_sel
  
  # eval_stats_t0 <- rbind( eval_stats_t0, 
  #                     cbind(sel_fields=sel_fields_str, max_itens = num_max_itens_sel,  
  #                       mtype, eval_stats_t)
  #                     )
  
  # #eval_stats_t10 <- NULL
  # eval_stats_t10 <- rbind(eval_stats_t10,eval_stats_t00)
  
  # aexpanded_state_value <- aexpanded_state
  # aexpanded_state_value$value <- horiz_sel_perr
  # aexpanded_state_value$horiz <- curr_horiz
  # aexpanded_state_value$eval_stats <- eval_stats_t00

  # aexpanded_state_evalued <- list(fields=sel_fields, value=horiz_perr, 
  #                                 horiz = curr_horiz, eval_stats=eval_stats_t00) 
  # 
  # return(aexpanded_state_evalued)
}




# *****************************************************************************************






