#
# Data Handling
#
# Fun��es de carga e pr�-processamento do arquivo de log
#



load_incidentevtlog <- function(dirName, fileName, numberOfRows, fileVersion) {
   # carrega lista de incidentes normalizada
   require(readr)
   filePath <- file.path(dirName, fileName)
   incidentevtlog <-
      read.table(filePath, quote = "\"", sep = ",", dec = ".",
                 header = T, encoding = "utf-8", stringsAsFactors = T, nrow = numberOfRows)

   # Main date fields
   date_columns <- list()
   dateFormat <- "%d/%m/%Y %H:%M:%S"

   if ( fileVersion == 1 )
   {
      # arquivo original Claudio

      # principais colunas
      date_columns = c(created = "sys_created_on", opened = "opened_at", updated = "sys_updated_on", resolved = "resolved_at", closed = "closed_at")

      # outras colunas espec�ficas desse arquivo
      #incidentevtlog[,"sys_created_on"] <- as.POSIXct(strptime(incidentevtlog[,"sys_created_on"], "%d/%m/%Y %H:%M:%S"))
      #incidentevtlog[,"sys_updated_on"] <- as.POSIXct(strptime(incidentevtlog[,"sys_updated_on"], "%d/%m/%Y %H:%M:%S"))
      #incidentevtlog[,"opened_at"] <- as.POSIXct(strptime(incidentevtlog[,"opened_at"], "%d/%m/%Y %H:%M:%S"))
      #incidentevtlog[,"resolved_at"] <- as.POSIXct(strptime(incidentevtlog[,"resolved_at"], "%d/%m/%Y %H:%M:%S"))
      #incidentevtlog[,"closed_at"] <- as.POSIXct(strptime(incidentevtlog[,"closed_at"], "%d/%m/%Y %H:%M:%S"))
      incidentevtlog[,"activity_due"] <- as.POSIXct(strptime(incidentevtlog[,"activity_due"], "%d/%m/%Y %H:%M:%S"))
      incidentevtlog[,"vendor_closed_at"] <- as.POSIXct(strptime(incidentevtlog[,"vendor_closed_at"], "%d/%m/%Y %H:%M:%S"))
      incidentevtlog[,"vendor_opened_at"] <- as.POSIXct(strptime(incidentevtlog[,"vendor_opened_at"], "%d/%m/%Y %H:%M:%S"))
      incidentevtlog[,"vendor_resolved_at"] <- as.POSIXct(strptime(incidentevtlog[,"vendor_resolved_at"], "%d/%m/%Y %H:%M:%S"))
   } else if ( fileVersion == 2 ) {
      # arquivo novo sara

      # principais colunas
      date_columns = c(created = "sys_created_at", opened = "opened_at", updated = "sys_updated_at", resolved = "resolved_at", closed = "closed_at")

      dateFormat <- "%d/%m/%Y %H:%M"

      # outras colunas espec�ficas desse arquivo
      #incidentevtlog[, "sys_created_at"] <- as.POSIXct(strptime(incidentevtlog[, "sys_created_at"], "%d/%m/%Y %H:%M"))
      #incidentevtlog[, "sys_updated_at"] <- as.POSIXct(strptime(incidentevtlog[, "sys_updated_at"], "%d/%m/%Y %H:%M"))
      #incidentevtlog[, "opened_at"] <- as.POSIXct(strptime(incidentevtlog[, "opened_at"], "%d/%m/%Y %H:%M"))
      #incidentevtlog[, "resolved_at"] <- as.POSIXct(strptime(incidentevtlog[, "resolved_at"], "%d/%m/%Y %H:%M"))
      #incidentevtlog[, "closed_at"] <- as.POSIXct(strptime(incidentevtlog[, "closed_at"], "%d/%m/%Y %H:%M"))
   }

   # faz o mesmo set acima, para as colunas principais
   for(i in 1:length(date_columns)) {
      incidentevtlog[,date_columns[i]] <- as.POSIXct(strptime(incidentevtlog[,date_columns[i]], dateFormat))
   }


   #  // cria nova coluna para deixar gen�rico no c�digo
   incidentevtlog$updated_at <- incidentevtlog$sys_updated_at

   # logica para usar o ultimo valor quando h� mais de um registro de status
   # closed e resolved

   incidentevtlog_closed <-
      as.data.frame(incidentevtlog[which(incidentevtlog$incident_state == "Closed"),])
   nrow(incidentevtlog_closed)

   incidentevtlog_resolved <-
      as.data.frame(incidentevtlog[which(incidentevtlog$incident_state == "Resolved"),])
   nrow(incidentevtlog_resolved)

   DT <- setDT(incidentevtlog_closed)[order("number", "updated_at", "incident_state")]
   nrow(DT)

   DTR <- setDT(incidentevtlog_resolved)[order("number", "updated_at", "incident_state")]
   nrow(DTR)

   DTUNIQ <-
      as.data.frame(unique(
         DT,
         by = c("number", "incident_state"),
         na.rm = T,
         fromLast = TRUE
      ))
   nrow(DTUNIQ)

   DTUNIQR <-
      as.data.frame(unique(
         DTR,
         by = c("number", "incident_state"),
         na.rm = T,
         fromLast = TRUE
      ))
   nrow(DTUNIQR)

   #seleciona apenas incidentes que  est�o concluidos status == closed
   # os demais n�o servem para o modelo
   incidentevtlog <- subset(incidentevtlog, incidentevtlog$number %in% DTUNIQ$number)

   # atualiza novamente
   incidentevtlog_closed <- as.data.frame(DTUNIQ)
   nrow(incidentevtlog_closed)


   # replica tempo de conclus�o para todos os registros das variaveis target

   incidentevtlog$closed_at <- DTUNIQ$updated_at[match(incidentevtlog$number, DTUNIQ$number)]
   # Alexandre: veja se ela linha � mesmo necess�ria
   #incidentevtlog$close_code <- DTUNIQ$close_code[match(incidentevtlog$number, DTUNIQ$number)]
   incidentevtlog$closed_by <- DTUNIQ$closed_by[match(incidentevtlog$number, DTUNIQ$number)]

   incidentevtlog$resolved_at <- DTUNIQR$updated_at[match(incidentevtlog$number, DTUNIQR$number)]
   incidentevtlog$resolved_by <- DTUNIQR$resolved_by[match(incidentevtlog$number, DTUNIQR$number)]

   # campos sem o registro de contador inicial
   # Alexandre: nenhuma linha do log da Sara atende � essa condi��o
#   evt_with_na_mod_count <- is.na(incidentevtlog$sys_mod_count)
#   incidentevtlog[evt_with_na_mod_count,]$updated_at <- incidentevtlog[evt_with_na_mod_count,]$opened_at
#   incidentevtlog[evt_with_na_mod_count,]$sys_mod_count <- 0
#   incidentevtlog[evt_with_na_mod_count,]$sys_updated_by <- 'system'

   # cria campos com valor inteiro para a contagem do tempo
   #incidentevtlog$sys_created_on_stc <- as.integer(incidentevtlog[, "sys_created_at"])
   #incidentevtlog$sys_updated_on_stc <- as.integer(incidentevtlog[, "sys_updated_at"])
   #incidentevtlog$opened_at_stc <- as.integer(incidentevtlog[, "opened_at"])
   #incidentevtlog$resolved_at_stc <- as.integer(incidentevtlog[, "resolved_at"])
   #incidentevtlog$closed_at_stc <- as.integer(incidentevtlog[, "closed_at"])

   # cria campos com valor inteiro para a contagem do tempo (nova vers�o, mais gen�rica)
   incidentevtlog$created_at_stc <- as.integer(incidentevtlog[, date_columns["created"]])
   incidentevtlog$updated_at_stc <- as.integer(incidentevtlog[, date_columns["updated"]])
   incidentevtlog$opened_at_stc <- as.integer(incidentevtlog[, date_columns["opened"]])
   incidentevtlog$resolved_at_stc <- as.integer(incidentevtlog[, date_columns["resolved"]])
   incidentevtlog$closed_at_stc <- as.integer(incidentevtlog[, date_columns["closed"]])

   # gera contadores para modelo MTA
   # elapsed: op��o 1 = updated - opened; op��o 2 = updated - created
   incidentevtlog$elapsed_stc <- incidentevtlog$updated_at_stc - incidentevtlog$opened_at_stc
   #incidentevtlog$elapsed_stc <- incidentevtlog$updated_at_stc - incidentevtlog$created_at_stc

   # remaining: op��o 1 = closed - updated; op��o 2 = resolved - updated
   incidentevtlog$remaining_stc <- incidentevtlog$closed_at_stc - incidentevtlog$updated_at_stc
   #incidentevtlog$remaining_stc <- incidentevtlog$resolved_at_stc - incidentevtlog$updated_at_stc

   #executada op��o 1,1
   #executada op��o 2,1
   #executada op��o 1,2
   #executada op��o 2,2

   #em execu��o: op��o 1,1


   # tempo de sojourn a ser preenchido na constru��o do modelo
   incidentevtlog$sojourn_set_stc <-  0
   incidentevtlog$sojourn_mset_stc <-  0
   incidentevtlog$sojourn_seq_stc <-  0

   #remover variaveis temporarias
   rm(DT, DTR, DTUNIQ, DTUNIQR)

   #cria colunas para os estados
   incidentevtlog$seq_state_id <- NA
   incidentevtlog$set_state_id <- NA
   incidentevtlog$mset_state_id <- NA
   incidentevtlog$seq_state_id <- 0
   incidentevtlog$set_state_id <- 0
   incidentevtlog$mset_state_id <- 0


   lincevtloglist <- list(incidentevtlog = incidentevtlog,
                          incidentevtlog_closed = incidentevtlog_closed)
   return (lincevtloglist)
}
