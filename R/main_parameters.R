####
#
# Sequência de execução dos arquivos
# - load_library.r
# - data_handling.r
# - state_helpers.r
# - best_first.r
# - save_results.r
# - main.r
#
####

############ CONSTANTES
HIGH_ERROR_CONST <- 2^31

NIVEL_LOG <- 1

############ PARÂMETROS

num_max_itens_sel <- 100

dirName <- 'C:/Mestrado/DadosClaudio'

fileFormat = 1;

paraleliza = FALSE;

remove_outliers <- 'none'

k <- 5
n <- 1

max_sel_fields <- 15
mtype <- c("T","V")

horiz_array <- c(3)
epsilon <- 0.01

MAX_K_EXP_COUNT <- 1



main()
