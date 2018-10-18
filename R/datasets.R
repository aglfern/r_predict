# Novo algoritmo de geração dos datasets
#

# ******************* Testing area ***************************
dataset_size_list <- c(5, 8, 12);

r <- dataset_splitting_indexes(dataset_size_list)

r

rm(r,dataset_size_list)

getwd()

xtable <- read.table(file.path('data','novo_log_sara.csv'), quote = "\"", sep = ",", dec = ".",
                    header = T, encoding = "utf-8", stringsAsFactors = T, nrow = 100)

require(dplyr)
xdistinct <- distinct(xtable,number)

nrow(xdistinct)

xdsize <- c(3, 4, 10)

xsplitted <- dataset_split(xdistinct,xdsize)

sum(xdsize[1:2])

xdistinct[c(2,4,6),]

as.list(xsplitted[[1]])

rm(xtable,xdistinct,xsplitted)


# ******************* Testing area ***************************



#' dataset_splitting_indexes
#' Returns randomized lists of indexes, based on the number and sizes indicated
#' @param datasets_sizes_list: a list containing the desired sizes of the splitted datasets
#'
#' @return
#' @export
#'
#' @examples
dataset_splitting_indexes <- function(ds_sizes_list)
{
   generate_log(paste("Generating random indexes for ", length(ds_sizes_list), " smaller datasets."),1)
   start <- 1; end <- 0
   random_indexes <- sample(sum(ds_sizes_list))
   datasets_indexes <- vector("list",length(ds_sizes_list))
   for(i in 1:length(ds_sizes_list))
   {
      end = end + ds_sizes_list[i]
      generate_log(paste("start: ",start," end: ", end),2)
      datasets_indexes[[i]] <- random_indexes[start:end]
      start = start + ds_sizes_list[i]
   }
   return(datasets_indexes)
}

#' Splits a dataset into smaller datasets according to the sizes requested
#'
#' @param dataset_to_split
#' @param splitting_sizes
#'
#' @return
#' @export
#'
#' @examples
dataset_split <- function(dataset_to_split, splitting_sizes)
{
   ds_size <- nrow(dataset_to_split)
   number_of_splits <- length(splitting_sizes)
   generate_log(paste("Splitting dataset of size ",ds_size),1)
   if(sum(splitting_sizes)>ds_size)
   {
      x <- sum(splitting_sizes[1:(number_of_splits-1)])
      splitting_sizes[number_of_splits] <- ds_size - x
      generate_log(paste("Size of last subset must not be greater than ", splitting_sizes[number_of_splits]))
   }
   indexes <- dataset_splitting_indexes(splitting_sizes)
   generate_log(paste("Indexes: ",indexes),2)
   splitted_datasets <- vector("list",number_of_splits)
   for(i in 1:number_of_splits)
   {
      splitted_datasets[[i]] <- dataset_to_split[indexes[[i]],]
   }
   return(splitted_datasets)
}

