
library(here) #To find path of home directory of repo
library(tidyverse)
library(brms)


set.seed(996) #ensures reproducibility for testing


Roudaia_data_dir<- here("..","roudaia_faubert_data","dat_csvs")

#"There is a handy csv that has all of them (sub-ALL_task-motc.csv)"
Roudaia_data_file<- "sub-ALL_task-motc.csv"       

if (!dir.exists(Roudaia_data_dir)) {
  message("Your data folder doesn't exist.")
}
Roudaia_data_file_with_path<- file.path(Roudaia_data_dir, Roudaia_data_file)

#Define function for when need to catch warnings and errors, https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
myTryCatch <- function(expr) { 
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}


read_csv_but_handle_erroneous_columns<- function(file_path) {
  #Check whether this is one of the files with the missing column label
  if (str_detect(file_path, "J55_b|J55_c|J56_|J57_")) { #One of the files with the missing column label
    #So supply correct column labels when read the file
    dfWithWarnings<- myTryCatch( 
      readr::read_tsv(file_path, show_col_types=FALSE, col_names=correctColumnNames, skip=1) #skip incorrect header
    )
  } else { #normal file
    #message(file_path)
    dfWithWarnings<- myTryCatch( 
      readr::read_tsv(file_path, show_col_types=FALSE) 
    )
  }
  return (dfWithWarnings)
}

dfWithWarnings<- myTryCatch( 
    readr::read_csv(Roudaia_data_file_with_path, show_col_types=TRUE) 
  )
if (!is.null(dfWithWarnings$warning)) {
  print("Warnings reading Roudaia data:")
  print(dfWithWarnings$warning)
}
if (!is.null(dfWithWarnings$error)) {
  print("Error reading Roudaia data:")
  print(dfWithWarnings$error)
}
Roudaia_data<- dfWithWarnings$value
numSs<- length(table(Roudaia_data$subj)) #36

#"cond has pretty self explanatory condition names:
#  t1.d05 == 1 targe`t, 5 objects per ring. t2.d10: 2 targets, 10 objects per ring."

Roudaia_data <- Roudaia_data %>%
  separate(cond, into = c("targetsString", "objsString"), sep = "\\.", remove = FALSE) %>%
  mutate(
    numTargets = as.integer(sub("^0*", "", sub(".*t(\\d+).*", "\\1", targetsString))),
    objPerRing = as.integer(sub("^0*", "", sub(".*d(\\d+).*", "\\1", objsString)))
  )
Roudaia_data$targetsString<- NULL
Roudaia_data$objPerRing<- NULL
#Validate the ChatGPT data
#table(Roudaia_data$cond,Roudaia_data$numTargets)
#table(Roudaia_data$cond,Roudaia_data$objPerRing)



# responseRing is the ring that was probed for the response. 1 = inner, 2 = middle 3 = outer,
  