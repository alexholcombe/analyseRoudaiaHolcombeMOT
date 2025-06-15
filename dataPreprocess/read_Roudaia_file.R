
library(here) #To find path of home directory of repo
library(tidyverse)
library(brms)

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
    numObjects = as.integer(sub("^0*", "", sub(".*d(\\d+).*", "\\1", objsString)))
  )
Roudaia_data$cond<- NULL
Roudaia_data$targetsString<- NULL
Roudaia_data$objsString<- NULL

#Validate the ChatGPT data
#table(Roudaia_data$cond,Roudaia_data$numTargets)
#table(Roudaia_data$cond,Roudaia_data$numObjects)

# responseRing is the ring that was probed for the response. 1 = inner, 2 = middle 3 = outer,
# Response ring strangely has many more outer, then inner, then middle
tab <- table(Roudaia_data$subj, Roudaia_data$responseRing)
# Convert counts to row-wise percentages, for each subject
#tab_pct <- prop.table(tab, margin = 1) * 100
#print( round(tab_pct,0) )
ring_response_percent <- Roudaia_data %>%
  count(responseRing) %>%
  mutate(percent = 100 * n / sum(n))
print(ring_response_percent) #28%, 11%, 61%

Roudaia_data <- Roudaia_data %>%
  mutate(ageGroup = if_else(group == 1, "younger", "older"))
Roudaia_data$group<-NULL

#Code gender in same way as Holcombe, with male and female
Roudaia_data<- Roudaia_data |> mutate(gender = case_when(
  gender == "M" ~ "male",
  gender == "F" ~ "female",
  TRUE ~ gender))
#Code ageGroup in same way as Holcombe
Roudaia_data <- Roudaia_data |>
  mutate(ageGroup = case_when(
    ageGroup == "Old" ~ "older",
    ageGroup == "Young" ~ "younger",
    TRUE ~ ageGroup
  ))

#Plot data for each participant
gg<- ggplot(Roudaia_data, #data_one_condition, 
            aes(x=speed,y=correct,linetype=factor(numObjects),
                color=factor(numTargets))) +
  stat_summary(fun=mean,geom="point") +
  stat_summary(fun=mean,geom="smooth")  +
  facet_wrap(subj~.) +
  labs(x = "Speed (revolutions per second)",
       y = "Correct",
       title = "Roudaia raw data") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) #remove gridlines

#Show floor with lines
gg<- gg + geom_hline( aes(yintercept = 1/numObjects),
                      colour = "purple", alpha=0.2 )
show(gg)

destination_fname<- "Roudaia_preprocessed.tsv"
destination<- here("dataPreprocess",destination_fname)
readr::write_tsv(Roudaia_data, file = destination)
