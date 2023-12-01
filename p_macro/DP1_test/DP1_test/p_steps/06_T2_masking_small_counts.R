




#MASK small counts
col<-c("numerator","denominator")
temp<-paste0(col,"=5")
temp2<-paste("c(",paste(temp, collapse = ','),")")
suppressWarnings(
  DRE_Threshold(
    Inputfolder = direxp,
    Outputfolder = dirsmallcountsremoved,
    Delimiter = ",",
    Varlist = c(eval(parse(text=(temp2)))),
    FileContains = "D4_proportion_per_"
  )
)


col<-c("N")
temp<-paste0(col,"=5")
temp2<-paste("c(",paste(temp, collapse = ','),")")
suppressWarnings(
  DRE_Threshold(
    Inputfolder = direxp,
    Outputfolder = dirsmallcountsremoved,
    Delimiter = ",",
    Varlist = c(eval(parse(text=(temp2)))),
    FileContains = "Flowchart_"
  )
)


col<-c("N_disp_linked_to")
temp<-paste0(col,"=5")
temp2<-paste("c(",paste(temp, collapse = ','),")")
suppressWarnings(
  DRE_Threshold(
    Inputfolder = direxp,
    Outputfolder = dirsmallcountsremoved,
    Delimiter = ",",
    Varlist = c(eval(parse(text=(temp2)))),
    FileContains = "Dispensing_"
  )
)


col<-c("count")
temp<-paste0(col,"=5")
temp2<-paste("c(",paste(temp, collapse = ','),")")
suppressWarnings(
  DRE_Threshold(
    Inputfolder = direxp,
    Outputfolder = dirsmallcountsremoved,
    Delimiter = ",",
    Varlist = c(eval(parse(text=(temp2)))),
    FileContains = "D5_meaning"
  )
)


