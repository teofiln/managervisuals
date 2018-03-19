library("tidyverse")

#create a single data frame from monthtly cash flow statemets
cashflow <- data_frame()

filenames <- list.files(path="cashflow", pattern="*csv", full.names = TRUE)
for (i in seq_along(filenames)) {
 
  # read the delimited file. it is "\t" (tab) delimited
  RAW <- read.delim(filenames[i], sep="\t", stringsAsFactors = FALSE)

  # make a column called Date
  # take the name of the 3 column (which was the date)
  # do a (global) substitute of "X" with nothing
  # the X is added by R because column names cannot start with a number
  RAW$Date <- gsub("X", "", colnames(RAW)[3])

  # replace the name of column 3, which was the date, with 'amount'
  colnames(RAW)[3] <- "Amount"
  # replace te name of column 2 with 'account'
  colnames(RAW)[2] <- "Account"


  # make sure numbers are formated with . for decimal
  RAW$Amount <- gsub("\\.", "", RAW$Amount)
  RAW$Amount <- gsub(",", ".", RAW$Amount)
  # make sure ammounts are numeric
  RAW$Amount <- as.numeric(RAW$Amount)

  # split the table into rows corresponding to summary values
  # e.g., net increase (decrease) in cash held
  # these are the last 4 rows
  # the operation just subsets with the range from the total number of rows -4 to the total number of rows
  # to take just the last four rows
  RAW_SUMS <- RAW[(nrow(RAW)-3):nrow(RAW),]

  # reverse this subsetting with the minus sign
  RAW_RAW <- RAW[-((nrow(RAW)-3):nrow(RAW)),]


  #remove accounts with amount 0
  RAW_RAW <- filter(RAW_RAW, Amount != 0)

  #make outflows negative (in case we need that)
  #cashflow <- cashflow %>%
  #  mutate(Direction = case_when(Group == "Inflows" ~ Amount,
  #                               Group == "Outflows" ~ -Amount))

  #append to data frame
  cashflow <- bind_rows(cashflow, RAW_RAW)
}

write.csv(cashflow, file = "cashflow.csv")
