library(summarytools)  # for creating data summaries

### LOAD DATA -----------------------------
# add code to load in Sanquin data
df <- NA

### SUBSET DATA -----------------------------
# subset data to DonorID, Visit Date + the 12 variables that will be used to train ML models as noted in the excel file
df <- NA

### CREATE SUMMARIES -------------------------------
# use summary tools to create data summary
print(dfSummary(df), file="sanquin_summary_prelabelling.html")

