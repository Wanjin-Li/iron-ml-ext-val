# Create Table 1

library(boot)
library(data.table)

# Load data ----
rise_hf <- fread("./3_intermediate/private/hgb_ferr_rise.csv")
rise_h <- fread("./3_intermediate/private/hgb_only_rise.csv")

str(rise_hf)
table1(~ rbc_loss_last_12_months + rbc_loss_last_24_months + r + thickness | status, data=melanoma2,
       overall=c(left="Total"), caption=caption, footnote=footnote)



x <- fread( "./3_intermediate/model_dev_data/main_model/pred_hgb/mdset_factors_hgb_only.csv")

str(x)

melanoma2 <- melanoma

# Factor the basic variables that
# we're interested in
melanoma2$status <- 
  factor(melanoma2$status, 
         levels=c(2,1,3),
         labels=c("Alive", # Reference
                  "Melanoma death", 
                  "Non-melanoma death"))






melanoma2$sex <- 
  factor(melanoma2$sex, levels=c(1,0),
         labels=c("Male", 
                  "Female"))

melanoma2$ulcer <- 
  factor(melanoma2$ulcer, levels=c(0,1),
         labels=c("Absent", 
                  "Present"))

label(melanoma2$sex)       <- "Sex"
label(melanoma2$age)       <- "Age"
label(melanoma2$ulcer)     <- "Ulceration"
label(melanoma2$thickness) <- "Thicknessᵃ"

units(melanoma2$age)       <- "years"
units(melanoma2$thickness) <- "mm"

caption  <- "Basic stats"
footnote <- "ᵃ Also known as Breslow thickness"

table1(~ sex + age + ulcer + thickness | status, data=melanoma2,
       overall=c(left="Total"), caption=caption, footnote=footnote)