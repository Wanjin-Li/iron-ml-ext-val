library(readxl)
library(ggplot2)
library(data.table)

t_dat <- read_excel("sub_group_lm_data.xlsx") |>
  data.table()

t_dat[, Variable := factor(Variable, levels = t_dat$Variable)]
#wide to long
t_dat <- melt(t_dat, id.vars = c("Variable"), variable.name = "concat")


t_dat[, c("Country", "Dataset", "Outcome", "Measure", "Measure2") := tstrsplit(as.character(concat), '_')]
t_dat[, Measure := fifelse(is.na(Measure2), Measure, Measure2)]
t_dat[, Measure2 := NULL]


t_dat[, Country := fifelse(Country=="s", "SA", "US")]
t_dat[, Dataset := fifelse(Dataset== "h", "Hemoglobin only", "Hemoglobin and ferritin")]
t_dat[, Outcome := fifelse(Outcome== "h", "Hemoglobin", "Ferritin")]

t_dat <- dcast(t_dat, Variable + Country + Dataset + Outcome ~ Measure, value.var = "value")
t_dat[, significant := fifelse(lower > 0 | upper < 0, T, F) ]

ggplot(data = t_dat[Variable !="Intercept"], 
       aes(x = coef, xmin = lower, xmax = upper, 
           y = Variable, color = Country, alpha = significant))+
  facet_grid(cols = vars(Dataset), rows = vars(Outcome))+
  geom_pointrange(position = position_dodge(width=.7))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()+
  scale_alpha_discrete(range = c(0.4, 1), guide = 'none')+
  labs(y="", x="Linear regression coefficient")+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(-8, 8))+
  annotate("text", label="Higher\nerror",x = 6, y = 6, color = "grey27")+
  annotate("text", label="Lower\nerror",x = -6, y = 6, color = "grey27")
  
ggsave("forest.svg",
       width = 6,
       height = 5,
       unit="in")
