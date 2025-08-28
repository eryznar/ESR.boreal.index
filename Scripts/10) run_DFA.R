# combine multiple indicators of borealization with 
# a Dynamic Factor Analysis model

source("./Scripts/1) load_libs_params.R")

# Updated 9/4/24 with 2024 data by ERR

# Omitting chl-a size fraction, BCS prevalence

## data processing -------------------------------------

#bloom timing
d1 <- read.csv(paste0("./Output/", current.year, "/bloom_timing.csv")) %>%
  dplyr::select(!X)


#bloom type  
d2 <- read.csv(paste0("./Output/", current.year, "/bloom_type.csv")) %>%
  dplyr::select(!X)

#sea ice
d3 <- read.csv(paste0("./Output/", current.year, "/ice.csv"))
  
#bottom temp
d4 <- read.csv(paste0("./Output/", current.year, "/date_corrected_bottom_temp.csv"))

#groundfish CPUE
d5 <- read.csv(paste0("./Output/", current.year, "/groundfish_mean_cpue.csv")) %>%
  dplyr::select(!X)

#zooplankton abundances
d6 <- read.csv(paste0("./Output/", prev.year, "/sdmTMB_copepods.csv")) %>%
  dplyr::select(!X) %>%
  rename(value = log_abundance)

# Combine all data
dat <- rbind(d1, d2, d3, d4, d5, d6)
 
# add NAs for plot
plot.dat <- dat %>%
  pivot_wider(names_from = name,
              values_from = value) %>%
  pivot_longer(cols = -year)



# reorder for plot
plot.order <- data.frame(name = unique(plot.dat$name),
                         order = c(3, 4, 5, 2, 1, 8, 9, 6, 7))

plot.dat <- left_join(plot.dat, plot.order)

plot.dat$name <- reorder(plot.dat$name, plot.dat$order)

plot.dat$name <- factor(plot.dat$name, levels = c("Jan-Feb ice cover", "Mar-Apr ice cover", "Bottom temperature",
                                                  "Open water bloom", "Bloom timing", "small copepods", "large copepods", "Pacific cod",
                                                  "Arctic groundfish"), 
                                       labels = c("Jan-Feb ice cover", "Mar-Apr ice cover", "Bottom temperature",
                                                  "Open water bloom", "Bloom timing", "Small copepods", "Large copepods", "Pacific cod", 
                                                  "Arctic groundfish"))

# save as Extended Data Fig. 4
ggplot(plot.dat, aes(year, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~name, scales = "free_y", ncol = 4) +
  theme(axis.title.x = element_blank()) +
  ylab("Value")

ggsave(paste0("./Figures/", current.year, "/DFA_timeseries.png"), width = 10, height = 6, units = 'in')


# Save time series
write.csv(dat, paste0("./Output/", current.year, "/dfa time series.csv"), row.names = F)

dfa.dat <- plot.dat %>%
  dplyr::select(!order) %>%
  pivot_wider(names_from = name, values_from = value) %>% 
  arrange(year) %>%
  dplyr::select(-year) %>%
  t()
  
colnames(dfa.dat) <- sort(unique(plot.dat$year))

# and plot correlations
cors <- cor(t(dfa.dat), use = "p")
diag(cors) <- 0

max(cors)
min(cors) 

plot <- as.data.frame(t(dfa.dat))

# plot correlations
corrplot(cors, method = "sq", col.lim = c(-0.865, 0.865), col = oceColorsPalette(64), tl.col = "black", cl.cex = 0.7, order = "FPC")


# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

# fit models & store results
for(R in levels.R) {
  for(m in 1:2) {  # considering either 1- or 2-trend model
    print(paste0("fitting level ", R, ", m=", m))
    dfa.model = list(A="zero", R=R, m=m)
    
    kemz = MARSS(dfa.dat, model=dfa.model,
                 form="dfa", z.score=TRUE, control=cntl.list)
    
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    
    assign(paste("kemz", m, R, sep="."), kemz)
  } # end m loop
} # end R loop

# calculate delta-AICc scores, sort in descending order, and compare
model.data$dAICc <- model.data$AICc-min(model.data$AICc)
model.data <- model.data %>%
  arrange(dAICc)
model.data # UNCONSTRAINED MODELS DID NOT CONVERGE

# save model selection table--note that unconstrained models did not converge!
write.csv(model.data, paste0("./Output/", current.year, "/dfa_model_selection_table.csv"),
          row.names = F)

## fit the best model --------------------------------------------------
model.list = list(A="zero", m=2, R="diagonal and unequal") # best model is two-trend

# not sure that these changes to control list are needed for this best model, but using them again!
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# rotate
# get the inverse of the rotation matrix
Z.est <- coef(mod, type = "matrix")$Z

H.inv <- varimax(coef(mod, type = "matrix")$Z)$rotmat

# rotate factor loadings
Z.rot <- Z.est %*% H.inv

# rotate trends
trends.rot <- solve(H.inv) %*% mod$states

# Add CIs to marssMLE object
mod <- MARSSparamCIs(mod)

# Use coef() to get the upper and lower CIs
Z.low <- coef(mod, type = "Z", what = "par.lowCI")
Z.up <- coef(mod, type = "Z", what = "par.upCI")
Z.rot.up <- Z.up %*% H.inv
Z.rot.low <- Z.low %*% H.inv

plot.CI <- data.frame(names=rownames(dfa.dat),
  mean = as.vector(Z.rot),
  upCI = as.vector(Z.rot.up),
  lowCI = as.vector(Z.rot.low)
)
plot.CI

dodge <- position_dodge(width=0.9)

plot.CI$names <- reorder(plot.CI$names, plot.CI$mean)


plot.CI$trend <- rep(c("T1", "T2"), each = length(unique(plot.CI$names)))

loadings.plot <- ggplot(plot.CI, aes(x=names, y=mean, fill = trend)) +
  geom_bar(position=dodge, stat="identity") +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme(axis.text.x  = element_text(angle=60, hjust=1,  size=9), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0) #only one doesn't overlap zero

# plot trend
trend <- data.frame(trend = rep(c("T1", "T2"), each = length(1972:current.year)),
                    t=1972:current.year,
                    estimate=as.vector(mod$states),
                    conf.low=as.vector(mod$states)-1.96*as.vector(mod$states.se),
                    conf.high=as.vector(mod$states)+1.96*as.vector(mod$states.se))


trend.plot <- ggplot(trend, aes(t, estimate, color = trend, fill = trend)) +
  theme_bw() +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_ribbon(aes(x=t, ymin=conf.low, ymax=conf.high), linetype=0, alpha=0.1) + xlab("") + ylab("Trend")

 #ggsave(paste0("./Figures/", current.year, "/best_two_trend_DFA_loadings_trend.png"), width = 9, height = 3.5, units = 'in')

# save
ggpubr::ggarrange(loadings.plot,
                  trend.plot,
                  ncol = 2,
                  widths = c(0.45, 0.55),
                  labels = "auto")

# only two loadings (?) can be distinguished from 0! 
# reject this model and fit second-best model (1 trend diagonal and unequal)

model.list = list(A="zero", m=1, R="diagonal and unequal") # fourth best model that converged - this is the borealization index

mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# save 
saveRDS(mod, paste0("./Output/", current.year, "/DFA_model.rds"))

# plot fits to data
DFA_pred <- print(predict(mod))

DFA_pred <- DFA_pred %>%
  mutate(year = rep(1972:current.year, length(unique(DFA_pred$.rownames)))) 

# get R^2 for each time series
summarise <- DFA_pred %>%
  group_by(.rownames) %>%
  reframe(R_sq = cor(y, estimate, use = "pairwise")^2) %>%
  mutate(plot_label = paste(.rownames, " (", round(R_sq, 3), ")", sep = ""))

DFA_pred <- left_join(DFA_pred, summarise)


ggplot(DFA_pred, aes(estimate, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~plot_label, ncol = 4, scale = "free") +
  labs(x = "Estimated", y = "Observed")

ggsave(paste0("./Figures/", current.year, "/DFA.BI_tsfits.png"), width = 10, height = 6, units = 'in')


# process loadings and trend

CI <- MARSSparamCIs(mod)

plot.CI <- data.frame(names=rownames(dfa.dat),
                          mean=CI$par$Z[1:length(unique(rownames(dfa.dat)))],
                          upCI=CI$par.upCI$Z[1:length(unique(rownames(dfa.dat)))],
                          lowCI=CI$par.lowCI$Z[1:length(unique(rownames(dfa.dat)))])

dodge <- position_dodge(width=0.9)


plot.CI$names <- reorder(plot.CI$names, CI$par$Z[1:length(unique(plot.CI$names))])


# plot trend
trend <- data.frame(t=1972:current.year,
                        estimate=as.vector(mod$states),
                        conf.low=as.vector(mod$states)-1.96*as.vector(mod$states.se),
                        conf.high=as.vector(mod$states)+1.96*as.vector(mod$states.se))

trend.plot <- ggplot(trend, aes(t, estimate))+
                geom_ribbon(mapping = aes(ymin = conf.low, ymax = conf.high), fill = "lightblue", alpha = 0.4)+
                geom_line(linewidth = 1, color = "dodgerblue3")+
                geom_point(color = "dodgerblue3", size = 2)+
                ylab("Borealization index")+
                xlab("Year")

loadings.plot <- ggplot(plot.CI, aes(x=names, y=mean)) +
  geom_bar(position=dodge, stat="identity", fill = "dodgerblue3") +
  geom_errorbar(aes(ymax=upCI, ymin=lowCI), position=dodge, width=0.5) +
  ylab("Loading") +
  xlab("") +
  theme(axis.text.x  = element_text(angle=60, hjust=1,  size=9), legend.title = element_blank(), legend.position = 'top') +
  geom_hline(yintercept = 0) 

ggsave(plot = trend.plot, paste0("./Figures/", current.year, "/borealization_index_plot.png"), width = 7, height = 5, units = "in")
ggsave(plot = loadings.plot, paste0("./Figures/", current.year, "/loadings_plot.png"), width = 7, height = 5, units = "in")


# and save loadings and trend
write.csv(plot.CI, paste0("./Output/", current.year, "/dfa_loadings.csv"), row.names = F)
write.csv(trend, paste0("./Output/", current.year, "/dfa_trend.csv"), row.names = F)

          