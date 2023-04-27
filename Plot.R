#################################################################################
#  plots                                                                        #
#################################################################################
library(magrittr) 
library(plyr)
library(ggpubr)
library(ggthemes)
library("ggsci")
library("ggplot2")
library(RColorBrewer)
library(grid)
library(gtable)
library(scales)
library(dplyr)

get.results <- function(x){
  result <- lapply(LETTERS[1:10], function(m.DAG){
    load(paste("res",x,m.DAG,"Rda", sep = ".")) 
    result <- simu.res[-1,]
  }) %>% ldply(., data.frame) 
  result[,1:14] <- apply(result[,1:14], 2, as.numeric)
  result[,15:19] <- lapply(result[,15:19], as.factor)
  levels(result$scenario) <- LETTERS[1:10]
  levels(result$Missingness) <- c("i","ii","iii","iv","v")
  levels(result$Outcome) <- c("I","II","III","IV","V","VI")
  result$Method <- factor(result$Method, levels = c("CCA","MI.Sim","MI.EC","MI.Com","MI.CART","MI.RF","MI.SMC"))
  result <- result %>%
    mutate(Method = recode(Method, 'MI.Sim'='MI-Sim', 'MI.EC'='MI-EC', 'MI.Com'='MI-Com', 'MI.CART'='MI-CART', 'MI.RF'='MI-RF', 'MI.SMC'='MI-SMC'))
  result
}
res.10 <- get.results("10%")
res.50 <- get.results("50%")
summary(res.10)
summary(res.50)
save(res.10, file = "res.10%.Rda")
save(res.50, file = "res.50%.Rda")


add.label.T.R <- function(plot, label.T = "m-DAGs", label.R = "Methods"){
  grob <- ggplotGrob(plot)
  posT <- subset(grob$layout, grepl("strip-t", name), select = t:r)
  posR <- subset(grob$layout, grepl("strip-r", name), select = t:r)
  stripT <- gTree(name = "Strip_top", children = gList(rectGrob(gp = gpar(col = NA, fill = NA)), textGrob(label.T, gp = gpar(fontsize = 15))))
  stripR <- gTree(name = "Strip_right", children = gList(rectGrob(gp = gpar(col = NA, fill = NA)), textGrob(label.R, rot = -90, gp = gpar(fontsize = 15))))
  plot <- gtable_add_cols(grob, unit(0.5, "cm"), max(posR$r)) %>% 
    gtable_add_grob(., stripR, t = min(posR$t), l = max(posR$r) + 1, b = max(posR$b), name = "strip-right") %>%
    gtable_add_cols(., unit(1/5, "line"), max(posR$r)) %>%
    gtable_add_rows(., unit(1, "cm"), min(posT$t)-1) %>% 
    gtable_add_grob(., stripT, r = max(posT$r), l = min(posT$l), t = min(posT$t), name = "strip-top") %>%
    gtable_add_rows(., unit(1/100, "line"), min(posT$t)) 
  grid.newpage()
  grid.draw(plot)
}

ReBias <- function(data){ggplot(data, aes(x = Missingness, y = Outcome)) + 
    geom_raster(aes(fill = PrBias), hjust = 0.5, vjust = 0.5, interpolate=F) +
    geom_text(aes(label = round(PrBias)), size=4, colour = ifelse(data$PrBias >= -49.5 & data$PrBias <= 49.5, "black", "white")) +
    scale_fill_gradient2(name="Bias (%)  ", low="#00324AFF", mid="#FFFFFFFF", high="#4C0000FF", na.value = "#072638", 
                         limits = c(-100,100), midpoint = 0, breaks=c(-100,-50,-20,0,20,50,100)) +
    scale_y_discrete(name ="Outcome scenario", limits = c("VI","V","IV","III","II","I")) +
    scale_x_discrete(name ="Missingness scenario") +
    theme_classic() + theme(legend.key.size = unit(0.3, "cm"), legend.key.width = unit(4, "cm")) +
    theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5), 
          plot.caption = element_text(hjust = 1, size = 20), legend.position = "bottom") +
    facet_grid(Method ~ scenario, scales = "free", space = "free", drop = TRUE, labeller = labeller(outcome = label_wrap_gen(10))) +
    theme(strip.text.y = element_text(angle = 0)) 
}
# Produce Figure 3 of the manuscript
png(file = "Figure_3a.png", width = 1800, height = 900)
ReBias(res.10) %>% `+`(labs(subtitle = "Low exposure prevalence (10%)")) %>% add.label.T.R(.) 
dev.off()

png(file = "Figure_3b.png", width = 1800, height = 900)
ReBias(res.50) %>% `+`(labs(subtitle = "Moderate exposure prevalence (50%)")) %>% add.label.T.R(.)
dev.off()

EmpSE <- function(data){ggplot(data, aes(x = Missingness, y = Outcome)) + 
    geom_raster(aes(fill = EmpSE), hjust = 0.5, vjust = 0.5, interpolate=F) +
    geom_text(aes(label = round(EmpSE,2)), size=4, colour = "white") +
    scale_fill_gradient2(name="Empirical SE  ", low="#00324AFF", mid="#FFFFFFFF", high="#4C0000FF", na.value = "#2C0000FF", 
                         limits = c(0,0.3), midpoint = 0, breaks=c(0,0.1,0.2,0.3)) +
    scale_y_discrete(name ="Outcome scenario", limits = c("VI","V","IV","III","II","I")) +
    scale_x_discrete(name ="Missingness scenario") +
    theme_classic() + theme(legend.key.size = unit(0.3, "cm"), legend.key.width = unit(4, "cm")) +
    theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5), 
          plot.caption = element_text(hjust = 1, size = 20), legend.position = "bottom") +
    facet_grid(Method ~ scenario, scales = "free", space = "free", drop = TRUE, labeller = labeller(outcome = label_wrap_gen(10))) +
    theme(strip.text.y = element_text(angle = 0)) 
}
# Produce Figure 4 of the manuscript
png(file = "Figure_4a.png", width = 1800, height = 900)
EmpSE(res.10) %>% `+`(labs(subtitle = "Low exposure prevalence (10%)")) %>% add.label.T.R(.) 
dev.off()

png(file = "Figure_4b.png", width = 1800, height = 900)
EmpSE(res.50) %>% `+`(labs(subtitle = "Moderate exposure prevalence (50%)")) %>% add.label.T.R(.)
dev.off()

Coverage <- function(data){ggplot(data, aes(x = Missingness, y = Outcome)) + 
    geom_raster(aes(fill = Coverage), hjust = 0.5, vjust = 0.5, interpolate=F) +
    geom_text(aes(label = round(Coverage)), size=4, colour = ifelse(round(data$Coverage) >= 90, "black", "white")) +
    scale_fill_gradient2(name="Coverage (%)  ", low="#00324AFF", mid="#FFFFFFFF", high="#4C0000FF", na.value = "#072638", 
                         limits = c(80,100), midpoint = 95, breaks=c(80,85,90,95,100)) +
    scale_y_discrete(name ="Outcome scenario", limits = c("VI","V","IV","III","II","I")) +
    scale_x_discrete(name ="Missingness scenario") +
    theme_classic() + theme(legend.key.size = unit(0.3, "cm"), legend.key.width = unit(4, "cm")) +
    theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5), 
          plot.caption = element_text(hjust = 1, size = 20), legend.position = "bottom") +
    facet_grid(Method ~ scenario, scales = "free", space = "free", drop = TRUE, labeller = labeller(outcome = label_wrap_gen(10))) +
    theme(strip.text.y = element_text(angle = 0)) 
}
# Produce Figure 5 of the manuscript
png(file = "Figure_5a.png", width = 1800, height = 900)
Coverage(res.10) %>% `+`(labs(subtitle = "Low exposure prevalence (10%)")) %>% add.label.T.R(.) 
dev.off()

png(file = "Figure_5b.png", width = 1800, height = 900)
Coverage(res.50) %>% `+`(labs(subtitle = "Moderate exposure prevalence (50%)")) %>% add.label.T.R(.)
dev.off()

MSE <- function(data){ggplot(data, aes(x = Missingness, y = Outcome)) + 
    geom_raster(aes(fill = MSE), hjust = 0.5, vjust = 0.5, interpolate=F) +
    geom_text(aes(label = round(MSE,2)), size=4, colour = "white") +
    scale_fill_gradient2(name="Empirical SE  ", low="#00324AFF", mid="#FFFFFFFF", high="#4C0000FF", na.value = "#2C0000FF", 
                         limits = c(0,0.2), midpoint = 0, breaks=c(0,0.1,0.2,0.3,0.4)) +
    scale_y_discrete(name ="Outcome scenario", limits = c("VI","V","IV","III","II","I")) +
    scale_x_discrete(name ="Missingness scenario") +
    theme_classic() + theme(legend.key.size = unit(0.3, "cm"), legend.key.width = unit(4, "cm")) +
    theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5), 
          plot.caption = element_text(hjust = 1, size = 20), legend.position = "bottom") +
    facet_grid(Method ~ scenario, scales = "free", space = "free", drop = TRUE, labeller = labeller(outcome = label_wrap_gen(10))) +
    theme(strip.text.y = element_text(angle = 0)) 
}
MSE(res.10) %>% `+`(labs(subtitle = "Low exposure prevalence (10%)")) %>% add.label.T.R(.) 
MSE(res.50) %>% `+`(labs(subtitle = "Moderate exposure prevalence (50%)")) %>% add.label.T.R(.)

