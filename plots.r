library(ggplot2)
library(gridExtra)

dados<-dados_TUDO[c(1:5, 8, 11, 18:20, 24:25)]
head(dados)

colnames(dados) <- c("display", "Corte", "Poleiro", "RR", "DET", "DLEntr", "LAM","MCEntr", "r", "No_males", "Cop_rate", "Fem_rate")    
colnames(dados)

part_1 <- as.matrix(cbind(dados[c(4:12)])) 
head(part_1)
part_2 <- as.matrix(cbind(dados[-c(4:12)])) 
head(part_2)
part_3 <- as.matrix(cbind(dados[c(12:13)])) 
head(part_3)
part_1_1 <- as.matrix(cbind(dados[c(4:12)])) 
head(part_1_1)

pairs.panels(part_1_1, method = "spearman")

# heatmap
part_1_1

heatmapCor <- heatmaply_cor(cor(normalize(part_1)),
                   show_dendrogram = c(F, F),
                   key.title = NULL,
                   fontsize_row = 20,
                   fontsize_col = 20,
                   cellnote = round(cor(normalize(part_1), method = "spearman"), 2))

heatmapCor

### Number of males in the display ####
## DLEntr ####
N_machos_plot_dl <- ggplot() +  
  geom_point(data = dados, aes(N_machos, dl_entropy), size = 5) +
  geom_line(data =x_N_machos_dl, aes(x=N_machos, y=fit), color="red", size = 1.5) +
  geom_ribbon(data =x_N_machos_dl, aes(x=N_machos, ymin=lower, ymax=upper), alpha= 0.7, fill="gray")+
  labs(x="No of males", y = "\nDiagonal entropy") +
  ggtitle("a")+
  theme_classic() +
  theme(plot.title = element_text(size = 60)) +
  theme(axis.title = element_text(size=60, face="bold")) + 
  theme(axis.text.x=element_text(size=50,face="bold", color="black")) +
  theme(axis.text.y=element_text(size=50,face="bold", color="black"))
N_machos_plot_dl

## S Vector ####
N_machos_plot_sv <- ggplot() +  
  geom_point(data = dados, aes(N_machos, S_vector), size = 5) +
  geom_line(data =x_N_machos_sv, aes(x=N_machos, y=fit), color="red", size = 1.5) +
  geom_ribbon(data =x_N_machos_sv, aes(x=N_machos, ymin=lower, ymax=upper), alpha= 0.7, fill="gray")+
  labs(x="No of males", y = "\nMicrostate entropy") +
  ggtitle("b")+
  theme_classic() +
  theme(plot.title = element_text(size = 60)) +
  theme(axis.title = element_text(size=60, face="bold")) + 
  theme(axis.text.x=element_text(size=50,face="bold", color="black")) +
  theme(axis.text.y=element_text(size=50,face="bold", color="black"))
N_machos_plot_sv

png(filename = "FIG_males.png",
    width = 2000, height = 900, units = "px",  pointsize = 12,
    bg = "white", res = NA, family = "")
grid.arrange(N_machos_plot_dl, N_machos_plot_sv,  ncol=2, nrow=1)
dev.off()

### Visitation Rate ####
## Determionism ####
determinism_plot <- ggplot() +  
  geom_point(data = dados, aes(determinism, fem_hora), size = 5) +
  geom_point(data = x_determinism, aes(x=determinism, y=fit), color="black", size = 5) +
  geom_line(data =x_determinism, aes(x=determinism, y=fit), color="red", size = 1.5) +
  geom_ribbon(data =x_determinism, aes(x=determinism, ymin=lower, ymax=upper), alpha= 0.7, fill="gray")+
  labs(x="Determinism", y = "\nVisitation rate") +
  ggtitle("a")+
  theme_classic() +
  theme(plot.title = element_text(size = 45)) +
  theme(axis.title = element_text(size=45, face="bold")) + 
  theme(axis.text.x=element_text(size=30,face="bold", color="black")) +
  theme(axis.text.y=element_text(size=30,face="bold", color="black"))
determinism_plot

## Diagonal entropy ####
dl_entropy_plot <- ggplot() +  
  geom_point(data = dados, aes(dl_entropy, fem_hora), size = 5) +
  geom_point(data = x_dl_entropy, aes(x=dl_entropy, y=fit), color="black", size = 5) +
  geom_line(data =x_dl_entropy, aes(x=dl_entropy, y=fit), color="red", size = 1.5) +
  geom_ribbon(data =x_dl_entropy, aes(x=dl_entropy, ymin=lower, ymax=upper), alpha= 0.7, fill="gray")+
  labs(x="Diagonal entropy", y = "\nVisitation rate") +
  ggtitle("b")+
  theme_classic() +
  theme(plot.title = element_text(size = 45)) +
  theme(axis.title = element_text(size=45, face="bold")) + 
  theme(axis.text.x=element_text(size=30,face="bold", color="black")) +
  theme(axis.text.y=element_text(size=30,face="bold", color="black"))
dl_entropy_plot

## Laminarity ####
laminarity_plot <- ggplot() +  
  geom_point(data = dados, aes(laminarity, fem_hora), size = 5) +
  geom_point(data = x_laminarity, aes(x=laminarity, y=fit), color="black", size = 5) +
  geom_line(data =x_laminarity, aes(x=laminarity, y=fit), color="red", size = 1.5) +
  geom_ribbon(data =x_laminarity, aes(x=laminarity, ymin=lower, ymax=upper), alpha= 0.7, fill="gray")+
  labs(x="Laminarity", y = "\nVisitation rate") +
  ggtitle("c")+
  theme_classic() +
  theme(plot.title = element_text(size = 45)) +
  theme(axis.title = element_text(size=45, face="bold")) + 
  theme(axis.text.x=element_text(size=30,face="bold", color="black")) +
  theme(axis.text.y=element_text(size=30,face="bold", color="black"))
laminarity_plot

png(filename = "FIG_visit.png",
    width = 1600, height = 1400, units = "px",  pointsize = 12,
    bg = "white", res = NA, family = "")
grid.arrange(determinism_plot, dl_entropy_plot, laminarity_plot,  ncol=2, nrow=2)
dev.off()

### Copulation ####
dados$cop<-round((dados$cop_hora)*10,0)

## Determinism ####
determinism_plot <- ggplot() +  
  geom_point(data = dados, aes(determinism, cop), size = 5) +
  geom_line(data =x_determinism, aes(x=determinism, y=fit+se), color="red", size = 1.5) +
  geom_ribbon(data =x_determinism, aes(x=determinism, ymin=fit, ymax=(fit+se+se)), alpha= 0.7, fill="gray") +
  labs(x="Determinism", y = "\nCopulation rate") +
  ggtitle("a")+
  theme_classic() +
  theme(plot.title = element_text(size = 60)) +
  theme(axis.title = element_text(size=60, face="bold")) + 
  theme(axis.text.x=element_text(size=50,face="bold", color="black")) +
  theme(axis.text.y=element_text(size=50,face="bold", color="black"))
determinism_plot

## Laminarity ####
laminarity_plot <- ggplot() +  
  geom_point(data = dados, aes(laminarity, cop), size = 5) +
  geom_line(data =x_laminarity, aes(x=laminarity, y=fit+se), color="red", size = 1.5) +
  labs(x="Laminarity", y = "\nCopulation rate") +
  geom_ribbon(data =x_laminarity, aes(x=laminarity, ymin=fit, ymax=(fit+se+se)), alpha= 0.7, fill="gray")+
  ggtitle("b")+
  theme_classic() +
  theme(plot.title = element_text(size = 60)) +
  theme(axis.title = element_text(size=60, face="bold")) + 
  theme(axis.text.x=element_text(size=50,face="bold", color="black")) +
  theme(axis.text.y=element_text(size=50,face="bold", color="black"))
laminarity_plot 

png(filename = "FIG_cop.png",
    width = 2000, height = 900, units = "px",  pointsize = 12,
    bg = "white", res = NA, family = "")

grid.arrange(determinism_plot, laminarity_plot,  ncol=2, nrow=1)

dev.off()