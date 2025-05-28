
rm(list=ls())
gc();gc();

install.packages("data.table")
library(data.table)
library(sf)
library(dplyr)
library(ggplot2)

filename <- c("愛知県・三重県","岡山・広島","岩手県","岐阜県・静岡県",
              "宮城県","熊本・大分・宮崎","高知・佐賀・長崎","埼玉県",
              "山口・徳島・香川・愛媛","滋賀県・京都府","鹿児島・沖縄",
              "秋田県・山形県","新潟県・富山県・石川県","神奈川県","青森県",
              "千葉県","大阪府","長野県","東京都","栃木県・群馬県","福井県・山梨県",
              "福岡県","福島県・茨城県","兵庫県・奈良県","北海道_市区","北海道_町村",
              "北海道_町村2","和歌山・鳥取・島根","欠損分")

table_list <-list()
for(ii in 1:length(filename)){
  file_path <- paste0("NITAS/", filename[ii],".res")
  
  table <- read.table(file_path,header = F,fileEncoding="Shift_JIS",sep=",")
  
  table_fil <- table[,c("V2","V4","V8","V9","V13","V29")]
  colnames(table_fil) <- c("suc","dep","code.x","des","code.y","total")
  table_fil2 <- table_fil[table_fil$suc==0,]
  table_list[[ii]] <- table_fil2
}
table_data <- do.call(rbind,table_list)　%>% distinct()


table_data <- table_data %>% filter(!(code.x %in% c("46303","46304","47381","47207"))) %>% 
  filter(!(code.y %in% c("46303","46304","47381","47207"))) 
table_data <- table_data[,-1]

#同起終点のリスト
shicode <- read.csv("gove_list.csv")
shicode <- na.omit(shicode)
shicode <- shicode[,c("検索名","市区町村コード")]
for (ii in 1:length(shicode[["市区町村コード"]])){ 
  if (nchar(shicode[[ii,"市区町村コード"]]) == 4) {
    shicode[[ii,"市区町村コード"]] <- paste0("0", shicode[[ii,"市区町村コード"]])
  }
}

zero <- rep(0,length(shicode))
data0 <- data.frame(
  des=shicode[["検索名"]],
  code.x=shicode[["市区町村コード"]],
  dep=shicode[["検索名"]],
  code.y=shicode[["市区町村コード"]],
  total=zero
) %>% filter(!(code.x %in% c("46303","46304","47381","47207"))) %>% 
  filter(!(code.y %in% c("46303","46304","47381","47207")))
data <- rbind(table_data,data0)

shicode_kind <- data$code.x %>% unique() %>% sort()

#市区町村別従業者数
load("SynD3.for.esrimap.xdr")
load("TFP.xdr")

SynD15=select(SynD3,contains("15"),contains("MUN"))
SynD_kind <- SynD15$MUN %>% unique() %>% sort()

EM_list <- list()
for(ii in 1:length(SynD_kind)){ #ii=1
  code <- SynD_kind[ii]
  SynD15_fil <- SynD15[SynD15$MUN == code,] %>% mutate(EM.15 = BE.15*em.15)
  EM.total <- sum(SynD15_fil$EM.15)
  EM_list[[ii]] <- data.frame(
    code = code,
    EM = EM.total
  ) 
}
EM_data <- do.call(rbind,EM_list)

#アクセシビリティ計算
data_EM_shicode <- table_data %>% left_join(EM_data,by=c("code.y"="code")) %>%na.omit()

data_EM_shicode$total <- as.numeric(data_EM_shicode$total)

AC_value_list <- list()
for(ii in 1:length(shicode_kind)){#ii=3
  
  if( shicode_kind[ii] %in% EM_data$code ){
    data_fil <- data_EM_shicode[data_EM_shicode$code.x == shicode_kind[ii],]
    #EM <- EM_data %>% filter(code==shicode_kind[ii])
    #emv = sum(SynD15$BE.15*SynD15$em.15)
    AC_value <- sum(data_fil$EM*exp(-1*data_fil$total/60))
    #for(jj in 1:nrow(EM_data))
    #A <- EM_data$EM
    #ac <-  A*B
    #AC_value <- sum(ac)
    
    AC_value_list[[ii]] <- data.frame(
      code = shicode_kind[ii],
      Accessibility = AC_value
    )
  }
  else{
    AC_value_list[[ii]] <- data.frame(
      code = shicode_kind[ii],
      Accessibility = NA
    )
  }
  
}


AC_value_data <- do.call(rbind,AC_value_list) %>% na.omit()
AC_value_data$Accessibility[AC_value_data$Accessibility <= 0] <- NA 
AC_value_data <- na.omit(AC_value_data)

#load("TFP.xdr")

TFP_AC <- list()
for(ii in 1:15){
  TFP_AC[[ii]] <- TFP[[ii]] %>% left_join(AC_value_data,by = c("MUN"="code")) %>% na.omit()
}

View(TFP_AC[[4]])

TFP_AC_plot <- list()
for(ii in 1:15){#ii=12
mean_value_4 <- mean(TFP_AC[[ii]]$Accessibility)
var_value_4 <- sd(TFP_AC[[ii]]$Accessibility)

# 上限と下限を設定
upper_bound_4 <- mean_value_4 + 2*var_value_4

tmpM=cbind(TFP_AC[[ii]]$Accessibility,TFP_AC[[ii]]$TFP) %>% na.omit()
cor(tmpM)
hist((TFP_AC[[ii]]$Accessibility))
 hist(log10(TFP_AC[[ii]]$Accessibility))
tmpM=cbind(log10(TFP_AC[[ii]]$Accessibility+1),TFP_AC[[ii]]$TFP) %>% na.omit()
tmpM=tmpM[which(tmpM[,2]>0),]
tmpM=tmpM[which(tmpM[,2]>-1 & tmpM[,2]<2),]
cor(tmpM)
cor.test(tmpM[,1],tmpM[,2])
#plot(tmpM)


plot <- ggplot(data = TFP_AC[[ii]],aes(x=Accessibility,y=TFP))+
  geom_point()+
  #coord_cartesian(ylim = c(-1, 1))+
  labs(x="アクセシビリティ値",y="TFP")+
  theme(
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
  )
plot <- plot + scale_x_log10()
TFP_AC_plot[[ii]] <- plot
}
TFP_AC_plot[[12]]
tdir3
ggsave("宿泊業.png", plot = TFP_AC_plot[[11]], width = 6, height = 4, dpi = 1500)
