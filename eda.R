library(corrplot)


data=read.csv("final_2024_player.csv")

data=data[, !(names(data) %in% c("Salary", "Guaranteed", "Next_Year_Guaranteed", "Awards"))]

data=na.omit(data)

data$accolade=as.numeric(data$All.Star+data$AwardWinner+data$FirstTeam+data$SecondTeam+data$ThridTeam+data$DefTeam1+data$DefTeam2+data$NumOfAwards>0)

names(data)[61]="ContractYear"

box_score_vars=c("Age", "Pos", "G", "MP_x", "PTS", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV.", "FG.", "X3P.", "FT.", "X2P.")


box_score_vars=c("Age", "Pos", "G", "MP_x", "PTS", "TRB", "AST", "STL", "BLK", "TOV.", "FG.", "X3P.", "FT.", "X2P.")

num_box_score_vars=c("Age", "G", "MP_x", "PTS", "TRB", "AST", "STL", "BLK", "TOV.", "FG.", "X3P.", "FT.", "X2P.")

advanced_vars=c("Age", "PER", "TS.", "eFG.", "VORP", "WS", "BPM", "OBPM", "DBPM", "USG.", "FTr", "accolade", "ContractYear")


cor_adv=cor(data[advanced_vars], use = "complete.obs")

corrplot(cor_adv, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)

cor_box=cor(data[num_box_score_vars], use="complete.obs")

corrplot(cor_box, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)
