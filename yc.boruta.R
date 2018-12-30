#-------------------
# Feature Reduction
#-------------------
if (!require('Boruta')) install.packages('Boruta'); library(Boruta)

train.imp <- read.csv('data/yc.train.imp.csv')[-1]
#names(train.imp)

# Feature Selection
set.seed(0-0)
write(format(Sys.time(), "%a %b %d %Y %X"),'blog.post/2.boruta.selected.features.txt')
train.boruta <- Boruta(SalePrice~., data=train.imp, doTrace=2, maxRuns=500)
write(format(Sys.time(), "%a %b %d %Y %X"),'blog.post/2.boruta.selected.features.txt',append=TRUE)

print(train.boruta)
jpeg('blog.post/2.boruta.selected.features.jpeg', width=1280,height=720)
plot(train.boruta, las=2, cex.axis=0.7, xlab='', main='Feature Importance by Boruta');
dev.off()
#plotImpHistory(boruta)

train.boruta.fix <- TentativeRoughFix(train.boruta)
train.boruta.selected.features <-  getSelectedAttributes(train.boruta.fix, withTentative = F)
write(train.boruta.selected.features,'blog.post/2.boruta.selected.features.txt',append=TRUE)

saveRDS(train.boruta.selected.features,
        'boruta/train.boruta.selected.features.rds')

train.boruta.selected.features.stats <- attStats(train.boruta.fix)
write(train.boruta.selected.features.stats,'blog.post/2.boruta.selected.features.txt',append=TRUE)

saveRDS(train.boruta.selected.features.stats,
        'boruta/train.boruta.selected.features.stats.rds')
