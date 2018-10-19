
require(dplyr)
require(sits.validate)

dir = "C:/Users/pedro/Dropbox/pesquisa/2015/TWDTWAmazoniaCerrado/"
mt2001 = paste0(dir, "Classificacoes/MATO_GROSSO_9classes_SVM_Smooth/sample_reduction_smooth_10_3x3/SR_sooth_10_3x3_mask/mt_2001_v3.tif")
mt2015 = paste0(dir, "Classificacoes/MATO_GROSSO_9classes_SVM_Smooth/sample_reduction_smooth_10_3x3/SR_sooth_10_3x3_mask/mt_2015_v3.tif")

lapig2015 = "lapig-resample.tif"

# Resample Lapig
#lapig = raster::raster(paste0(dir, "MapasReferencia/Pastagem/MT_sinu/pastagens_2015_lapig_mt.tif"))
#raster::resample(lapig, raster::raster(mt2015), method = "ngb", filename = lapig2015, progress = "text")

hansen2000 = "hansen-resample.tif"

# Resample Hansen
#hansen = raster::raster(paste0(dir, "MapasReferencia/Hansen-GlobalForestWatch/sinu/Hansen_GFC2017_treecover2000_MT_reclass.tif"))
#raster::resample(hansen, raster::raster(mt2015), method = "ngb", filename = hansen2000, progress = "text")

res = sits.validate::compareRasters(mt2015, lapig2015)
res
write.table(res, "diff-lapig-vs-sits.csv", sep=",")

res = sits.validate::compareRasters(mt2001, hansen2000)
res
write.table(res, "diff-hansen-vs-sits.csv", sep=",")
