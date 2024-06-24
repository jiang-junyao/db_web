# library 加载----
library(shiny)
library(ggiraph)
library(readxl)
library(pheatmap)
library(RColorBrewer)
library(reactable)
library(htmltools)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyWidgets)
library(plotly)
library(shinythemes)
library(stringr)
library(sortable)
library(shinydashboard)
library(heatmaply)
library(shinyBS)
library(bslib)
#----

# 控制用户文件上传最大为2G
options(shiny.maxRequestSize=1024*1024^2)

col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))

# 定义自定义颜色custom_colors
custom_colors <- c(
  "#BDBDBD", "#8DB5CE", "#542E8B", "#D7BD90", "#C82129", "#F296C0", "#647950", "#114B9B", "#FBBD90", "#193E51", "#BE93BF")

getPalette = colorRampPalette(custom_colors)

# 设置颜色向量cols----
cols <- c("OrangeRed","SlateBlue3","DarkOrange","GreenYellow","Purple",
          "DarkSlateGray","Gold","DarkGreen","DeepPink2","Red4","#4682B4",
          "#FFDAB9","#708090","#836FFF","#CDC673","#CD9B1D","#FF6EB4","#CDB5CD"
          ,"#008B8B","#43CD80","#483D8B","#66CD00","#CDC673","#CDAD00","#CD9B9B"
          ,"#FF8247","#8B7355","#8B3A62","#68228B","#CDB7B5","#CD853F","#6B8E23"
          ,"#696969","#7B68EE","#9F79EE","#B0C4DE","#7A378B","#66CDAA","#EEE8AA"
          ,"#00FF00","#EEA2AD","#A0522D","#000080","#E9967A","#00CDCD","#8B4500"
          ,"#DDA0DD","#EE9572","#EEE9E9","#8B1A1A","#8B8378","#EE9A49","#EECFA1"
          ,"#8B4726","#8B8878","#EEB4B4","#C1CDCD","#8B7500","#0000FF","#EEEED1"
          ,"#4F94CD","#6E8B3D","#B0E2FF","#76EE00","#A2B5CD","#548B54","#BBFFFF"
          ,"#B4EEB4","#00C5CD","#008B8B","#7FFFD4","#8EE5EE","#43CD80","#68838B"
          ,"#00FF00","#B9D3EE","#9ACD32","#00688B","#FFEC8B","#1C86EE","#CDCD00"
          ,"#473C8B","#FFB90F","#EED5D2","#CD5555","#CDC9A5","#FFE7BA","#FFDAB9"
          ,"#CD661D","#CDC5BF","#FF8C69","#8A2BE2","#CD8500","#B03060","#FF6347"
          ,"#FF7F50","#CD0000","#F4A460","#FFB5C5","#DAA52")
#----

# # 创建clone_fate_bias_list----
# clone_fate_bias_list <- list()
# rds_files <- list.files("clone_fate_bias", pattern = "\\.rds$", full.names = TRUE)
# for (file_path in rds_files) {
#   var_name <- sub("\\.rds$", "", basename(file_path))
#   data_list <- readRDS(file_path)
#   clone_names <- lapply(data_list, function(df) df$clone_name)
#   clone_fate_bias_list[[var_name]] <- clone_names
#   rm(var_name,data_list,clone_names,file_path)
# }
# rm(rds_files)
#
# #加上--bias后缀
# clone_fate_bias_list <- lapply(clone_fate_bias_list, function(df) {
#   names(df) <- paste(names(df), '--bias')
#   return(df) })
#
# # 清理 pbmc 数据的函数
# clean_pbmc_data <- function(pbmc) {
#   pbmc %>%
#     dplyr::filter(!is.na(celltype) & !grepl("^\\s*$", celltype) &
#              !barcodes %in% c('Negative', 'None') & !is.na(barcodes) & !grepl("^\\s*$", barcodes)) %>%
#     mutate(celltype = as.character(celltype), barcodes = as.character(barcodes)) }
#
# #针对所有数据集加入No bias的barcodes
# for (name in names(obj_metadata_list)) {
#   pbmc <- clean_pbmc_data(obj_metadata_list[[name]][, c('celltype', 'barcodes')])
#   clean_barcodes <- unique(pbmc$barcodes)
#
#   # 对应的 clone_fate_bias_list 中的列表
#   corresponding_list <- clone_fate_bias_list[[name]]
#   if (is.null(corresponding_list)) {
#     corresponding_list <- list()
#   }
#   all_barcodes <- unlist(corresponding_list)  # 假设需要比较的条码已经在列表的各个元素中
#
#   # 查找不在对应列表中的条码
#   unique_barcodes <- setdiff(clean_barcodes, all_barcodes)
#
#   # 如果有独特的条码，将其添加到 'No bias' 子列表中
#   if (length(unique_barcodes) > 0) {
#     if (!"No bias" %in% names(corresponding_list)) {
#       corresponding_list[["No bias"]] <- vector("list", length(unique_barcodes))
#     }
#     corresponding_list[["No bias"]] <- unique_barcodes
#   }
#   # 更新原列表
#   clone_fate_bias_list[[name]] <- corresponding_list
#   rm(pbmc,clean_barcodes,corresponding_list,all_barcodes,unique_barcodes,name) }
#
# #定义一个按barcode出现次数进行排序的函数
# reorder_barcodes <- function(pbmc, corresponding_list) {
#   reordered_lists <- list()
#
#   for (i in names(corresponding_list)) {
#     barcodes_vector <- corresponding_list[[i]]
#     counts <- pbmc %>%
#       dplyr::filter(barcodes %in% barcodes_vector) %>%
#       count(barcodes) %>%
#       arrange(desc(n))  # 按出现次数降序排序
#
#     reordered_barcodes <- counts$barcodes
#     reordered_lists[[i]] <- reordered_barcodes
#   }
#   return(reordered_lists) }
#
#
# #按barcodes频率对clone_fate_bias_list进行排序
# for (name in names(obj_metadata_list)) {
#   pbmc <- clean_pbmc_data(obj_metadata_list[[name]][, c('celltype', 'barcodes')])
#   q_barcodes = clone_fate_bias_list[[name]]
#   q_barcodes_q <- reorder_barcodes(pbmc,q_barcodes)
#   clone_fate_bias_list[[name]] = q_barcodes_q
#   rm(pbmc,q_barcodes,q_barcodes_q,name) }
# #----

# 清理 pbmc 数据的函数clean_pbmc_data----
clean_pbmc_data <- function(pbmc) {
  pbmc %>%
    dplyr::filter(!is.na(celltype) & !grepl("^\\s*$", celltype) &
             !barcodes %in% c('Negative', 'None') & !is.na(barcodes) & !grepl("^\\s*$", barcodes)) %>%
    mutate(celltype = as.character(celltype), barcodes = as.character(barcodes)) }
#----


# 读取所有celltype__fate_use文件----
subfolders <- list.files(path = "clone_fate_bias_deg/", full.names = TRUE, recursive = FALSE)
celltype__fate_use_list <- list()
for (folder in subfolders) {
  file_path <- file.path(folder, "celltype__fate_use.rds")
    data <- readRDS(file_path)
    folder_name <- basename(folder)
    celltype__fate_use_list[[folder_name]] <- data
    rm(folder,file_path,data,folder_name)
}
rm(subfolders)
#----

# 读取所有celltype__fate_use_size文件----
subfolders <- list.files(path = "celltype_clone_deg/", full.names = TRUE, recursive = FALSE)
celltype__fate_use_size_list <- list()
for (folder in subfolders) {
  file_path <- file.path(folder, "celltype__fate_use.rds")
  data <- readRDS(file_path)
  folder_name <- basename(folder)
  celltype__fate_use_size_list[[folder_name]] <- data
  rm(folder,file_path,data,folder_name)
}
rm(subfolders)
#----

# # 读取所有deg.rds文件----
#  # 列出所有一级子文件夹
# subfolders <- list.dirs(path = "clone_fate_bias_deg", full.names = TRUE, recursive = FALSE)
#  # 为每个子文件夹构建期望的 .rds 文件路径
# rds_files <- sapply(subfolders, function(folder) {
#   file_path <- file.path(folder, paste0(basename(folder), ".rds"))
#   if (file.exists(file_path)) { return(file_path) } else { return(NA) } })
#  # 去掉不存在的文件路径
# rds_files <- rds_files[!is.na(rds_files)]
#  # 读取所有 .rds 文件并将它们存储到一个 list 中
#  # 使用文件名（去掉扩展名和路径）作为 list 的元素名
# #[c(12,25,59)]
# deg_rds_list <- lapply(rds_files, readRDS)
# names(deg_rds_list) <- sapply(rds_files, function(x) tools::file_path_sans_ext(basename(x)))
# #----

# # 读取所有deg_size.rds文件----
#  # 列出所有一级子文件夹
# subfolders <- list.dirs(path = "celltype_clone_deg", full.names = TRUE, recursive = FALSE)
#  # 为每个子文件夹构建期望的 .rds 文件路径
# rds_files <- sapply(subfolders, function(folder) {
#   file_path <- file.path(folder, paste0(basename(folder), ".rds"))
#   if (file.exists(file_path)) { return(file_path) } else { return(NA) } })
#  # 去掉不存在的文件路径
# rds_files <- rds_files[!is.na(rds_files)]
#  # 读取所有 .rds 文件并将它们存储到一个 list 中
#  # 使用文件名（去掉扩展名和路径）作为 list 的元素名
# #[c(13,26,60)]
# deg_size_rds_list <- lapply(rds_files, readRDS)
# names(deg_size_rds_list) <- sapply(rds_files, function(x) tools::file_path_sans_ext(basename(x)))
# #----

#Read and prepare 'scLTdb summary.xlsx' ----
coretable <- readxl::read_xlsx('scLTdb summary.xlsx') %>%
  dplyr::select(1:9) %>%
  mutate(Species = as.factor(Species),
         `Tissue source` = as.factor(`Tissue source`),
         Technology = as.factor(Technology),
         `Barcode type` = as.factor(`Barcode type`))

data_summary_table <- readxl::read_xlsx('scLTdb summary.xlsx') %>%
  dplyr::select(2,10:13)

#----

# Read and prepare 'scLTdb download.xlsx' ----
coretable_download <- readxl::read_xlsx('scLTdb download.xlsx') %>%
  mutate(Dataset = as.factor(Dataset),
         Species = as.factor(Species),
         `Tissue source` = as.factor(`Tissue source`),
         Technology = as.factor(Technology),
         `Barcode type` = as.factor(`Barcode type`),
         H5ad_data_download = as.character(H5ad_data_download),
         R_data_download = as.character(R_data_download),
         `Fate bias DEGs/DBRs download` = as.character(`Fate bias DEGs/DBRs download`),
         `Clone DEGs/DBRs download` = as.character(`Clone DEGs/DBRs download`))

coretable1 = coretable_download
coretable1$R_data_download <- sapply(coretable1$R_data_download, function(x) {
  if(startsWith(x, "http")) { paste0('<a href="', x, '" target="_blank">Download data</a>') } else { x } })

coretable1$H5ad_data_download <- sapply(coretable1$H5ad_data_download, function(x) {
  if(startsWith(x, "http")) { paste0('<a href="', x, '" target="_blank">Download data</a>') } else { x } })

coretable1$`Fate bias DEGs/DBRs download` <- sapply(coretable1$`Fate bias DEGs/DBRs download`, function(x) {
  if(startsWith(x, "http")) { paste0('<a href="', x, '" target="_blank">Download data</a>') } else { x } })

coretable1$`Clone DEGs/DBRs download` <- sapply(coretable1$`Clone DEGs/DBRs download`, function(x) {
  if(startsWith(x, "http")) { paste0('<a href="', x, '" target="_blank">Download data</a>') } else { x } })

coretable1 <- as.data.frame(coretable1)
rownames(coretable1) <- NULL
#----

# pseudotime颜色函数
blues_palette <- brewer.pal(9, "Blues")

# dataumap函数,用来画celltype的svg图----
dataset_umap <- function( pbmc, page_number ){

  pbmc$celltype = as.character(pbmc$celltype)
  pbmc$barcodes = as.character(pbmc$barcodes)
  pbmc = pbmc[!is.na(pbmc$celltype),]
  pbmc <- pbmc[!grepl("^\\s*$", pbmc$celltype), ]
  if (nrow(pbmc) > 20000) {
    set.seed(123)
    pbmc <- pbmc[sample(1:nrow(pbmc), 20000), ]
  }
  if (nrow(pbmc)>10000) { cell_size=0.15 }else{ cell_size=0.2 }

  pbmc$celltype <- str_wrap(pbmc$celltype, width = 20)
  all_celltype = unique(pbmc$celltype)
  start <- (page_number - 1) * 12 + 1
  end <- min(start + 11, length(all_celltype))
  levels_to_show <- all_celltype[start:end]

  palette <- setNames( getPalette( length(all_celltype) ), all_celltype )

  #pbmc$tooltip = paste0("Cell name: ", rownames(pbmc), "\nCell type: ", pbmc$celltype , "\nCell barcode: ", pbmc$barcodes , "\nUMAP coordinates: ", pbmc$UMAP_1, ", ", pbmc$UMAP_2)

  gg <- ggplot() +
    geom_point(data = pbmc,  aes(x = UMAP_1, y = UMAP_2, color = celltype ), size = cell_size ) +
    theme_void() +
    scale_color_manual(values = palette[all_celltype],limits = all_celltype, breaks = levels_to_show) +
    labs(color = "Cell type (state)")+
    theme(legend.position = "right",
          legend.key.height = unit(2, "lines") )+
    guides(color = guide_legend(override.aes = list(size = 3)))

   girafe(ggobj = gg,options = list( opts_selection(type = "none"),opts_tooltip(offx = 0, offy = 0) ), width_svg = 8, height_svg = 6 )
}
#----

# # 生成pseudotime_svg_list----
# subfolders <- list.files(path = "pseudotime_new", full.names = TRUE, recursive = FALSE)
# pseudotime_svg_list <- list()
# for (folder in subfolders) {
#   data <- read.csv(folder,row.names = 1)
#   folder_name <- basename(folder)
#   aaa <- gsub("\\.csv$", "", folder_name)
#   meta_use = obj_metadata_list[[aaa]][,c('UMAP_1', 'UMAP_2', 'celltype', 'barcodes')]
#
#   # 检查行数是否相同
#   if (nrow(meta_use) == nrow(data)) {  meta_use <- cbind(meta_use, data) } else {
#     meta_use <- meta_use[!is.na(meta_use$celltype), ]
#     meta_use = cbind(meta_use, data) }
#
#   if (nrow(meta_use)>10000) { cell_size=0.5 }else{ cell_size=1 }
#
#   meta_use$tooltip = paste0("Cell name: ", rownames(meta_use), "\nCell type: ", meta_use$celltype , "\nCell barcode: ", meta_use$barcodes , "\nUMAP coordinates: ", meta_use$UMAP_1, ", ", meta_use$UMAP_2, "\nPseudotime: ", meta_use$pseudotime )
#   meta_use <- meta_use[order(meta_use$pseudotime, decreasing = FALSE), ]
#   gg <- ggplot() +
#     geom_point_interactive(data = meta_use,  aes(x = UMAP_1, y = UMAP_2, color = pseudotime, tooltip = tooltip ), size = cell_size ) +
#     theme_void() +
#     scale_color_gradientn(colors = blues_palette )
#
#   p <- girafe(ggobj = gg,options = list( opts_sizing(rescale = TRUE)), width_svg = 8, height_svg = 6 )
#
#   pseudotime_svg_list[[aaa]] <- p
#   rm(data,folder_name,aaa,meta_use,gg,p)
# }
# rm(subfolders)
# #----

# 生成 pseudotime_svg 的函数generatePseudotimeSvg----
generatePseudotimeSvg <- function(dataset_name, metadata ) {

  file_path <- paste0("pseudotime_new/", dataset_name, ".csv")
  data <- read.csv(file_path, row.names = 1)
  meta_use <- metadata[, c('UMAP_1', 'UMAP_2', 'celltype', 'barcodes')]

  if (nrow(meta_use) == nrow(data)) {
    meta_use <- cbind(meta_use, data)
  } else {
    meta_use <- meta_use[!is.na(meta_use$celltype), ]
    meta_use <- cbind(meta_use, data)
  }
  if (nrow(meta_use) > 20000) {
    set.seed(123)
    meta_use <- meta_use[sample(1:nrow(meta_use), 20000), ]
  }
  cell_size <- ifelse(nrow(meta_use) > 10000, 0.15, 0.2)

  #meta_use$tooltip <- paste0("Cell name: ", rownames(meta_use), "\nCell type: ", meta_use$celltype, "\nCell barcode: ", meta_use$barcodes, "\nUMAP coordinates: ", meta_use$UMAP_1, ", ", meta_use$UMAP_2, "\nPseudotime: ", meta_use$pseudotime)
  inf_indices <- is.infinite(meta_use$pseudotime)
  if (any(inf_indices)) {
    max_non_inf <- max(meta_use$pseudotime[!inf_indices], na.rm = TRUE)
    meta_use$pseudotime[inf_indices] <- max_non_inf
  }
  meta_use <- meta_use[order(meta_use$pseudotime, decreasing = FALSE), ]

  gg <- ggplot() +
    geom_point(data = meta_use, aes(x = UMAP_1, y = UMAP_2, color = pseudotime), size = cell_size) +
    theme_void() +
  #  scale_color_gradientn(colors = blues_palette)+
    scale_color_viridis_c()

  girafe(ggobj = gg, options = list( opts_selection(type = "none"),opts_tooltip(offx = 0, offy = 0) ), width_svg = 8, height_svg = 6)
}
#----

# 根据用户的行数锚定到数据集上的getSelectedDataset常用函数----
#getSelectedDataset <- function(selected_rows=NULL, data_table){ if (!is.null(selected_rows)) { return(data_table[selected_rows, ]$Dataset) } else { return('Biddy_2018_Nature') } }
#----

# 绘制clone的函数----
plot_clone_embedding2 <- function(barcode_use, pbmc){
  if(length(barcode_use) > 5){
    p <- ggplot() +
      annotate("text", x = 1, y = 1, label = "The number of selected clones cannot exceed 5", size = 5) +
      theme_void()
     return(girafe(ggobj = p,options = list(
       opts_sizing(rescale = TRUE),  # 允许图形随容器大小变化
       opts_tooltip(offx = 0, offy = 0)
     ) ))
  }
  
 # colors = c( rgb(205/255,205/255,205/255),rgb(42/255,122/255,155/255) )
 # getPalette = colorRampPalette( colors )
    all_type=c('Other cell',barcode_use)
    pbmc$celltype = as.character(pbmc$celltype)
    pbmc$barcodes = as.character(pbmc$barcodes)
    barcode_anno = rep('Other cell', nrow(pbmc))
    idx <- which(pbmc$barcodes %in% barcode_use)
    barcode_anno[idx] <- pbmc$barcodes[idx]

    coor = pbmc[, c('UMAP_1', 'UMAP_2', 'celltype', 'barcodes')]
    coor$barcode_type = barcode_anno
    coor$barcode_type <- str_wrap(coor$barcode_type, width = 20)
    all_type <- str_wrap(all_type, width = 20)
   # coor$tooltip = paste0("Cell name: ", rownames(coor), "\nCell type: ", coor$celltype , "\nCell barcode: ", coor$barcodes, "\nChoice: " , coor$barcode_type , "\nUMAP coordinates: ", coor$UMAP_1, ", ", coor$UMAP_2)

    coor1 = coor[coor$barcode_type == 'Other cell',]
    coor2 = coor[coor$barcode_type != 'Other cell',]

    if (nrow(coor1) > 20000) {
      set.seed(123)
      coor1 <- coor1[sample(1:nrow(coor1), 20000), ] }

    if (nrow(coor2) > 20000) {
      set.seed(123)
      coor2 <- coor2[sample(1:nrow(coor2), 20000), ] }

    if (nrow(coor1)>10000) { cell_size_1=0.15 }else{ cell_size_1=0.2 }
    if (nrow(coor2)>10000) { cell_size_2=0.25 }else{ cell_size_2=0.3 }

    if (length(all_type) <= 11) {
      palette_colors <- custom_colors[1:length(all_type)]
    } else {
      palette_colors <- getPalette( length(all_type) )
    }
    gg <- ggplot() +
      geom_point(data = coor1, aes(x = UMAP_1, y = UMAP_2, color = barcode_type ), size = cell_size_1   ) +
      geom_point(data = coor2, aes(x = UMAP_1, y = UMAP_2, color = barcode_type ), size = cell_size_2   ) +
      theme_void() +
      scale_color_manual(values = palette_colors, breaks = all_type) +
      labs(color = "Clonal distribution")+
      theme(legend.position = "right",
            legend.key.height = unit(2, "lines"))

    #ggsave(filename = '/data/yexing/scLT/www/111aaa_plot_clone.pdf', plot = gg, device = "pdf", width = 8, height = 6, units = "in", dpi = 100, limitsize = FALSE)

    girafe(ggobj = gg,options = list( opts_selection(type = "none"),opts_tooltip(offx = 0, offy = 0) ), width_svg = 8, height_svg = 6 )
  }
#----

# 绘制fate bias的clone的函数----
plot_clone_embedding3 <- function( pbmc, fate_use, clone_fate_bias){
 # colors = c( rgb(205/255,205/255,205/255),rgb(42/255,122/255,155/255) )
 # getPalette = colorRampPalette( colors )
    all_type=c('Other cell',fate_use)
    pbmc$celltype = as.character(pbmc$celltype)
    pbmc$barcodes = as.character(pbmc$barcodes)
    barcode_anno = rep('Other cell', nrow(pbmc))
    for(fate in fate_use) {
      barcode_use <- clone_fate_bias[[fate]]
      idx <- which(pbmc$barcodes %in% barcode_use)
      barcode_anno[idx] <- fate }
    coor = pbmc[, c('UMAP_1', 'UMAP_2', 'celltype', 'barcodes')]
    coor$barcode_type = barcode_anno
    coor$barcode_type <- str_wrap(coor$barcode_type, width = 20)
    all_type <- str_wrap(all_type, width = 20)
  #  coor$tooltip = paste0("Cell name: ", rownames(coor), "\nCell type: ", coor$celltype , "\nCell barcode: ", coor$barcodes, "\nChoice: " , coor$barcode_type , "\nUMAP coordinates: ", coor$UMAP_1, ", ", coor$UMAP_2)

    coor1 = coor[coor$barcode_type == 'Other cell',]
    coor2 = coor[coor$barcode_type != 'Other cell',]

    if (nrow(coor1) > 20000) {
      set.seed(123)
      coor1 <- coor1[sample(1:nrow(coor1), 20000), ] }

    if (nrow(coor2) > 20000) {
      set.seed(123)
      coor2 <- coor2[sample(1:nrow(coor2), 20000), ] }

    if (nrow(coor1)>10000) { cell_size_1=0.15 }else{ cell_size_1=0.2 }
    if (nrow(coor2)>10000) { cell_size_2=0.15 }else{ cell_size_2=0.2 }

    if (length(all_type) <= 11) {
      palette_colors <- custom_colors[1:length(all_type)]
    } else {
      palette_colors <- getPalette( length(all_type) )
    }
    gg <- ggplot() +
      geom_point(data = coor1, aes(x = UMAP_1, y = UMAP_2, color = barcode_type ), size = cell_size_1 ) +
      geom_point(data = coor2, aes(x = UMAP_1, y = UMAP_2, color = barcode_type ), size = cell_size_2 ) +
      theme_void() +
      scale_color_manual(values = palette_colors, breaks = all_type) +
      labs(color = "Fate bias")+
      theme(legend.position = "right",
            legend.key.height = unit(2, "lines"))+
      guides(color = guide_legend(override.aes = list(size = 3)))

    #ggsave(filename = '/data/yexing/scLT/www/111aaa_plot_fate_bias.pdf', plot = gg, device = "pdf", width = 8, height = 6, units = "in", dpi = 100, limitsize = FALSE)

    girafe(ggobj = gg,options = list( opts_selection(type = "none"),opts_tooltip(offx = 0, offy = 0) ), width_svg = 8, height_svg = 6 )
  }
#----

# 绘制celltype_numebr_barplot----
plot_celltype_barplot <- function(pbmc) {
  # 计算每种 celltype 的计数
  celltype_counts <- pbmc %>%
    group_by(celltype) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  celltype_counts$celltype <- factor(celltype_counts$celltype,levels = rev(unique(celltype_counts$celltype)) )
  # 获取 unique 的 celltype
  unique_celltypes <- unique(pbmc$celltype)

  # 生成所需数量的颜色，并与 unique 的 celltype 一一对应
  palette_colors <- setNames(getPalette(length(unique_celltypes)), unique_celltypes)

  p <- ggplot(celltype_counts, aes(y = celltype, x = count, fill = celltype)) +
    geom_bar_interactive(stat = "identity", aes(tooltip = paste0("Counts: ", count, "\nCell type (state): ",  celltype), data_id = celltype ) ) +
    scale_fill_manual(values = palette_colors) +
    labs(
      x = "Count",
      y = "Cell type (state)") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # 移除主要网格线
      panel.grid.minor = element_blank(),  # 移除次要网格线
      axis.line = element_line(color = "black"),  # 保留轴线
      legend.title = element_blank(),
      legend.position = "none"
    )

  #ggsave(filename = '/data/yexing/scLT/www/111aaa_plot_barplot.pdf', plot = p, device = "pdf", width = 8, height = 6, units = "in", dpi = 100, limitsize = FALSE)

  # 将 ggplot2 图转换为 girafe 对象
  girafe(ggobj = p, options = list(
    opts_selection(type = "none"),
    opts_hover(css = "stroke:black; stroke-width:2px;"),
    opts_hover_inv(css = "opacity:0.2;"),
    opts_sizing(rescale = TRUE)), width_svg = 8, height_svg = 6)
}

plot_celltype_piechart  <- function(pbmc) {
  celltype_counts <- pbmc %>%
    group_by(celltype) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  celltype_counts <- celltype_counts %>%
    mutate(proportion = count / sum(count))

  unique_celltypes <- unique(pbmc$celltype)
  palette_colors <- setNames(getPalette(length(unique_celltypes)), unique_celltypes)

  p <- ggplot(celltype_counts, aes(x = "", y = proportion, fill = celltype)) +
    geom_bar_interactive(width = 1, stat = "identity", aes(tooltip = paste0("Proportion: ", round(proportion * 100, 2), "%\nCount: ", count, "\nCell type(state): ",celltype), data_id = celltype )) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = palette_colors) +
    theme_void() +
    theme(
      legend.title = element_blank(),
      legend.position = "none"
    )

  #ggsave(filename = '/data/yexing/scLT/www/111aaa_plot_pieplot.pdf', plot = p, device = "pdf", width = 8, height = 6, units = "in", dpi = 100, limitsize = FALSE)

  girafe(ggobj = p, options = list(
    opts_selection(type = "none"),
    opts_hover(css = "stroke:black; stroke-width:2px;"),
    opts_hover_inv(css = "opacity:0.2;"),
    opts_sizing(rescale = TRUE)), width_svg = 8, height_svg = 6)
}

#----


# 绘制unique barcode number in each cell type----
plot_barplot_barcode_1 <- function(pbmc) {
  # 计算每种 celltype 中 unique barcode 的数量
  celltype_counts <- pbmc %>%
    group_by(celltype) %>%
    summarise(unique_barcodes = n_distinct(barcodes)) %>%
    arrange(desc(unique_barcodes))

  celltype_counts$celltype <- factor(celltype_counts$celltype,levels = rev(unique(celltype_counts$celltype)) )
  # 获取 unique 的 celltype
  unique_celltypes <- unique(pbmc$celltype)

  # 生成所需数量的颜色，并与 unique 的 celltype 一一对应
  palette_colors <- setNames(getPalette(length(unique_celltypes)), unique_celltypes)

  p <- ggplot(celltype_counts, aes(y = celltype, x = unique_barcodes, fill = celltype)) +
    geom_bar_interactive(stat = "identity", aes(tooltip = paste0("Unique barcode number: ", unique_barcodes, "\nCell type (state): ",  celltype), data_id = celltype ) ) +
    scale_fill_manual(values = palette_colors) +
    labs(
      x = "Unique barcode number",
      y = "Cell type (state)") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # 移除主要网格线
      panel.grid.minor = element_blank(),  # 移除次要网格线
      axis.line = element_line(color = "black"),  # 保留轴线
      legend.title = element_blank(),
      legend.position = "none"
    )

  # 将 ggplot2 图转换为 girafe 对象
  girafe(ggobj = p, options = list(
    opts_selection(type = "none"),
    opts_hover(css = "stroke:black; stroke-width:2px;"),
    opts_hover_inv(css = "opacity:0.2;"),
    opts_sizing(rescale = TRUE)), width_svg = 8, height_svg = 6)
}
#----

# 绘制number of cell with barcode in each cell type----
plot_barplot_barcode_2 <- function(pbmc) {
  # 计算每种 celltype 的计数
  celltype_counts <- pbmc %>%
    group_by(celltype) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  celltype_counts$celltype <- factor(celltype_counts$celltype,levels = rev(unique(celltype_counts$celltype)) )
  # 获取 unique 的 celltype
  unique_celltypes <- unique(pbmc$celltype)

  # 生成所需数量的颜色，并与 unique 的 celltype 一一对应
  palette_colors <- setNames(getPalette(length(unique_celltypes)), unique_celltypes)

  p <- ggplot(celltype_counts, aes(y = celltype, x = count, fill = celltype)) +
    geom_bar_interactive(stat = "identity", aes(tooltip = paste0("Number of cell with barcode: ", count, "\nCell type (state): ",  celltype), data_id = celltype ) ) +
    scale_fill_manual(values = palette_colors) +
    labs(
      x = "Number of cell with barcode",
      y = "Cell type (state)") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  # 移除主要网格线
      panel.grid.minor = element_blank(),  # 移除次要网格线
      axis.line = element_line(color = "black"),  # 保留轴线
      legend.title = element_blank(),
      legend.position = "none"
    )

  # 将 ggplot2 图转换为 girafe 对象
  girafe(ggobj = p, options = list(
    opts_selection(type = "none"),
    opts_hover(css = "stroke:black; stroke-width:2px;"),
    opts_hover_inv(css = "opacity:0.2;"),
    opts_sizing(rescale = TRUE)), width_svg = 8, height_svg = 6)
}
#----


# 画克隆热图的准备函数----
fate_mapping <- function(data, idx='celltype', input_type = 'table'){
  data_levels=NULL
  if (input_type == 'table') {
    if (class(data[,idx])== 'factor') { data_levels<-levels(data[,idx]) }
    data <- clean_pbmc_data(data)
    lineage_use = unique(data[,idx])
    freq_list <- purrr::map(unique(data$barcodes),function(i){
      data_use = data[data$barcodes==i,]
      freq = as.data.frame(table(data_use$celltype))
      freq$Var1 = as.character(freq$Var1)
      freq = freq[freq$Freq!=0,]
      lineage_absent = lineage_use[!lineage_use %in% as.character(freq$Var1)]
      lineage_absent_df =data.frame(lineage_absent,rep(0,length(lineage_absent)))
      colnames(lineage_absent_df) = colnames(freq)
      freq = rbind(freq,lineage_absent_df)
      rownames(freq) = freq$Var1
      freq = freq[lineage_use,]
      return(as.data.frame(t(data.frame(freq$Freq)))) })

    freq_df = do.call(dplyr::bind_rows,freq_list)
    rownames(freq_df) = unique(data$barcodes)
    colnames(freq_df) = lineage_use }else if(input_type == 'matrix'){ freq_df = data }
  return(freq_df) }
#----

# 生成克隆热图的函数clone_heatmap----
clone_heatmap <- function( pbmc,input_type_clone = 'table',normalize_method='ratio' ){

  if (all(c("barcodes", "celltype") %in% names(pbmc))) {
    pbmc <- pbmc[, c("barcodes", "celltype")]
    pbmc <- clean_pbmc_data(pbmc) }

    barcode_freq = fate_mapping(pbmc, input_type = input_type_clone )

    if(normalize_method == 'ratio'){
    barcode_freq = apply(barcode_freq, 1, function(x){
      row_sum = sum(x)
      return(x/row_sum) })
    barcode_freq = t(barcode_freq)
    barcode_freq <- as.data.frame(barcode_freq)
    barcode_freq <- barcode_freq[nrow(barcode_freq):1, ]

    row_dend <- t(barcode_freq) %>%
      dist(method = "euclidean") %>%
      hclust(method = "complete") %>%
      as.dendrogram()

    col_dend <- (barcode_freq) %>%
      dist(method = "euclidean") %>%
      hclust(method = "complete") %>%
      as.dendrogram()

    row_order <- order.dendrogram(row_dend)
    col_order <- order.dendrogram(col_dend)

    ordered_barcode_freq <- barcode_freq[col_order, row_order]

    return(ordered_barcode_freq)
    }else{
    barcode_freq <- (log10(barcode_freq))
    barcode_freq <- as.data.frame(lapply(barcode_freq, function(x) {
      x[is.infinite(x)] <- 0
      return(x)
    }))

    row_dend <- t(barcode_freq) %>%
      dist(method = "euclidean") %>%
      hclust(method = "ward.D2") %>%
      as.dendrogram()

    col_dend <- (barcode_freq) %>%
      dist(method = "euclidean") %>%
      hclust(method = "ward.D2") %>%
      as.dendrogram()

    row_order <- order.dendrogram(row_dend)
    col_order <- order.dendrogram(col_dend)

    ordered_barcode_freq <- barcode_freq[col_order, row_order]

    return(ordered_barcode_freq)} }

#----

# 生成谱系相关性热图的函数----
cell_type_fate_similartiy <- function(data_raw,idx='celltype',input_type='table',method='spearman',...){
  if (input_type=='table') {
  cell_fate = data_raw[,c('barcodes','celltype')]
  cell_fate <- clean_pbmc_data(cell_fate)
  lineage_use = unique(cell_fate[,idx])
  sample_similarity_list = list()
  for (i in lineage_use) {
    for (j in lineage_use) {
      sample1 = cell_fate[cell_fate[,idx]==i,]
      sample2 = cell_fate[cell_fate[,idx]==j,]

      fre_all = as.data.frame(table(sample1[,1]))
      fre_all_all = as.data.frame(table(fre_all[,2]))
      fre_all1 = as.data.frame(table(sample2[,1]))
      fre_all_all1 = as.data.frame(table(fre_all1[,2]))

      overlapped_idx = intersect(sample1[,1],sample2[,1])
      all_idx = union(sample1[,1],sample2[,1])
      df_plot = data.frame(all_idx,rep(0,length(all_idx)),rep(0,
                                                                   length(all_idx)))
      df_plot[,2] = fre_all[match(df_plot$all_idx,fre_all[,1]),2]
      df_plot[,3] = fre_all1[match(df_plot$all_idx,fre_all1[,1]),2]
      colnames(df_plot)[2:3] = c(i,j)
      df_plot[is.na(df_plot)] = 0

      sample_similarity = cor(df_plot[,2],df_plot[,3],method = method)
      if (i==j && is.na(sample_similarity)) { sample_similarity=1 }
      sample_similarity_list[[paste0(i,'-',j)]] = data.frame(i,j,sample_similarity) } }

  sample_similarity_df = do.call(dplyr::bind_rows,sample_similarity_list)
  sample_similarity_df = reshape2::dcast(sample_similarity_df,i~j)
  rownames(sample_similarity_df) = sample_similarity_df[,1]
  sample_similarity_df = sample_similarity_df[,-1]
  sample_similarity_df[is.na(sample_similarity_df)]=0 } else if(input_type=='matrix'){
    sample_similarity_df = cor(data_raw,method = method) }

  row_dend <- t(sample_similarity_df) %>%
    dist(method = "euclidean") %>%
    hclust(method = "complete") %>%
    as.dendrogram()

  col_dend <- (sample_similarity_df) %>%
    dist(method = "euclidean") %>%
    hclust(method = "complete") %>%
    as.dendrogram()

  row_order <- order.dendrogram(row_dend)
  col_order <- order.dendrogram(col_dend)

  sample_similarity_df_dev <- sample_similarity_df[col_order, row_order]
  return(sample_similarity_df_dev) }
#----

# 画fate bias 统计图的函数----
celltype_sample_fraction <- function(dataset_name, rna, sample='celltype', fate_bias='lineage',bar_pie_choice) {
  scvi_meta <- rna
  ct_freq_list <- list()

  # 计算每个细胞类型的命运偏差的比例
  for (i in unique(scvi_meta[,sample])) {
    meta_use <- scvi_meta[scvi_meta[,sample] == i, ]
    df1 <- as.data.frame(table(meta_use[,fate_bias]))
    colnames(df1) <- c('fate_bias', 'number')
    df1$sample <- i
    df1$number <- (df1$number / sum(df1$number)) * 100
    ct_freq_list[[i]] <- df1
  }

  ct_freq_df <- do.call(bind_rows, ct_freq_list)

  # 创建工具提示信息
  sample_stats <- ct_freq_df %>%
    group_by(fate_bias) %>%
    reframe(
      tooltip = paste0("Fate Bias: ", fate_bias, "\n",
                       "Cell type (state): ", sample, "\n",
                       "Ratio: ", format(number, digits=2, nsmall=2), "%")
    ) %>%
    distinct(fate_bias, .keep_all = TRUE)

  ct_freq_df <- ct_freq_df %>%
    left_join(sample_stats, by = "fate_bias")

  ct_freq_df$ggiraph_label <- paste("Point", 1:nrow(ct_freq_df))
  custom_colors <- c(
    "#BDBDBD", "#8DB5CE", "#542E8B", "#D7BD90", "#C82129", "#F296C0", "#647950", "#114B9B", "#FBBD90", "#193E51", "#BE93BF")

  # umap颜色函数
  getPalette = colorRampPalette( custom_colors )

  if(dataset_name == 'Weinreb_2020_Science'){
    cols <- c(
      "Baso --bias" = "#D7BD90",
      "Ccr7_DC --bias" = "#114B9B",
      "Eos --bias" = "#193E51",
      "Erythroid --bias" = "#F296C0",
      "Lymphoid --bias" = "#FBBD90",
      "Mast --bias" = "#C82129",
      "Meg --bias" = "#647950",
      "Monocyte --bias" = "#8DB5CE",
      "Neutrophil --bias" = "#542E8B",
      "pDC --bias" = "#BE93BF",
      "undiff --bias" = "#BDBDBD",
      "No bias" = "black"
    ) }else{
      # 生成颜色映射
      unique_fate_bias <- unique(ct_freq_df$fate_bias)
      cols <- setNames(getPalette(length(unique_fate_bias)), unique_fate_bias)
    }

  # 按 number 排序，并设置因子的级别
  ct_freq_df <- ct_freq_df %>%
    arrange(desc(number))

  ct_freq_df$fate_bias <- factor(ct_freq_df$fate_bias, levels = unique(ct_freq_df$fate_bias))

  if(bar_pie_choice == 'Bar plot'){
  gg <- ggplot(ct_freq_df, aes(x = fate_bias, y = number, fill = fate_bias, tooltip = tooltip, data_id = ggiraph_label)) +
    geom_col_interactive() +
    theme_classic() +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 35),
          legend.position = "none") +
    xlab('Fate Bias') +
    scale_y_continuous(expand = c(0, 0)) +
    ylab('Ratio (%)') +
    scale_fill_manual(values = cols)

  #ggsave(filename = '/data/yexing/scLT/www/celltype_sample_fraction_barplot.pdf', plot = gg, device = "pdf", width = 8, height = 6, units = "in", dpi = 100, limitsize = FALSE)

  p <- girafe(ggobj = gg, options = list(
    opts_selection(type = "none"),
    opts_hover(css = "stroke:black; stroke-width:2px;"),
    opts_hover_inv(css = "opacity:0.2;"),
    opts_sizing(rescale = TRUE)),
    width_svg = 12,
    height_svg = 10)    }else{

  gg <- ggplot(ct_freq_df, aes(x = "", y = number, fill = fate_bias, tooltip = tooltip, data_id = ggiraph_label)) +
    geom_bar_interactive(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    theme_classic() +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme(text = element_text(size = 15),
          axis.text.x = element_blank(),  # 隐藏x轴文本
          axis.ticks = element_blank(),   # 隐藏x轴刻度
          panel.grid = element_blank(),# 隐藏网格线
          legend.position = "none") +
    xlab('') +
    ylab('') +
    scale_fill_manual(values = cols)

  #ggsave(filename = '/data/yexing/scLT/www/celltype_sample_fraction_pieplot.pdf', plot = gg, device = "pdf", width = 8, height = 6, units = "in", dpi = 100, limitsize = FALSE)

  p <- girafe(ggobj = gg, options = list(
    opts_selection(type = "none"),
    opts_hover(css = "stroke:black; stroke-width:2px;"),
    opts_hover_inv(css = "opacity:0.2;"),
    opts_sizing(rescale = TRUE)),
    width_svg = 12,
    height_svg = 10)  }



  return(p)
}
#----

# fate_bias_summary函数----
fate_bias_summary <- function(dataset_name, which_celltype, fate_bias, pbmc, bar_pie_choice_summary ){
  pbmc = pbmc[,c('celltype', 'barcodes')]
  pbmc <- clean_pbmc_data(pbmc)
  pbmc$lineage = 'No bias'
    for (j in 1:length(fate_bias)) {
      fate = fate_bias[[j]]
      for ( clone_name in fate ){
        matching_rows <- which(pbmc$barcodes == clone_name )
        pbmc$lineage[matching_rows] <- names(fate_bias)[j] } }

  pbmc_1 = pbmc[pbmc$celltype %in% which_celltype, ]
  if( nrow(pbmc_1)!= 0 ){
  pbmc_2 = celltype_sample_fraction(dataset_name ,pbmc_1, bar_pie_choice= bar_pie_choice_summary)
  return(pbmc_2) }else{ return(NULL) } }
#----

# 统计clone size的函数----
create_interactive_bar_plot <- function(data, barcodes_vector, clone_fate_bias) {
  if( length(barcodes_vector)>0 ){
    barcodes_vector <- unique(barcodes_vector)
  # 过滤并统计条形码的出现次数
    counts <- data %>%
      dplyr::filter(barcodes %in% barcodes_vector) %>%
      dplyr::count(barcodes, name = "count")

    index <- match(barcodes_vector, counts$barcodes)
    sorted_counts <- counts[index, ]
    sorted_counts$barcodes <- factor(sorted_counts$barcodes, levels = unique(sorted_counts$barcodes))
  # 创建图形
    if (nrow(sorted_counts) > 6) {
      p <- ggplot(sorted_counts, aes(x = barcodes, y = count, tooltip = paste("Clone ID:", barcodes, "\nCell number count:", count," cells"), data_id = barcodes)) +
        geom_col_interactive(fill = "#BDBDBD") +
        labs(x = "Barcodes", y = "Count") +
        theme_classic() +
        theme(
          panel.grid.major = element_blank(),  # 移除主要网格线
          panel.grid.minor = element_blank(),  # 移除次要网格线
          axis.line = element_line(color = "black"),  # 保留轴线
          text = element_text(size = 15),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 35)) +
        scale_x_discrete(breaks = NULL)+
        scale_y_continuous(expand = c(0, 0))
    } else {
      p <- ggplot(sorted_counts, aes(x = barcodes, y = count, tooltip = paste("Clone ID:", barcodes, "\nCell number count:", count," cells"), data_id = barcodes)) +
        geom_col_interactive(fill = "#BDBDBD") +
        theme_classic() +
        theme(
          panel.grid.major = element_blank(),  # 移除主要网格线
          panel.grid.minor = element_blank(),  # 移除次要网格线
          axis.line = element_line(color = "black"),  # 保留轴线
          text = element_text(size = 15),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 35)) +
        labs(x = "Barcodes", y = "Count") +
        scale_y_continuous(expand = c(0, 0))
    }

  # 创建交互式图形
  girafe_plot <- girafe(ggobj = p,options = list( opts_selection(type = "none"), opts_sizing(rescale = TRUE) ), width_svg = 12, height_svg = 10)
  return(girafe_plot) }else{return(NULL)} }

create_interactive_bar_plot_2 <- function(data, dataset_name, fate_use, clone_fate_bias) {

  barcodes_vector <- c()
  for(fate in fate_use) {
    barcode_use <- clone_fate_bias[[fate]]
    barcodes_vector <- c(barcodes_vector, barcode_use) }
  barcodes_vector <- unique(barcodes_vector)

  if( length(barcodes_vector)>0 ){
    # 过滤并统计条形码的出现次数
    counts <- data %>%
      dplyr::filter(barcodes %in% barcodes_vector) %>%
      dplyr::count(barcodes, name = "count")

    index <- match(barcodes_vector, counts$barcodes)
    sorted_counts <- counts[index, ]
    sorted_counts$barcodes <- factor(sorted_counts$barcodes, levels = sorted_counts$barcodes)
    # 创建图形
    if (nrow(sorted_counts) > 6) {
      p <- ggplot(sorted_counts, aes(x = barcodes, y = count, tooltip = paste("Clone ID:", barcodes, "\nCell number count:", count," cells"), data_id = barcodes)) +
        geom_col_interactive(fill = "#BDBDBD") +
        labs(x = "Barcodes", y = "Count") +
        theme_classic() +
        theme(
          panel.grid.major = element_blank(),  # 移除主要网格线
          panel.grid.minor = element_blank(),  # 移除次要网格线
          axis.line = element_line(color = "black"),  # 保留轴线
          text = element_text(size = 15),
          axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 35)) +
        scale_x_discrete(breaks = NULL)+
        scale_y_continuous(expand = c(0, 0))

    } else {
      p <- ggplot(sorted_counts, aes(x = barcodes, y = count, tooltip = paste("Clone ID:", barcodes, "\nCell number count:", count," cells"), data_id = barcodes)) +
        geom_col_interactive(fill = "#BDBDBD") +
        theme_classic() +
        theme(
          panel.grid.major = element_blank(),  # 移除主要网格线
          panel.grid.minor = element_blank(),  # 移除次要网格线
          axis.line = element_line(color = "black"),  # 保留轴线
              text = element_text(size = 15),
              axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 35)) +
        labs(x = "Barcodes", y = "Count") +
        scale_y_continuous(expand = c(0, 0))
      }

    #ggsave(filename = '/data/yexing/scLT/www/clone_size.pdf', plot = p, device = "pdf", width = 8, height = 6, units = "in", dpi = 100, limitsize = FALSE)

    # 创建交互式图形
    girafe_plot <- girafe(ggobj = p,options = list( opts_selection(type = "none"), opts_sizing(rescale = TRUE) ), width_svg = 12, height_svg = 10)
    return(girafe_plot) }else{return(NULL)} }
#----

# 读取上传csv文件的函数----
file_type <- 'matrix'
readDataset <- function(filePath) {
  header <- read.csv(filePath, nrows = 1)
  if ('barcodes' %in% names(header) && 'celltype' %in% names(header)) {
    file_type <<- 'table'
    dataset <- read.csv(filePath)
  } else {
    file_type <<- 'matrix'
    dataset <- read.csv(filePath, row.names = 1) }
  return(dataset) }
#----
#老的clone_fate_bias函数接着用----
clone_fate_bias <- function(data, fate_use = '', data_type = 'table', alternative = 'greater') {
  list_result <- list()

  if (data_type == 'table') {
    data <- data[!is.na(data[, 1]), ]
    all_clone_size <- nrow(data)

    if (fate_use == '') {
      dominant_fate <- data %>%
        group_by(clone = data[, 1]) %>%
        count(fate = data[, 2]) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(fate)
      names(dominant_fate) <- unique(data[, 1])
    }

    for (i in unique(data[, 1])) {
      if (fate_use == '') {
        fate_use <- dominant_fate[[i]]
      }
      ct_all_clone_size <- nrow(data[data[, 2] == fate_use, ])
      data_clone <- data[data[, 1] == i, ]
      clone_size <- nrow(data_clone)
      clone_ct_size <- nrow(data_clone[data_clone[, 2] == fate_use, ])
      fate_ratio <- clone_ct_size / clone_size
      if (length(clone_ct_size) > 0) {
        contingency_table <- matrix(c(clone_ct_size, clone_size - clone_ct_size,
                                      ct_all_clone_size - clone_ct_size, all_clone_size - (ct_all_clone_size - clone_ct_size)),
                                    nrow = 2)
        p_val <- fisher.test(contingency_table, alternative = alternative)$p.value
        list_result[[as.character(i)]] <- c(i, fate_use, clone_size, fate_ratio, p_val)
      }
    }
  } else if (data_type == 'matrix') {
    clone_ct_freq <- as.data.frame(as.table(as.matrix(data)))
    clone_ct_freq <- clone_ct_freq[clone_ct_freq$Freq != 0, ]
    all_clone_size <- sum(clone_ct_freq$Freq)

    if (fate_use == '') {
      dominant_fate <- clone_ct_freq %>%
        group_by(Clone = clone_ct_freq[, 1]) %>%
        count(Fate = clone_ct_freq[, 2], wt = Freq) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(Fate)
      names(dominant_fate) <- unique(clone_ct_freq[, 1])
    }

    for (i in unique(clone_ct_freq[, 1])) {
      if (fate_use == '') {
        fate_use <- dominant_fate[[i]]
      }
      ct_all_clone_size <- sum(clone_ct_freq[clone_ct_freq[, 2] == fate_use, 3])
      clone_ct_freq_clone <- clone_ct_freq[clone_ct_freq[, 1] == i, ]
      clone_size <- sum(clone_ct_freq_clone[, 3])
      clone_ct_size <- sum(clone_ct_freq_clone[clone_ct_freq_clone[, 2] == fate_use, 3])
      fate_ratio <- clone_ct_size / clone_size
      if (length(clone_ct_size) > 0) {
        contingency_table <- matrix(c(clone_ct_size, clone_size - clone_ct_size,
                                      ct_all_clone_size - clone_ct_size, all_clone_size - (ct_all_clone_size - clone_ct_size)),
                                    nrow = 2)
        p_val <- fisher.test(contingency_table, alternative = alternative)$p.value
        list_result[[as.character(i)]] <- c(i, fate_use, clone_size, fate_ratio, p_val)
      }
    }
  }

  #result_df = as.data.frame(t(as.data.frame(list_result)))
  result_df <- as.data.frame(do.call(rbind, list_result), stringsAsFactors = FALSE)
  colnames(result_df) <- c('clone_name', 'fate_use', 'clone_size', 'fate_ratio', 'pvalue')
  result_df <- result_df %>%
    mutate(clone_size = as.numeric(clone_size),
           pvalue = as.numeric(pvalue),
           fdr = p.adjust(pvalue, method = "fdr")) %>%
    arrange(fdr) %>%
    mutate(clone_size_rank = row_number()) %>%
    filter(fate_ratio > 0)
  #result_df <- result_df %>% select(-fate_ratio)

  return(result_df)
}



# 画gene 小提琴图使用的fate_bias_summary函数变体----
fate_bias_summary_2 <- function(which_celltype, fate_bias, pbmc ){
  pbmc = pbmc[,c('celltype', 'barcodes')]
  pbmc <- clean_pbmc_data(pbmc)
  pbmc$lineage = 'No bias'
  for (j in 1:length(fate_bias)) {
    fate = fate_bias[[j]]
    for ( clone_name in fate ){
      matching_rows <- which(pbmc$barcodes == clone_name )
      pbmc$lineage[matching_rows] <- names(fate_bias)[j] } }

  pbmc_1 = pbmc[pbmc$celltype %in% which_celltype, ]
  if( nrow(pbmc_1)!= 0 ){ return(pbmc_1) }else{ return(NULL) } }
#----




