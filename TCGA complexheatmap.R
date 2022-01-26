#complexheatmap
setwd("")
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
rt=read.table("mat.tsv.txt",header = T,row.names = 1,sep = "\t")
metadata=read.table("metadata.tsv.txt",header = T,row.names = 1,sep="\t")
rt=as.matrix(rt)

metadata[1:6,1:4]
Type <- metadata$Type
Type <- Type[!is.na(Type)] 
length(unique(Type)) #5

pick.col <- brewer.pal(9, 'Greens') # allowed maximum for palette Greens is 9
col.Type <- colorRampPalette(pick.col)(length(unique(Type)))

# Grade
Grade <- metadata$Grade
Grade <- Grade[!is.na(Grade)]
pick.col <- brewer.pal(9, 'Blues')
col.Grade <- colorRampPalette(pick.col)(length(unique(Grade)))

# Gender
Gender <- metadata$Gender
Gender <- Gender[!is.na(Gender)]
pick.col <- brewer.pal(9, 'Oranges')
col.Gender <- colorRampPalette(pick.col)(length(unique(Gender)))

# Original.Subtype
Original.Subtype <- metadata$Original.Subtype
Original.Subtype <- Original.Subtype[!is.na(Original.Subtype)]
pick.col <- brewer.pal(9, 'Purples')
col.Original.Subtype <- colorRampPalette(pick.col)(length(unique(Original.Subtype)))
#
IDH.status <- metadata$IDH.status
IDH.status <- IDH.status[!is.na(IDH.status)]
pick.col <- brewer.pal(9, 'Purples')
col.IDH.status <- colorRampPalette(pick.col)(length(unique(IDH.status)))
#
Vital.status <- metadata$Vital.status
Vital.status <- Vital.status[!is.na(Vital.status)]
pick.col <- brewer.pal(9, 'Purples')
col.Vital.status <- colorRampPalette(pick.col)(length(unique(Vital.status)))
# Sample information data 
ann <- data.frame(
  Type = metadata$Type,
  Grade = metadata$Grade,
  Gender = metadata$Gender,
  Original.Subtype = metadata$Original.Subtype,
  IDH.status = metadata$IDH.status,
  Vital.status=metadata$Vital.status)
# Color list
names(col.Type)=as.character(0:3)
names(col.Grade)=as.character(0:2)
names(col.Gender)=as.character(0:1)
names(col.Original.Subtype)=as.character(0:7)
names(col.IDH.status)=as.character(0:1)
names(col.Vital.status)=as.character(0:1)
colors=list(Type = c('IS1' = 'blue', 'IS2' = 'red', 'IS3' = 'green3', 'IS4' = 'gold'),
            Grade=c("G4"="yellow","G3"="brown","G2"="purple"),
            Gender=c("female"="orange","male"="tan"),
            Original.Subtype=c("Proneural"="violet","Mesenchymal"="brown","G-CIMP"="blue","Classical"="green","Neural"="red","IDHwt"="magenta","IDHmut-non-codel"="navy","IDHmut-codel"="purple"),
            IDH.status=c("Mutant"="green","WT"="red"),
            Vital.status=c("1"="turquoise", "0"="coral"))
colAnn <- HeatmapAnnotation(
  df = ann,
  col = colors,
  which = 'col', # set 'col' (samples) or 'row' (gene) annotation
  na_col = 'white',
  annotation_height = 0.6,
  annotation_width = unit(1, 'cm'),
  gap = unit(1, 'mm'),
  annotation_legend_param = list(
    Type = list(
      nrow = 4,
      title = 'Type',
      title_position = 'topcenter',
      legend_direction = 'vertical',
      title_gp = gpar(fontsize = 12, fontface = 'bold'),
      labels_gp = gpar(fontsize = 12, fontface = 'bold')),
    Grade = list(
      nrow = 3,
      title = 'Grade',
      title_position = 'topcenter',
      legend_direction = 'vertical',
      title_gp = gpar(fontsize = 12, fontface = 'bold'),
      labels_gp = gpar(fontsize = 12, fontface = 'bold')),
    Gender = list(
      nrow = 2,
      title = 'Gender',
      title_position = 'topcenter',
      legend_direction = 'vertical',
      title_gp = gpar(fontsize = 12, fontface = 'bold'),
      labels_gp = gpar(fontsize = 12, fontface = 'bold')),
    Original.Subtype = list(
      nrow = 8,
      title = 'Original.Subtype',
      title_position = 'topcenter',
      legend_direction = 'vertical',
      title_gp = gpar(fontsize = 12, fontface = 'bold'),
      labels_gp = gpar(fontsize = 12, fontface = 'bold')),
    IDH.status = list(
      nrow = 2,
      title = 'IDH.status',
      title_position = 'topcenter',
      legend_direction = 'vertical',
      title_gp = gpar(fontsize = 12, fontface = 'bold'),
      labels_gp = gpar(fontsize = 12, fontface = 'bold'))))
#draw heatmap
rt=t(scale(t(rt)))
col_fun = colorRamp2(c(-0.5, 0, 1),c('turquoise', 'white', 'red'))
group_list=c(rep('IS1',264),rep('IS2',137),rep("IS3",218),rep("IS4",53))
Heatmap(rt, name = "mat", col = col_fun,show_column_names = FALSE,top_annotation = colAnn,cluster_columns = FALSE,column_split = group_list)

