read.celegans.graph <- function(csv) {
  csv.dat <- read.csv(csv, header=FALSE)
  type.col <- 1; type.row <- 1
  vertex.col <- 3; vertex.row <- 3
  graph.startcol <- 4; graph.startrow <- 4
  graph.endcol <- dim(csv.dat)[2] - 1; graph.endrow <- dim(csv.dat)[1] - 1
  gobj <- data.frame(source=c(), target=c(), weight=c())
  src.vertices.name <- as.character(csv.dat$V3[graph.startrow:graph.endrow])

  # get the vertex type for the sources
  src.vertices.type <-as.character(csv.dat$V1[graph.startrow:graph.endrow])
  for (vtx.id in 1:length(src.vertices.name)) {
    if (src.vertices.type[vtx.id] == "") {
      src.vertices.type[vtx.id] <- type
    } else {
      type <- src.vertices.type[vtx.id]
    }
  }
  src.vertices.type[src.vertices.type %in% c("SEX-SPECIFIC CELLS", "SEX SPECIFIC")] = "SEX-SPECIFIC"
  src.vertices.data <- data.frame(name=src.vertices.name, type=src.vertices.type)

  # get the vertex type for the targets
  tgt.vertices.name <- as.character(sapply(csv.dat[graph.startcol:graph.endcol],
                                           function(datcol) as.character(datcol[3])))
  tgt.vertices.type <- as.character(sapply(csv.dat[graph.startcol:graph.endcol],
                                           function(datcol) as.character(datcol[1])))
  for (vtx.id in 1:length(tgt.vertices.name)) {
    if (tgt.vertices.type[vtx.id] == "") {
      tgt.vertices.type[vtx.id] <- type
    } else {
      type <- tgt.vertices.type[vtx.id]
    }
  }
  tgt.vertices.type[tgt.vertices.type %in% c("SEX-SPECIFIC CELLS", "SEX SPECIFIC")] = "SEX-SPECIFIC"
  tgt.vertices.data <- data.frame(name=tgt.vertices.name, type=tgt.vertices.type)
  vertices.data <- rbind(src.vertices.data, tgt.vertices.data)
  vertices.data <- vertices.data[!duplicated(vertices.data),]

  # add the row vertices and the corresponding data
  for (col in csv.dat[names(csv.dat)[graph.startcol:graph.endcol]]) {
    ss.col <- as.numeric(as.character(col[graph.startrow:graph.endrow]))
    ss.col[is.na(ss.col)] <- 0  # empty cell
    if (sum(ss.col) > 0) {
      src.vertices <- as.character(src.vertices.name[ss.col > 0]); target.vertex <- as.character(col[3])
      gobj <- rbind(gobj, data.frame(source=src.vertices, target=target.vertex,
                                     weight=ss.col[ss.col > 0]))
    }
  }
  ig.gobj <- graph_from_data_frame(gobj, vertices=vertices.data)
  return(ig.gobj)
}

read.mouse.graph <- function(csv) {
  csv.dat <- read.csv(csv, header=FALSE)
  vertex.col <- 1; vertex.row <- 1
  graph.startcol <- 2; graph.startrow <- 2
  graph.endcol <- dim(csv.dat)[2] - 1; graph.endrow <- dim(csv.dat)[1] - 1
  gobj <- data.frame(source=c(), target=c(), weight=c())
  src.vertices.name <- as.character(csv.dat$V1[graph.startrow:graph.endrow])
  src.vertices.data <- data.frame(name=src.vertices.name)


  # get the vertex type for the targets
  tgt.vertices.name <- as.character(sapply(csv.dat[graph.startcol:graph.endcol],
                                           function(datcol) as.character(datcol[1])))
  tgt.vertices.data <- data.frame(name=tgt.vertices.name)
  vertices.data <- rbind(src.vertices.data, tgt.vertices.data)
  vertices.data <- vertices.data[!duplicated(vertices.data),]

  # add the row vertices and the corresponding data
  for (col in csv.dat[names(csv.dat)[graph.startcol:graph.endcol]]) {
    ss.col <- as.numeric(as.character(col[graph.startrow:graph.endrow]))
    ss.col[is.na(ss.col)] <- 0  # empty cell
    if (sum(ss.col) > 0) {
      src.vertices <- as.character(src.vertices.name[ss.col > 0]); target.vertex <- as.character(col[1])
      gobj <- rbind(gobj, data.frame(source=src.vertices, target=target.vertex,
                                     weight=ss.col[ss.col > 0]))
    }
  }
  ig.gobj <- graph_from_data_frame(gobj, vertices=vertices.data)
  return(ig.gobj)
}

read.human.graph <- function(human) {
  hemisphere.raw <- read.csv('../../ndmg-repos/DS01216_res-1x1x1_hemispheric_res-1x1x1.csv')
  tissue.raw <- read.csv('../../ndmg-repos/DS01216_res-1x1x1_tissue_res-1x1x1.csv')
  colnames(hemisphere.raw) <- c("id", "non-labelled", "left", "right")
  colnames(tissue.raw) <- c( "id", "non-labelled", "csf", "gray", "white")
  vertex.data <- data.frame()
  for (i in 1:dim(hemisphere.raw)[1]) {
    tissue.ss <- tissue.raw[i,][,colnames(tissue.raw) != "id"]
    hemisphere.ss <- hemisphere.raw[i,][,colnames(hemisphere.raw) != "id"]
    vertex.data <- rbind(vertex.data, data.frame(id=hemisphere.raw[i,]$id,
                                                 tissue=colnames(tissue.ss)[which(tissue.ss == max(tissue.ss))[1]],
                                                 hemisphere=colnames(hemisphere.ss)[which(hemisphere.ss == max(hemisphere.ss)[1])]))
  }
}


read.human.graph.brodmann <- function() {
  dmri.graph <- read.graph('/data/sub-NDARAD481FXF_acq-64dir_dwi_desikan_res-1x1x1_measure-spatial-ds.edgelist', format='ncol', directed=FALSE)
  fmri.graph <- read.graph('/data/sub-NDARAD481FXF_task-rest_bold_desikan_res-2x2x2_measure-correlation.edgelist', format='ncol', directed=FALSE)

  V.dmri <- V(dmri.graph)
  V.fmri <- V(fmri.graph)

  lobes.raw <- read.csv('../data/desikan_res-1x1x1_lobes_res-1x1x1.csv')
  colnames(lobes.raw) <- c("id", "non-labeled", "frontal", "temporal", "occipital", "parietal", "midbrain", "frontal", "temporal", "occipital", "parietal")

  vertex.data <- data.frame()
  for (i in 1:dim(lobes.raw)[1]) {
    lobes.ss <- lobes.raw[i,][,colnames(lobes.raw) != "id"]
    vertex.data <- rbind(vertex.data, data.frame(id=as.character(round(as.numeric(lobes.raw[i,]$id))),
                                                 lobe=colnames(lobes.ss)[which(lobes.ss == max(lobes.ss))[1]]))
  }
  V(dmri.graph)$name = as.character(round(as.numeric(V(dmri.graph)$name)))
  V(fmri.graph)$name = as.character(round(as.numeric(V(fmri.graph)$name)))

  E(dmri.graph)$dmri <- E(dmri.graph)$weight
  dmri.graph <- delete_edge_attr(dmri.graph, "weight")
  E(fmri.graph)$fmri <- E(fmri.graph)$weight
  fmri.graph <- delete_edge_attr(fmri.graph, "weight")

  gcomb <- union(fmri.graph, dmri.graph)

  E(gcomb)$dmri[is.na(E(gcomb)$dmri)] = 0
  E(gcomb)$fmri[is.na(E(gcomb)$fmri)] = 0

  gcomb <- set_vertex_attr(gcomb, "lobe", index=vertex.data$id, value=as.character(vertex.data$lobe))
  return(gcomb)
}

read.human.graph.desikan <- function() {
  dmri.graph <- read.graph('/data/average_dMRI_3228.edgelist', format='ncol', directed=FALSE)
  fmri.graph <- read.graph('/data/average_fMRI_1790.edgelist', format='ncol', directed=FALSE)
  dmri.male <- read.graph('/data/average_dMRI_male_613.edgelist', format='ncol', directed=FALSE)
  dmri.female <- read.graph('/data/average_dMRI_female_612.edgelist', format='ncol', directed=FALSE)

  lobes.raw <- read.csv('/home/eric/Documents/research/ndmg-repos/desikan_res-1x1x1_lobes_res-1x1x1.csv')
  lobes.raw <- lobes.raw[, -13]
  lobes <- c("non-labeled", "frontal", "temporal", "occipital", "parietal", "midbrain", "frontal", "temporal", "occipital", "parietal", "midbrain")
  hemispheres <- c("non-labeled", "right", "right", "right", "right", "right", "left", "left", "left", "left", "left")

  vertex.data <- data.frame()
  for (i in 1:dim(lobes.raw)[1]) {
    lobes.ss <- lobes.raw[i,][,colnames(lobes.raw) != "p1reg"]
    vertex.data <- rbind(vertex.data, data.frame(id=as.character(round(as.numeric(lobes.raw[i,]$p1reg))),
                                                 lobe=lobes[which(lobes.ss == max(lobes.ss))[1]],
                                                 hemisphere=hemispheres[which(lobes.ss == max(lobes.ss))[1]]))
  }
  graphs <- lapply(list(dmri.graph, fmri.graph, dmri.male, dmri.female), function(graph) {
    graph <- permute.vertices(graph, as.numeric(V(graph)$name))
    graph <- set_vertex_attr(graph, "lobe", index=vertex.data$id, value=as.character(vertex.data$lobe))
    graph <- set_vertex_attr(graph, "hemisphere", index=vertex.data$id, value=as.character(vertex.data$hemisphere))
  })
  return(graphs[[1]], graphs[[2]], graphs[[3]], graphs[[4]])
}

fly.merge <- function(flyleft, flyright) {
  V1 <- V(flyleft)
  V2 <- V(flyright)

  Vss1 <- V1[V1$name %in% V2$name]
  Vss2 <- V2[V2$name %in% V1$name]

  left.ss <- induced_subgraph(flyleft, Vss1)
  right.ss <- induced_subgraph(flyright, Vss2)

  E(left.ss)$left <- E(left.ss)

  E(right.ss)$right <- E(right.ss)
  gcomb <- union(left.ss, right.ss)
  V(gcomb)$type <- V(gcomb)$type_1
  gcomb <- delete_vertex_attr(gcomb, "type_1")
  gcomb <- delete_vertex_attr(gcomb, "type_2")

  E(gcomb)$right[is.na(E(gcomb)$right)] = 0
  E(gcomb)$left[is.na(E(gcomb)$left)] = 0
}

read.celegans.merge <- function(gap.male, gap.herm, chem.male, chem.herm) {
  V1 <- V(gap.male)
  V2 <- V(gap.herm)
  V3 <- V(chem.male)
  V4 <- V(chem.herm)

  Vss1 <- V1[V1$name %in% V2$name & V1$name %in% V3$name & V1$name %in% V4$name & V1$type %in% c("SENSORY NEURONS", "INTERNEURONS", "MOTOR NEURONS")]
  Vss2 <- V2[V2$name %in% V1$name & V2$name %in% V3$name & V2$name %in% V4$name & V2$type %in% c("SENSORY NEURONS", "INTERNEURONS", "MOTOR NEURONS")]
  Vss3 <- V3[V3$name %in% V1$name & V3$name %in% V2$name & V3$name %in% V4$name & V3$type %in% c("SENSORY NEURONS", "INTERNEURONS", "MOTOR NEURONS")]
  Vss4 <- V4[V4$name %in% V1$name & V4$name %in% V2$name & V4$name %in% V3$name & V4$type %in% c("SENSORY NEURONS", "INTERNEURONS", "MOTOR NEURONS")]

  gap.male.ss <- induced_subgraph(gap.male, Vss1)
  gap.herm.ss <- induced_subgraph(gap.herm, Vss2)

  chem.male.ss <- induced_subgraph(chem.male, Vss3)
  chem.herm.ss <- induced_subgraph(chem.herm, Vss4)

  E(gap.ss)$gap <- E(gap.ss)$weight
  gap.ss <- delete_edge_attr(gap.ss, "weight")

  E(chem.ss)$chem <- E(chem.ss)$weight
  chem.ss <- delete_edge_attr(chem.ss, "weight")
  gcomb <- union(gap.ss, chem.ss)
  V(gcomb)$type <- V(gcomb)$type_1
  gcomb <- delete_vertex_attr(gcomb, "type_1")
  gcomb <- delete_vertex_attr(gcomb, "type_2")

  E(gcomb)$chem[is.na(E(gcomb)$chem)] = 0
  E(gcomb)$gap[is.na(E(gcomb)$gap)] = 0
  return(gcomb)
}
