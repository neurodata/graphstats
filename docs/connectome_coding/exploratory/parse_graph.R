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


read.human.graph.brodmann <- function(human) {
  dmri.graph <- read.graph('/data/sub-NDARAD481FXF_acq-64dir_dwi_brodmann_res-1x1x1_measure-spatial-ds.edgelist', format='ncol', directed=FALSE)
  fmri.graph <- read.graph('/data/sub-NDARAD481FXF_task-rest_bold_brodmann_res-2x2x2_measure-correlation.edgelist', format='ncol', directed=FALSE)

  V.dmri <- V(dmri.graph)
  V.fmri <- V(fmri.graph)

  lobes.raw <- read.csv('../data/brodmann_res-1x1x1_lobes_res-1x1x1.csv')
  colnames(lobes.raw) <- c("id", "non-labelled", "frontal", "temporal", "parietal", "occiptal")
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

read.celegans.merge <- function(gap, chem) {
  V1 <- V(gap)
  V2 <- V(chem)

  Vss1 <- V1[V1$name %in% V2$name & V1$type %in% c("SENSORY NEURONS", "INTERNEURONS", "MOTOR NEURONS")]
  Vss2 <- V2[V2$name %in% V1$name & V2$type %in% c("SENSORY NEURONS", "INTERNEURONS", "MOTOR NEURONS")]

  gap.ss <- induced_subgraph(gap, Vss1)
  chem.ss <- induced_subgraph(chem, Vss2)

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
