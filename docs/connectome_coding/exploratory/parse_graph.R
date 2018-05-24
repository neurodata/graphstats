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
