#' Human dMRI Connectome
#'
#' A graph comprised from n=3228 diffusion MRI fiber graphs, primarily from the CoRR and MRN1313 datasets. Graphs were processed with the ndmg pipeline; see references for details.
#'
#' @format An `\link[igraph]{igraph}` object containing the average dMRI graph.
#' \describe{
#' \item{`V(human.dmri)$name`}{The id in the Desikan-Killany parcellation for the vertex.}
#' \item{`V(human.dmri)$lobe`}{The lobe of the brain the parcel overlaps maximally with. `M` is midbrain, `P` is parietal, `O` is occipital, `T` is temporal, `F` is frontal.}
#' \item{`V(human.dmri)$hemisphere`}{The hemisphere of the brain the parcel overlaps maximally with. `R` is right; `L` is left.}
#' \item{`V(human.dmri)$region`}{A composite of the `lobe` and `hemisphere` attributes.}
#' \item{`E(human.dmri)$weight`}{The average number of fibers connecting the particular two vertices.}
#' }
#' @references Kiar, G., Bridgeford, E.W., Roncal, W.G., et al. A High-Throughput Pipeline Identifies Robust Connectomes but Troublesome Variability.
#' \emph{bioarxiv}, 2018.
"human.dmri"

#' Human fMRI Connectome
#'
#' A graph comprised from n=1790 functional MRI connectivity matrices, primarily from the CoRR dataset. Graphs were processed with the ndmg pipeline; see references for details.
#'
#' @format An `\link[igraph]{igraph}` object containing the average fMRI graph.
#' \describe{
#' \item{`V(human.fmri)$name`}{The id in the Desikan-Killany parcellation for the vertex.}
#' \item{`V(human.fmri)$lobe`}{The lobe of the brain the parcel overlaps maximally with. `M` is midbrain, `P` is parietal, `O` is occipital, `T` is temporal, `F` is frontal.}
#' \item{`V(human.fmri)$hemisphere`}{The hemisphere of the brain the parcel overlaps maximally with. `R` is right; `L` is left.}
#' \item{`V(human.fmri)$region`}{A composite of the `lobe` and `hemisphere` attributes.}
#' \item{`E(human.fmri)$weight`}{The average functional connectivity between the two vertices.}
#' }
#' @references Kiar, G., Bridgeford, E.W., Roncal, W.G., et al. A High-Throughput Pipeline Identifies Robust Connectomes but Troublesome Variability.
#' \emph{bioarxiv}, 2018.
"human.fmri"

#' Human Male dMRI Connectome
#'
#' A graph comprised from n=613 male diffusion MRI fiber graphs, primarily from the CoRR and MRN1313 datasets. Graphs were processed with the ndmg pipeline; see references for details.
#'
#' @format An `\link[igraph]{igraph}` object containing the average dMRI graph.
#' \describe{
#' \item{`V(human.male.dmri)$name`}{The id in the Desikan-Killany parcellation for the vertex.}
#' \item{`V(human.male.dmri)$lobe`}{The lobe of the brain the parcel overlaps maximally with. `M` is midbrain, `P` is parietal, `O` is occipital, `T` is temporal, `F` is frontal.}
#' \item{`V(human.male.dmri)$hemisphere`}{The hemisphere of the brain the parcel overlaps maximally with. `R` is right; `L` is left.}
#' \item{`V(human.male.dmri)$region`}{A composite of the `lobe` and `hemisphere` attributes.}
#' \item{`E(human.male.dmri)$weight`}{The average number of fibers connecting the particular two vertices.}
#' }
#' @references Kiar, G., Bridgeford, E.W., Roncal, W.G., et al. A High-Throughput Pipeline Identifies Robust Connectomes but Troublesome Variability.
#' \emph{bioarxiv}, 2018.
"human.male.dmri"

#' Human Female dMRI Connectome
#'
#' A graph comprised from n=612 female diffusion MRI fiber graphs, primarily from the CoRR and MRN1313 datasets. Graphs were processed with the ndmg pipeline; see references for details.
#'
#' @format An `\link[igraph]{igraph}` object containing the average dMRI graph.
#' \describe{
#' \item{`V(human.female.dmri)$name`}{The id in the Desikan-Killany parcellation for the vertex.}
#' \item{`V(human.female.dmri)$lobe`}{The lobe of the brain the parcel overlaps maximally with. `M` is midbrain, `P` is parietal, `O` is occipital, `T` is temporal, `F` is frontal.}
#' \item{`V(human.female.dmri)$hemisphere`}{The hemisphere of the brain the parcel overlaps maximally with. `R` is right; `L` is left.}
#' \item{`V(human.female.dmri)$region`}{A composite of the `lobe` and `hemisphere` attributes.}
#' \item{`E(human.female.dmri)$weight`}{The average number of fibers connecting the particular two vertices.}
#' }
#' @references Kiar, G., Bridgeford, E.W., Roncal, W.G., et al. A High-Throughput Pipeline Identifies Robust Connectomes but Troublesome Variability.
#' \emph{bioarxiv}, 2018.
"human.female.dmri"

#' Mouse dMRI Connectome
#'
#' A graph comprised from a diffusion MRI fiber graph of a mouse brain. Data was collected at the Center for In Vivo Microscopy (Duke University; )
#'
#' @format An `\link[igraph]{igraph}` object containing the dMRI graph:
#' \describe{
#' \item{`V(mouse.dmri)$name`}{The id of the parcellation for the vertex.}
#' \item{`V(mouse.dmri)$level1`}{The level 1 hierarchy the vertex is categorized as. `M` is midbrain, `F` is forebrain, `H` is hindbrain, `W` is white matter.}
#' \item{`V(mouse.dmri)$hemisphere`}{The hemisphere of the brain the parcel overlaps maximally with. `R` is right; `L` is left.}
#' \item{`V(mouse.dmri)$region`}{A composite of the `level1` and `hemisphere` attributes.}
#' \item{`E(mouse.dmri)$weight`}{The number of fibers connecting the particular two vertices.}
#' }
#' @references Calabrese, E. et al. A Diffusion MRI Tractography Connectome of the Mouse Brain and Comparison with Neuronal Tracer Data.
#' \emph{Cerebral Cortex}, 2015.
"mouse.dmri"

#' C. Elegans Hermaphrodite Gap-Junction Connectome
#'
#' A graph consisting of a synapse-level map of the gap-junction connections in the brain of a hermaphrodite Caenorhabditis (C.) Elegans.
#'
#' @format An `\link[igraph]{igraph}` object containing the gap-junction graph:
#' \describe{
#' \item{`V(celegans.herm.chem)$name`}{The name of the C. Elegans neuron.}
#' \item{`V(celegans.herm.chem)$type`}{The type of the C. Elegans neuron. Either `M` motor neuron, `I` inter neuron, or `S` sensory neuron.}
#' \item{`V(celegans.herm.chem)$hemisphere`}{The hemisphere the C. Elegans neuron is found in. Either `R` right, `L` left, or `B` bilateral, ring neuron spanning both hemispheres, etc.}
#' }
#' @references Cook et al. The complete connectome of Caenorhabditis Elegans. 2018.
"celegans.herm.gap"

#' C. Elegans Male Gap-Junction Connectome
#'
#' A graph consisting of a synapse-level map of the gap-junction connections in the brain of a male Caenorhabditis (C.) Elegans.
#'
#' @format An `\link[igraph]{igraph}` object containing the gap-junction graph:
#' \describe{
#' \item{`V(celegans.male.gap)$name`}{The name of the C. Elegans neuron.}
#' \item{`V(celegans.male.gap)$type`}{The type of the C. Elegans neuron. Either `M` motor neuron, `I` inter neuron, or `S` sensory neuron.}
#' \item{`V(celegans.male.gap)$hemisphere`}{The hemisphere the C. Elegans neuron is found in. Either `R` right, `L` left, or `B` bilateral, ring neuron spanning both hemispheres, etc.}
#' }
#' @references Cook et al. The complete connectome of Caenorhabditis Elegans. 2018.
"celegans.male.gap"

#' C. Elegans Male Chemical Connectome
#'
#' A graph consisting of a synapse-level map of the chemical connections in the brain of a male Caenorhabditis (C.) Elegans.
#'
#' @format An `\link[igraph]{igraph}` object containing the chemical graph:
#' \describe{
#' \item{`V(celegans.male.chem)$name`}{The name of the C. Elegans neuron.}
#' \item{`V(celegans.male.chem)$type`}{The type of the C. Elegans neuron. Either `M` motor neuron, `I` inter neuron, or `S` sensory neuron.}
#' \item{`V(celegans.male.chem)$hemisphere`}{The hemisphere the C. Elegans neuron is found in. Either `R` right, `L` left, or `B` bilateral, ring neuron spanning both hemispheres, etc.}
#' }
#' @references Cook et al. The complete connectome of Caenorhabditis Elegans. 2018.
"celegans.male.chem"

#' C. Elegans Hermaphrodite Chemical Connectome
#'
#' A graph consisting of a synapse-level map of the chemical connections in the brain of a hermaphrodite Caenorhabditis (C.) Elegans.
#'
#' @format An `\link[igraph]{igraph}` object containing the chemical graph:
#' \describe{
#' \item{`V(celegans.herm.chem)$name`}{The name of the C. Elegans neuron.}
#' \item{`V(celegans.herm.chem)$type`}{The type of the C. Elegans neuron. Either `M` motor neuron, `I` inter neuron, or `S` sensory neuron.}
#' \item{`V(celegans.herm.chem)$hemisphere`}{The hemisphere the C. Elegans neuron is found in. Either `R` right, `L` left, or `B` bilateral, ring neuron spanning both hemispheres, etc.}
#' }
#' @references Cook et al. The complete connectome of Caenorhabditis Elegans. 2018.
"celegans.herm.chem"

#' Drosophila Left Mushroom Body Connectome
#'
#' A binary graph consisting of a synapse-level map of the connections in the left mushroom body of a Drosophila (fruit fly).
#'
#' @format An `\link[igraph]{igraph}` object containing the graph:
#' \describe{
#' \item{`V(fly.mushroom.left)$name`}{The name of the Drosophila neuron.}
#' \item{`V(fly.mushroom.left)$type`}{The type of the Drosophila neuron. Either `K` Kenyon Cell, `I` Input neurons,  `O` Output neurons, or `P` Projection neurons.}
#' \item{`V(fly.mushroom.left)$claw`}{The claw each Kenyon Cell is part of in the mushroom body. Cells not marked with `type` as `K` are not assigned a value.}
#' \item{`V(fly.mushroom.left)$dist`}{The distance along the claw each Kenyon Cell is part of in the mushroom body. Cells not marked with `type` as `K` are not assigned a value.}
#' }
#' @references Eichler, K. et al. The complete wiring diagram of a high-order learning and memory center, the insect mushroom body.
#' \emph{Nature}, 2017.
"fly.mushroom.left"

#' Drosophila Right Mushroom Body Connectome
#'
#' A binary graph consisting of a synapse-level map of the connections in the right mushroom body of a Drosophila (fruit fly).
#'
#' @format An `\link[igraph]{igraph}` object containing the graph:
#' \describe{
#' \item{`V(fly.mushroom.left)$name`}{The name of the Drosophila neuron.}
#' \item{`V(fly.mushroom.left)$type`}{The type of the Drosophila neuron. Either `K` Kenyon Cell, `I` Input neurons,  `O` Output neurons, or `P` Projection neurons.}
#' \item{`V(fly.mushroom.left)$claw`}{The claw each Kenyon Cell is part of in the mushroom body. Cells not marked with `type` as `K` are not assigned a value.}
#' \item{`V(fly.mushroom.left)$dist`}{The distance along the claw each Kenyon Cell is part of in the mushroom body. Cells not marked with `type` as `K` are not assigned a value.}
#' }
#' @references Eichler, K. et al. The complete wiring diagram of a high-order learning and memory center, the insect mushroom body.
#' \emph{Nature}, 2017.
"fly.mushroom.right"
