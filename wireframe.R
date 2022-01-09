library(geomorph)

# plotRefToTarget(ref,PCA.field$pc.shapes$PC1min,links = gup.links)
# # Item 3
# plotRefToTarget(ref,PCA.field$pc.shapes$PC1max,links = gup.links)
# # Item 4
# plotRefToTarget(ref,PCA.field$pc.shapes$PC2min,links = gup.links)
# # Item 5
# plotRefToTarget(ref,PCA.field$pc.shapes$PC2max, links = gup.links)

GP <- gridPar(pt.bg = "grey", pt.size = 0.85, tar.pt.bg = "black", 
              tar.pt.size = 0.85)

pc1.field <- plotRefToTarget(PCA.field$pc.shapes$PC1min, PCA.field$pc.shapes$PC1max, 
                gridPars=GP, mag=1.2, method="points", links = gup.links)

pc2.field <- plotRefToTarget(PCA.field$pc.shapes$PC2min, PCA.field$pc.shapes$PC2max, 
                gridPars=GP, mag=1.2, method="points", links = gup.links)
 

pc1.lab <- plotRefToTarget(PCA.lab$pc.shapes$PC1min, PCA.lab$pc.shapes$PC1max, 
                gridPars=GP, mag=1.5, method="points", links = gup.links)

pc2.lab <- plotRefToTarget(PCA.lab$pc.shapes$PC2min, PCA.lab$pc.shapes$PC2max, 
                gridPars=GP, mag=1, method="points", links = gup.links)
ref.field <- mshape(gpa$coords[,,1:1111])
ref.lab   <- mshape(gpa$coords[,,1112:1387])


# Item 2 to plot, the first TPS grid; 
# here we use the outline option to add to the visualisation
par(mar = c(0,0,0,0)) # sets the margins

plotRefToTarget(ref.lab,PCA.lab$pc.shapes$PC1min,gridPars=GP, mag=3, method="TPS", links = gup.links)
plotRefToTarget(ref.lab,PCA.lab$pc.shapes$PC1max,gridPars=GP, mag=3, method="TPS", links = gup.links)
plotRefToTarget(ref.lab,PCA.lab$pc.shapes$PC2min,gridPars=GP, mag=1.5, method="TPS", links = gup.links)
plotRefToTarget(ref.lab,PCA.lab$pc.shapes$PC2max,gridPars=GP, mag=1.5, method="TPS", links = gup.links)

plotRefToTarget(ref.field,PCA.field$pc.shapes$PC1min,gridPars=GP, mag=1.5, method="TPS", links = gup.links)
plotRefToTarget(ref.field,PCA.field$pc.shapes$PC1max,gridPars=GP, mag=1.5, method="TPS", links = gup.links)
plotRefToTarget(ref.field,PCA.field$pc.shapes$PC2min,gridPars=GP, mag=1.5, method="TPS", links = gup.links)
plotRefToTarget(ref.field,PCA.field$pc.shapes$PC2max,gridPars=GP, mag=1.5, method="TPS", links = gup.links)


