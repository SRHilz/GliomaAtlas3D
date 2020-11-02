library(GliomaAtlas3D)

message("plot3DModel() ...")

## Adopted from example("plot3DModel")
path <- system.file(package="GliomaAtlas3D", "exdata", "models")
samples <- dir(path)
message("All samples: ", paste(sQuote(samples), collapse=", "))
sample <- sample(samples, size = 1L)
message("Random sample: ", sQuote(sample))

sfs <- dir(file.path(path, sample))
message("All sfs: ", paste(sQuote(sfs), collapse=", "))
sf <- sample(sfs, size = 1L)
message("Random sf: ", sQuote(sf))
coords <- readRDS(file.path(path, sample, sf, "coordinates_samples.rds"))
colors <- rep("#FF0000", times=nrow(coords))
names(colors) <- rownames(coords)
message("Colors: ", paste(sQuote(colors), collapse=", "))

plot3DModel(sample, sf=sf, colors=colors)

message("plot3DModel() ... OK")
