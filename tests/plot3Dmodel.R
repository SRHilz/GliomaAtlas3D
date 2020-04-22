library(GliomaAtlas3D)

message("plot3Dmodel() ...")

## Adopted from example("plot3Dmodel")
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
message("Colors: ", paste(sQuote(colors), collapse=", "))

plot3Dmodel(sample, sf=sf, colors=colors)

message("plot3Dmodel() ... OK")
