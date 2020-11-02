# Linked Data Frames

Work with linked-data idiomatically in R using data frames.

Linked Data Frames have columns of RDF resources. These resources are expressed as S3 objects with rich descriptions.

We use the `vctrs` package to encapsulate RDF resources into vectors. This means they can be combined into data frames and dealt with as atomic values while still retaining orthogonal attributes for e.g. their label.
