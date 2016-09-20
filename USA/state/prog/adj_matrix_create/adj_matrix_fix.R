rm(list=ls())

# load text file with adjacency matrix
dat <- read.table('~/git/mortality/USA/state/output/adj_matrix_create/USA.graph',header=F, sep= ' ', col.names = (paste0('V',seq_len(10))),fill=T)

# adjust the file to fix Hawaii and Alaska connections



