#---------------------------------------------------#
# 1. Installing and loading the required packages
#---------------------------------------------------#

#install.packages ("bipartite")
require (bipartite)
#install.packages ("igraph")
require (igraph)
require (magrittr)

#----------------------#
# 2. Reading the networks - quantitative matrices
#----------------------#

# Ant-plant interactions (AP)
ap <-"https://www.nceas.ucsb.edu/interactionweb/data/ant_plant/text_matrices/davidson&fisher_1991.txt" %>%
  url() %>%
  read.csv(.,sep = "\t", header=F) %>%
  as.matrix(.)
dimnames(ap)=NULL

# Plant-Pollinator (PP) (bezerra is built in the bipartite pkg)
pp <- bezerra2009
dimnames(pp)=NULL

# There are many more implemented datasets in the bipartite package
# Also many more available on https://www.nceas.ucsb.edu/interactionweb/resources.html



#-------------------------------------#
# 4. Transforming into a binary matrix 
#-------------------------------------#

ap.bin=matrix(0,dim(ap)[1],dim(ap)[2]) 	# creating a matrix filled with zeroes 
ap.bin[(which(ap>=1))]=1                # Filling the existing interactions with ones
# to save the new binary matrices
write.table(ap.bin,file="ap.bin_0.txt",row.names=F, col.names=F, append=T, quote=F)

pp.bin=matrix(0,dim(pp)[1],dim(pp)[2]) 
pp.bin[(which(pp>=1))]=1
write.table(pp.bin,file="pp.bin_0.txt",row.names=F, col.names=F, append=T, quote=F)

#---------------------------#
# 5. Visualising the networks 
#---------------------------#

# Matrix visualisation
# ~~~~~~~~~~~~~~~~~~~~
# Ant-Plant:
par(mfrow=c(1,1))
visweb(ap.bin, type = "none")               # print of the matrix "as is"
visweb(ap, type = "none")                   # grey shading mapped on interaction frequencies
visweb(ap.bin, type = "nested")             # maximal row and column sums nested in upper left corner
visweb(ap, type = "nested")                 # interaction weights matter
visweb(ap.bin, type = "diagonal", frame=F)  # highest interactions along the diagonal 
visweb(ap, type = "diagonal", frame=F)      # interaction weights matter

# Plant-Pollinator
visweb(pp, type = "none")          # this network is already sorted to optimize nestedness
visweb(pp, type = "nested")        # no difference
visweb(pp.bin, type = "nested")    

# ~~ more graphical parameters in ?visweb, examples section.~~

# Bipartite visualisation separating trophic levels
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plotweb(ap.bin)
plotweb(ap) # minimum crossing of links; nestedness not implemented in plotweb function.

ap.sorted <- sortweb(ap) # sortweb puts the higher interactions in the upper left corner.
plotweb(ap.sorted, method="normal") # need to specify method="normal", otherwise minimal
                                    # crossing of links overwrite the sorting

plotweb(pp, method="normal")

# there are other graphical parameters in plotweb function too.
order.ap=sortweb(ap.bin)
order.pp=sortweb(pp.bin)

# to compare both networks on same plot:
par(mfrow=c(1,2))
plotweb(order.ap, method="normal", arrow="both",bor.col.interaction="grey", y.width.low=0.02, y.width.high=0.02, col.high="red", col.low="blue", high.lablength=0, low.lablength=0)
mtext("ap",side=3 , line=1, font=1)
plotweb(order.pp, method="normal",arrow="both",bor.col.interaction="grey", y.width.low=0.02, y.width.high=0.02, col.high="red", col.low="blue", high.lablength=0, low.lablength=0)
mtext("pp",side=3 , line=1, font=1)
par(mfrow=c(1,1))



# Graph visualisation: usaually for complex networks/food webs but work for bipartite too.
# ~~~~~~~~~~~~~~~~~~~
# we will use a built-in network: Safariland. More info at ?Safariland

# need to transform the incidence matrix into a graph object:
g <- graph_from_incidence_matrix(Safariland, directed=F, weighted=T)
plot(g)

# which nodes are most connected? Can you guess some modules?

# there is a "layout" argument in the plot function for grpah objects, with many options.
# default to layout=layout.fruchterman.reingold




#--------------------------------#
# 6. Measuring network properties
#--------------------------------#

# Connectance
#~~~~~~~~~~~~

K.ap=colSums(ap.bin) #degree of the columns
E.ap=sum(ap.bin) #total number of interactions 
R.ap=dim(ap.bin)[1] #number of lines
C.ap=L=dim(ap.bin)[2] #number of columns
Conec.ap=E.ap/(R.ap*C.ap) #calculating connectance
Conec.ap

K.pp=apply(pp.bin,2,sum)
E.pp=apply(as.matrix(K.pp), 2, sum)
R.pp=dim(pp.bin)[1]
C.pp=L=dim(pp.bin)[2]
Conec.pp=E.pp/(R.pp*C.pp)
Conec.pp



# Nestedness: weighted NODF
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate nestedness for the following built-in datasets and visualise the
# networks in using the different methods we went through. You can also 
# transform the weighted matrices into binary ones.


# Use the following function to calculate nestedness:
networklevel("enter_dataset_here", index="choose an index") 
# Use index="nestedness" for binary matrices, and index="weighted NODF" for
# weighted matrices.
# Ranges: for index="nestedness", 0=perfect nestedness and 100=chaos
# index="weighted NODF"= high values indicate high nestedness.


# Here are examples with the built-in data sets and the visweb function
# Mutualistic networks
# 
# ?Safariland for more info
Saf_N <- networklevel(Safariland, index="weighted NODF")
visweb(Safariland, type = "nested", prednames=F, preynames= F)
legend("bottom", legend=paste("N=", Saf_N))
visweb(Safariland, type = "diagonal", prednames=F, preynames= F, frame=F)

# ?small1976
small_N <- networklevel(small1976, index="weighted NODF")
visweb(small1976, type = "nested", prednames=F, preynames= F)
legend("bottom", legend=paste("N=", small_N))
visweb(small1976, type = "diagonal", prednames=F, preynames= F, frame=F)

# ?vazllo
networklevel(vazllao, index="weighted NODF")
visweb(vazllao, type = "nested", prednames=F, preynames= F)
visweb(vazllao, type = "diagonal", prednames=F, preynames= F, frame=F)

# ?schemske1978
schemske_N <- networklevel(schemske1978, index="weighted NODF")
visweb(schemske1978, type = "nested", prednames=F, preynames= F)
legend("bottom", legend=paste("N=", schemske_N))
visweb(schemske1978, type = "diagonal", prednames=F, preynames= F, frame=F)


# Antagonistic bipartite networks: you can download the excel files from the 
# Interaction Web Database https://www.nceas.ucsb.edu/interactionweb/resources.html
# Choose the "intensity" matrices. Or use the following code:
# host-parasite
aish_i <- "https://www.nceas.ucsb.edu/interactionweb/data/host_parasite/text_matrices/aishihik_i.txt" %>%
  url() %>%
  read.csv(.,sep = "\t", header=F) %>%
  as.matrix(.)
aish_N <- networklevel(aish_i, index="weighted NODF")
visweb(aish_i, "nested", prednames=F, preynames= F) # red cells are NA values
legend("bottom", legend=paste("N=", aish_N))
visweb(aish_i, "diagonal", prednames=F, preynames= F, frame=F)


cold_i <- "https://www.nceas.ucsb.edu/interactionweb/data/host_parasite/text_matrices/smallwood_i.txt" %>%
  url() %>%
  read.csv(.,sep = "\t", header=F) %>%
  as.matrix(.)
cold_N <- networklevel(cold_i, index="weighted NODF")
visweb(cold_i, "nested", prednames=F, preynames= F)
legend("bottom", legend=paste("N=", cold_N))
visweb(cold_i, "diagonal", prednames=F, preynames= F, frame=F)


parsnip_i <- "https://www.nceas.ucsb.edu/interactionweb/data/host_parasite/text_matrices/smallwood_i.txt" %>%
  url() %>%
  read.csv(.,sep = "\t", header=F) %>%
  as.matrix(.)
parsnip_N <- networklevel(parsnip_i, index="weighted NODF")
visweb(parsnip_i, "nested", prednames=F, preynames= F)
legend("bottom", legend=paste("N=", parsnip_N))
visweb(parsnip_i, "diagonal", frame=F)


# Modularity: playing with Rnetcarto
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("rnetcarto") #installing rnetcarto
#https://cran.r-project.org/web/packages/rnetcarto  #in case you want to check the documentation
require (rnetcarto)

rownames(pp)<-paste0("r",as.character(1:nrow(pp)))
colnames(pp)<-paste0("c",as.character(1:ncol(pp)))
netcarto(pp,bipartite=TRUE)

rownames(pp.bin)<-paste0("r",as.character(1:nrow(pp.bin)))
colnames(pp.bin)<-paste0("c",as.character(1:ncol(pp.bin)))
netcarto(pp.bin,bipartite=TRUE)
