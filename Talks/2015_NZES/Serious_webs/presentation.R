# ---
# title: "Serious webs"
# author: "Giulio Valentino Dalla Riva"
# date: "November 19, 2015"
# output:
#   ioslides_presentation:
#     widescreen: true
#     smaller: true
# subtitle: An NZES powered workshop
# ---

# Hypothesis testing in food webs | And now, for something totally different

# Get all the ingredients

## R packages | What do we need?

```{r,echo=T,warning=FALSE,message=FALSE,error=FALSE}
library("igraph") # To handle the webs objects and plot them
library("magrittr") # Provides pipes ( %>% )
library("NetIndices") # To compute ecological indices
library("dplyr") # Filter, select, mutate, ... data sets
library("reshape2") # To organise and 'shape' our data sets
library("ggplot2") # Nice plots

options(stringsAsFactors = FALSE) # Ask me why
```

## Ingredients: interactions data | The best stuff is Open Access

Ryan F. Hechinger, Kevin D. Lafferty, John P. McLaughlin, Brian L. Fredensborg, Todd C. Huspeni, Julio Lorda, Parwant K. Sandhu, Jenny C. Shaw, Mark E. Torchin, Kathleen L. Whitney, and Armand M. Kuris (2011). *Food webs including parasites, biomass, body sizes, and life stages for three California/Baja California estuaries*. **Ecology** 92:791---791. http://dx.doi.org/10.1890/10-1383.1

```{r}
Web.source <- "http://esapubs.org/archive/ecol/E092/066/CSMweb_Links.txt"
```

## Ingredients: interactions data | The best stuff is Open Access

```{r}
Web.source %>%
  url %>% # Open the connection
  # and read the data
  read.csv(sep = "\t") -> Raw.web.data
```

## Ingredients: interactions data | What's in there?

```{r}
Raw.web.data %>% names
```

## Ingredients| Knead the data to a web (1/2)

```{r}
"http://esapubs.org/archive/ecol/E092/066/CSMweb_Links.txt" %>%
  url %>% # Open the connection
  read.csv(sep = "\t") %>% # Read in the raw data
  filter(LinkType %in% "predation") %>% # Filter only the relevant interactions
  filter(LinkEvidence %in% "observed") %>% # and only if directly observed
  select(ResourceNodeID,ConsumerNodeID) %>% # Select the column of From and To
  graph.data.frame(directed=TRUE) -> CSM.graph # And build a igraph web
```

## Ingredients| Knead the data to a web (2/2)

What do we have?

```{r}
CSM.graph %>% vcount # How many vertices in the graph?
CSM.graph %>% ecount # How many edges in the graph?
```

Connectance:

```{r}
Link.density <- function(x) x %>% {ecount(.) / vcount(.)^2}
CSM.graph %>% Link.density
```

# Null models and observed food webs

## Which is which? | Hard!

```{r, echo=FALSE}
CSM.graph %>%
  {erdos.renyi.game(vcount(.),Link.density(.),type="gnp",directed = T, loops = F)} -> Random.graph 
par(mfrow=c(1,2),mar=c(0,0,0,0))

CSM.graph %>%            
plot.igraph(vertex.label=NA,
            vertex.size=4,
            edge.arrow.size=.1,
            layout=layout.circle)

Random.graph %>%
plot.igraph(vertex.label=NA,
            vertex.size=4,
            edge.arrow.size=.1,
            layout=layout.circle)
```

## Which is which? |  Better?

```{r, echo=FALSE}
par(mfrow=c(1,2),mar=c(0,0,0,0))
CSM.graph %>%            
plot.igraph(vertex.label=NA,
            vertex.size=sqrt(degree(.)),
            edge.arrow.size=.2,
            layout=layout.kamada.kawai)

Random.graph %>%
plot.igraph(vertex.label=NA,
            vertex.size=sqrt(degree(.)),
            edge.arrow.size=.2,
            layout=layout.kamada.kawai)
```

## Which is which? | What do we look at?

```{r, echo=FALSE}
par(mfrow=c(3,2), mar = c(2,1,1,1))
CSM.graph %>%
  degree(mode = "total") %>%
  hist(main = NULL, breaks = seq(0,max(.)), xlab = "Total degree")

Random.graph %>%
  degree(mode = "total")  %>%
  hist(main = NULL, breaks = seq(0,max(.)), xlab = "Total degree")

CSM.graph %>%
  degree(mode = "out") %>%
  hist(main = NULL, breaks = seq(0,max(.)), xlab = "Outward degree")

Random.graph %>%
  degree(mode = "out")  %>%
  hist(main = NULL, breaks = seq(0,max(.)), xlab = "Outward degree")

CSM.graph %>%
  degree(mode = "in") %>%
  hist(main = NULL, breaks = seq(0,max(.) ), xlab = "Inward degree")

Random.graph %>%
  degree(mode = "in")  %>%
  hist(main = NULL, breaks = seq(0,max(.)), xlab = "Inward degree")
```

# Null models

## Null models | The grandfather of random webs (1/3)

Paul Erd&#337;s and Alfr&#233;d R&#233;nyii. (1959)  
*On Random Graphs. I* **Publicationes Mathematicae** 6: 290–297.

- $n$ vertices
- each link has i.i.d. probability $p$

Hence, each and every graph with $M$ links appears with probability
$$p^M \left( 1 - p \right)^{n^2 - M}$$

## Null models | Erd&#337;s--R&#233;nyii model (2/3)

```{r, eval=FALSE}
CSM.graph %>%
  erdos.renyi.game(n = vcount(.), # Random graph with the same number of nodes...
                   p.or.m = Link.density(.), # ...and link density as the observed
                   type = "gnp", # This is to tell igraph we specify link density
                   directed = T, # We want a directed graph...
                   loops = F # ...with no loops
                  ) -> Random.graph
```

## Null models | Erd&#337;s--R&#233;nyii model (3/3)

Shorter version:

```{r, eval=FALSE}
CSM.graph %>%
  {erdos.renyi.game(vcount(.), # number of vertices
                    Link.density(.), # link density
                   "gnp", T, F) # type, directed, and no loops
  } -> Random.graph
```

Notice the curly brackets { } around the *game*.

## Null models | So many many *games*  {.columns-2}

- aging.ba.game
- aging.barabasi.game
- aging.prefatt.game
- asymmetric.preference.game
- barabasi.game
- bipartite.random.game
- callaway.traits.game
- cited.type.game
- degree.sequence.game
- erdos.renyi.game
- establishment.game
- forest.fire.game
- grg.game
- growing.random.game
- hrg.game
- interconnected.islands.game
- k.regular.game
- sbm.game
- static.fitness.game
- static.power.law.game
- watts.strogatz.game
- ...

# A Little Something Special

## Something special in the degree?

For the observed

```{r}
CSM.graph %>% degree(mode = "total") -> CSM.degree.tot
CSM.graph %>% degree(mode = "in") -> CSM.degree.in
CSM.graph %>% degree(mode = "out") -> CSM.degree.out
```

And the random

```{r}
Random.graph %>% degree(mode = "total") -> Random.degree.tot
Random.graph %>% degree(mode = "in") -> Random.degree.in
Random.graph %>% degree(mode = "out") -> Random.degree.out
```

## Something special in the degree? | Exploratory plots

Prepare the data...

```{r}
CSM.degrees <- data.frame("graph.name" = "CSM",
                       "degree.tot" = CSM.degree.tot,
                        "degree.in" = CSM.degree.in,
                        "degree.out" = CSM.degree.out)

Random.degrees <- data.frame("graph.name" = "Random",
                          "degree.tot" = Random.degree.tot,
                          "degree.in" = Random.degree.in,
                          "degree.out" = Random.degree.out)
```

```{r, eval = FALSE}
CSM.degrees %>% summary
Random.degrees %>% summary
```

## Something special in the degree? | Exploratory plots

... massage the data...

```{r}
CSM.degrees  %<>% melt
Random.degrees %<>% melt
rbind(CSM.degrees,Random.degrees) -> Full.degrees
```


## Something special in the degree? | Exploratory plots

... define a suitable ggplot function ...

```{r}
Plot.density.degree <- function(data){
  data %>%
    ggplot(aes(x = value, fill=graph.name)) %>%
  + geom_histogram(binwidth = 1, alpha = .5, position = "identity") %>%
  + facet_grid(. ~ variable) %>%
  + theme_bw() %>% return
}
```

## Something special in the degree? | Exploratory plots

```{r}
Full.degrees %>% Plot.density.degree
```

## Something special in the degree? | Is the *mean* special?

```{r}
CSM.degree.tot %>% mean
Random.degree.tot %>% mean
```

## Something special in the degree? | Is the *peak* special?

```{r}
Peak.density <- function(array_values) {
  array_values %>% density %$% # compute the density
    x[which.max(y)] %>% return # find the value at which the density is max
}

CSM.degree.tot %>% Peak.density
Random.degree.tot %>% Peak.density
```

## Something special in the degree? | Peak.density sanity test (1/2)

```{r}
Full.degrees %>%
  filter(variable %>% equals("degree.tot")) %>%
  Plot.density.degree %>%
  + geom_vline(xintercept = CSM.degree.tot %>% Peak.density, col = "red") %>%
  + geom_vline(xintercept = Random.degree.tot %>% Peak.density, col = "blue") -> densities.vs.obs
```

## Something special in the degree? | Peak.density sanity test (2/2)

```{r}
densities.vs.obs
```

## Something special in the degree? | Is the *variance* special?

```{r}
CSM.degree.tot %>% var
Random.degree.tot %>% var
```

# Hypothesis testing | When the going gets tough, the tough get going.

## Test what? | The intrepid hypothesizers

$H_0$: The degree distribution' (mean, peak, variance) is the same under the random (*null*) model and in the observed food web.

$H_1$: No, it is not, you silly! How often do you see such a weird distribution!

## Test what? | The repetitive testers

Idea: **sample** a random web,  
compute its degree distribution,  
compute the mean, peak and var,  
compare with the observed one,  
sample a random web,  
compute its **degree** distribution,  
compute the mean, peak and var,  
compare with the observed one,  
sample a random web,  
compute its degree distribution,  
compute the **mean**, **peak** and **var**,  
compare with the observed one,  
sample a random web,  
compute its degree distribution,  
compute the mean, peak and var,  
**compare** with the observed one,  
sample a random web,  
compute its degree distribution,  
compute the mean, peak and var,  
compare with the observed one,  
...

## Hypothesis testing! | Let's build a randomizer (1/3).

Random sampler (we know this already!):

```{r}
Sampler <- function(Graph) {
  Graph %>% {erdos.renyi.game(vcount(.), # number of vertices
                              Link.density(.), # link density
                              "gnp", T, F) # type, directed, and no loops
             } %>%
  return
}
```

## Hypothesis testing! | Let's build a randomizer (2/3).

Compute the degrees:

```{r}
Degreer <- function(Graph) {
  data.frame("degree.tot" = Graph %>% degree(mode = "total"),
             "degree.in" = Graph %>% degree(mode = "in"),
             "degree.out" = Graph %>% degree(mode = "out")) %>%
  return
}
```

## Hypothesis testing! | Let's build a randomizer (3/3).

Compute the desired statistics:
```{r}
# we define an ancillary function to compute the statistics we want
Stats <- function(Degrees,mode){
  data.frame("Mode" = mode, # We record whether in, out or total
             "Mean" = mode %>% Degrees[,.] %>% mean, # We want to compare the mean
             "Peak" = mode %>% Degrees[,.] %>% Peak.density, # The density peak
             "Var"  = mode %>% Degrees[,.] %>% var) %>% # And the variance
  return
}
```

For in, out and total degree:

```{r}
Statistiker <- function(Degree.data) {
  rbind(
    Degree.data %>% Stats("degree.in"),
    Degree.data %>% Stats("degree.out"),
    Degree.data %>% Stats("degree.tot")
  ) %>% # We bind all together
  return
}
```

## Hypothesis testing! | How does it work?

Now we have a pipeline from the graph to the statistics:

```{r, warning = FALSE,message=FALSE}
CSM.graph %>% # Get the graph
  Degreer %>% # Compute the degrees
  Statistiker %>% # Compute the statistics
  melt %>% # Melt the data (because plots)
  # mutate adds a news variable
  mutate(Origin = "observed") -> CSM.stats
```


## Hypothesis testing! | Run the simulations (1/2).

Prepare the data...

```{r}
R <- 42 # How many randomizations?
# Each sample of Statistics() is made by 3 rows
L <- R*3
# We initialise an empty data frame to store the simulations data
data.frame("Mode" = L %>% character,
           "Mean" = L %>% numeric,
           "Peak" = L %>% numeric,
           "Var"  = L %>% numeric) -> Rands.stats
```

## Hypothesis testing! | Run the simulations (2/2).

... fill the data.

```{r}
# Each sample of Statistics() is made by 3 rows
run.range <- seq(from = 1, to = L, by = 3)
for(run in run.range) {
  CSM.graph %>%
    Sampler %>%
    Degreer %>%
    Statistiker -> Rands.stats[run:(run+2),]
}
```

## Hypothesis testing! | Plot, plot, plot.

Transform the data to the same format of `CSM.data`

```{r, warning = FALSE,message=FALSE}
Rands.stats %<>%
  melt %>% # Melt the data (because plots)
  # mutate adds a news variable
  mutate(Origin = "random")
```

## Hypothesis testing! | Plot, plot, plot.

Put everything together

```{r, warning = FALSE}
Full.stats <- rbind(Rands.stats,CSM.stats)
```

## Hypothesis testing! | Plot, plot, plot.

... define a suitable ggplot function ...

```{r}
Plot.comparison <- function(Data.to.plot){
  Data.to.plot %>%
    ggplot(aes(x = value))%>% # the value on x and density on y
  + geom_histogram( # plot an histogram with the 'random' data
    data = Data.to.plot %>% filter(Origin %>% equals("random")), 
                   binwidth = 1) %>% # and set a proper bin width
  + geom_vline( # plot a vertical line with the 'observed' data
    data = Data.to.plot %>% filter(Origin %>% equals("observed")),
               aes(xintercept = value), # The observed statistics value
               col = "red", size = 2) %>% # And make it thick and red
  + facet_wrap(Mode ~ variable, scale="free") %>% # Do that for each Mode and statistics
  return
}
```

## Hypothesis testing! | Plot, plot, plot.

```{r}
Full.stats %>% Plot.comparison
```

# Wow, we did it! | Thanks everybody!

# Do we still have time?

## An ecologically sensible plot

- $x$ axis Omnivory Index (from `NetIndices`)
- $y$ axis Trophic Level (from `NetIndices`)
- vertex size degree (from `igraph`)
- vertex color centrality (betweenness) rank (from `igraph`)

## An ecologically sensible plot

```{r}
Plot.foodweb <- function(Graph, dodge=0.08, heat=10, size.scale=3) {
   if("viridis" %in% rownames(installed.packages()) == FALSE) {install.packages("viridis")}
   library(viridis)
   Layer <- function(Graph){
       Graph %>% vcount -> V # How many vertices in the graph?
       Graph %>% get.adjacency(sparse = F) %>% TrophInd -> Trophs
       # We define a matrix with two columns:
       c(Trophs$OI + V %>% runif(max=dodge), # one with Omnivory Index (and a bit of noise)
         Trophs$TL - 1 # and one with the Trophic Levels
        ) %>% matrix(ncol=2) %>%
       return
     }
  Graph %>% betweenness(di=F,nor=T) %>% rank(ties="min") %>% max %>% viridis -> col_palette
  Graph %>% betweenness(di=F,nor=T) %>% rank(ties="min") %>% col_palette[.] -> V(Graph)$color
  Graph %>% get.adjacency(sparse = F) %>% TrophInd %$% OI -> Omni
  Graph %>% plot.igraph(layout= Layer(.),
                        vertex.label=NA,
                        vertex.size= degree(.) %>% sqrt * size.scale,
                        edge.arrow.size=.5,
                        edge.width=.5)
}
```

## An ecologically sensible plot

```{r}
par(mar=c(0,0,0,0))
CSM.graph %>% Plot.foodweb
```

## Let's do it better!

Everything into a big function!

```{r, message=FALSE}
Tester <- function(Observed.Graph,Runs) {
  L <- Runs *3
  Observed.Graph %>% Degreer %>% Statistiker %>%
    melt %>% mutate(Origin = "observed") -> Observed.stats
  data.frame("Mode" = L %>% character,
             "Mean" = L %>% numeric,
             "Peak" = L %>% numeric,
             "Var"  = L %>% numeric) -> Rands.stats
  run.range <- seq(from = 1, to = L, by = 3)
  for(run in run.range) {
    Observed.Graph %>% Sampler %>%
    Degreer %>% Statistiker -> Rands.stats[run:(run+2),]
  }
  Rands.stats %>% melt %>%
    mutate(Origin = "random") %>% 
    rbind(Observed.stats) %>% Plot.comparison
}
```

## Let's do it better!

Everything into a big function!

```{r}
Graph.grabber <- function(web.source,linktype,linkevidence){
  web.source %>%
    url %>% # Open the connection
    read.csv(sep = "\t") %>% # Read in the raw data
    filter(LinkType %in% linktype) %>% # Filter on interactions type
    filter(LinkEvidence %in% linkevidence) %>% # and evidence type
    select(ResourceNodeID,ConsumerNodeID) %>% # Select the column of From and To
    graph.data.frame(directed=TRUE) %>%
return
}
```

## Let's do it better!

```{r, warning = FALSE, message = FALSE}
"http://esapubs.org/Archive/ecol/E092/066/EPBweb_Links.txt" %>%
  Graph.grabber(c("predation",
                  "Social Predation",
                  "predation on free-living non-feeding stage"),
                c("observed",
                  "inferred")) %>%
  Plot.foodweb
```

## Let's do it better!

```{r, warning = FALSE, message = FALSE}
"http://esapubs.org/Archive/ecol/E092/066/EPBweb_Links.txt" %>%
  Graph.grabber("predation","observed") %>%
  Tester(100)
```