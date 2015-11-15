# ---
# title: "Web! Can you spell it?"
# author: "Giulio Valentino Dalla Riva"
# date: "November 19, 2015"
# output:
#   ioslides_presentation:
#     widescreen: true
#     smaller: true
# subtitle: An NZES powered workshop
# ---

# The dialects of networkland

## Which one do you speak?

```{r,echo=F,warning=FALSE,message=FALSE,error=FALSE}
library("igraph") # To handle the webs objects and plot them

options(stringsAsFactors = FALSE) # Ask me why
```

- Edge list
- Adjacency list
- Adjacency matrix
- ...

## R speaks in tongues

```{r, eval=FALSE}
library("igraph") # We will use it a lot
read.graph(file,
           format = c("edgelist", "pajek",
                      "ncol", "lgl", "graphml",
                      "dimacs","graphdb", "gml", "dl"), ...)
```

## Edge list {.columns-2}

Read the web as:

- a is adjacent to b.
- a is adjacent to c.
- b is adjacent to a.
- b is adjacent to b.
- c is adjacent to b.

And write it as:

```{r, eval=FALSE}
Edge.list <- matrix(c("a","b","a","c",
                      "b","a","b","b",
                      "c","b"),ncol=2,
                      byrow=TRUE)
```

## Edge list {.columns-2}

Which translates to:

```{r, eval=TRUE}
Edge.list <- matrix(c("a","b","a","c",
                      "b","a","b","b",
                      "c","b"),ncol=2,
                      byrow=TRUE)
Edge.list
```

## Adjacency matrix

Read the web as:

- a is adjacent to a? no; to b? yes; to c? yes.
- b is adjacent to a? yes; to b? yes; to c? no;
- c is adjacent to a? no; to b? yes; to c? no.



## Adjacency matrix

And write it as:

```{r, eval=TRUE}
Adjacency.matrix <- matrix(c(0,1,1,
                             1,1,0,
                             0,1,0),
                           nrow=3,ncol=3,
                           byrow=TRUE)
row.names(Adjacency.matrix) <- colnames(Adjacency.matrix) <- c("a","b","c")
```

Which translates to:

```{r, eval=TRUE}
Adjacency.matrix
```

## Into igraph | Edge list

igraph can read those webs easily:

```{r,echo=T,warning=FALSE,message=FALSE,error=FALSE}
graph.data.frame(Edge.list,directed=TRUE) -> Edge.web
```

The order matters: FROM -> TO.

```{r}
graph.adjacency(Adjacency.matrix,mode="directed") -> Adjacency.web
```

Notice we have to use two different functions.

## Into igraph | Edge list

```{r, eval=TRUE}
plot(Edge.web)
```

## Into igraph | Adjacency matrix

```{r}
plot(Adjacency.web)
```