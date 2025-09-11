---
  title: "Estatística em Psicologia (MATC65)"
author: 
  - Raydonal Ospina <a href='mailto:raydonal@castlab.org'> `r fontawesome::fa(name = 'envelope-circle-check', fill = 'white')`</a>&nbsp;&nbsp; , &nbsp;&nbsp; 
- André Leite <a href='mailto:leite@castlab.org'>`r fontawesome::fa(name = 'envelope-circle-check', fill = 'white')`</a>&nbsp;&nbsp; & &nbsp;&nbsp; 
- Cristiano Ferraz <a href='mailto:cferraz@castlab.org'>`r fontawesome::fa(name = 'envelope-circle-check', fill = 'white')`</a>
  ratio: 16x10
maketitle: false
output:
  rmdshower::shower_presentation:
  self_contained: false
katex: true
theme: material
css: www/styles.css
highlight: espresso
fig_retina: 1
includes:
  in_header: www/to_header.html
in_footer: www/to_footer.html
bibliography: refs.bib
csl: apa.csl
header-includes:
  - \usepackage{tikz}       
- \usepackage[english]{babel}
- \usepackage[latin1]{inputenc}
- \usepackage{amsmath,amsthm}
- \usepackage{amssymb,latexsym}
- \usepackage{graphics}
- \usepackage{graphicx}
- \usepackage{amscd,eufrak}
- \usepackage{placeins}
- \usepackage{setspace}
- \usepackage{url}
- \usepackage{color}
- \setcounter{secnumdepth}{-1}
- \usepackage{pgfplots}
- \usepackage{subfigure}
- \usepackage{geometry}
- \usepackage{chngcntr}
- \onehalfspacing
- \counterwithin{figure}{section}
- \counterwithin{table}{section}  
---
  
  \def\P{\mathsf{\sf P}}
\def\E{\mathsf{\sf E}}
\def\Var{\mathsf{\sf Var}}
\def\Cov{\mathsf{\sf Cov}}
\def\std{\mathsf{\sf std}}
\def\Cor{\mathsf{\sf Cor}}
\def\R{\mathbb{R}}
\def\c{\,|\,}
\def\bb{\boldsymbol}
\def\diag{\mathsf{\sf diag}}
\def\defeq{\stackrel{\tiny\text{def}}{=}}

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      fig.width  = 8,
                      fig.height = 4,
                      fig.cap = '',
                      fig.align = 'center')


options(knitr.table.format = "html",
        show.signif.stars = TRUE)


knitr::knit_hooks$set(wrap = function(before, options, envir){
  if (before){
    paste0('<', options$wrap, '>')
  } else {
    paste0('</', options$wrap, '>')
  }
})



library(leaflet)
library(knitr)
library(tidyverse)
library(haven)
#library(kableExtra)
library(htmltools)
#library(webshot) # Desnecessário
#library(gm)
library(kableExtra)
library(ggrepel)
library(lubridate)
library(GGally)
library(mosaicData)
library(paletteer)
library(tidymodels)
library(fastDummies)
library(htmltools)
library(DT)
library(cowplot)
library(gghalfnorm)
library(showtext)
library(fontawesome)
library(car)
library(patchwork)
library(scales)
library(viridis)
library(ggrepel)

library(infer)
library(moderndive)


font_add_google(name = "Montserrat", family = "montserrat")
font_add_google(name = "Jost", family = "jost")
font_add_google(name = "Manjari", family = "manjari")


stcol <- function(x, color = "#3c763d", ontouchend="") {
  paste0("<strong ontouchend='", ontouchend,  "' style='color:",color,"'>",x,"</strong>")
}

## Set PDF links

pdflinks = 1;
```

```{r counters, echo=FALSE}
#Determine the output format of the document
#outputFormat   = opts_knit$get("rmarkdown.pandoc.to")

slideNumber <- 1
slideTotal <- 1

mathCounter <- list(`Observação` = 1,
                    `Exemplo` = 1,
                    `Definição` = 1,
                    `Lema` = 1L, # Lemma
                    `Corolário` = 1L, # Corollary
                    `Proposição` = 1L, # Proposition
                    `Teorema` = 1L) # Theorem


capMath <- function(type = "Lema"){
  stopifnot(type %in% c('Observação', 'Exemplo', 'Definição', 
                        'Lema', 'Proposição','Corolário', 'Teorema'))
  #if(outputFormat == 'html'){
  x = paste0(type, " ", mathCounter[[type]])
  mathCounter[[type]] <<- mathCounter[[type]] + 1L
  #}; x
  return(x)  
}

slideNumber <- 1
slideTotal <- read_lines("data/total_slides.dat", n_max = 1)

setNumber <- function(){
  #if(outputFormat == 'html'){
  x = paste0("<div id='rodape'>", slideNumber, " de ", slideTotal, "</div>")
  slideNumber <<- slideNumber + 1L
  #}; x
  return(x)
} 
```

# Basics in R {.off}
<!-- <div style="font-size:17%;"> -->
  <!-- <p style="margin: 03em 0;"></p>  -->
  <!-- ```{r first-timer, echo=FALSE} -->
  <!--  # solarized <- xaringanthemer:::solarized -->
  <!-- countdown( -->
                    <!--   minutes = 25, seconds = 00, -->
                    <!--  bottom = "2.5%", left  = "80%", -->
                    <!--   play_sound = TRUE -->
                    <!--   # color_border              = solarized$magenta, -->
                    <!--   # color_text                = solarized$magenta, -->
                    <!--   # color_running_background  = solarized$cyan, -->
                    <!--   # color_running_text        = solarized$base02, -->
                    <!--   # color_finished_background = solarized$red, -->
                    <!--   # color_finished_text       = solarized$base3 -->
                    <!-- ) -->
  <!-- ``` -->
  <!-- </div> -->
  <center>
  <img src="img/R_logo.png" width="50%">
  </center>
  
  
  
  ## What is `R`? {.off}
  ⊲ `S` is a high-level language for manipulating and displaying data. It forms the basis of two
highly acclaimed and widely used data analysis software systems, the commercial `S-PLUS`
and the Open Source `R`. (Venables and Ripley, 2000, p.v).

⊲ `R` is `GNU S` - A language and environment for statistical computing and graphics. `R` is
similar to the award-winning `S` system, which was developed at Bell Laboratories by John
Chambers et al. It provides a wide variety of statistical and graphical techniques. 

⊲ In short: (i) `R` is based on the `S` high-level programming language; (ii) `R` is <strong style="color:OrangeRed"> sexy</strong>. It's free and open
source; (iii) `R` is similar in many aspected to the commercial software `S-PLUS`.

## {.off}
⊲ The `R` main page on the web is http://www.R-project.org , from which the binaries
for different operating systems and the documentation can be downloaded. One can also
download the source code of `R`.

⊲ There are also many additional packages available from the `R` web page; they can be installed
in order to expand the capabilities of `R` in some desired directions.

⊲ `R` is developed by a ‘core TOP team’ which currently consists of: _Douglas Bates, John Chambers,
Peter Dalgaard, Set Falcon, Robert Gentleman, Kurt Hornik, Stefano Iacus, Ross Ihaka,
Friedrich Leisch, Thomas Lumley, Martin Maechler, Duncan Murdoch, Paul Murrell, Mar-
tyn Plummer, Brian Ripley, Deepayan Sarkar, Duncan Temple Lang, Luke Tierney, Simon
Urbanek and others._



## Why `R`? {.off}
- It's free!  (open-source)
- It runs on a variety of platforms including `Windows`, `Unix` and `MacOS`. 
- It contains advanced and updated statistical routines not yet available in other packages.  
- Programming language (not point-and-click)
- Excellent graphics - It has state-of-the-art graphics capabilities. 
- Offers broadest range (top and updated) of statistical tools
- Easy to generate reproducible reports 
- Easy to integrate with other tools

## The `R` Console {.off}
Basic interaction with `R` is through typing in the **console**
  
  This is the **terminal** or **command-line** interface

<center>
  <img src="img/terminal.png" width="58%">
  </center>
  `R` is designed to operate the way that problems are thought about and has very simple syntax.

## {.off}
- You type in commands, `R` gives back answers (or errors)
- Menus and other graphical interfaces are extras built on top of the console
- We will use **RStudio** 
  
  <center>
  <img src="img/RStudio-Logo.png" width="35%">
  </center>
  <br>
  
  **RStudio** is an IDE for `R`
`RStudio` has 4 main windows ('panes'):
  
  - Source
- Console
- Workspace/History
- Files/Plots/Packages/Help

##  {.off}
<center>
  <img src="img/Rstudioconsole.png" width="99%">
  </center>
  
  <!-- 1. Download `R`: https://cran.r-project.org/ -->
  <!-- 2. Download `RStudio`: http://www.rstudio.com/ -->
  
  
  <!-- # Setup -->
  
  ## `R` and `R`Studio
  <div style="font-size:75%;line-height:1.1;"> 
  To avoid problems, 
please start by updating your installation of R to 3.6.1 (2018-03-15, "Action of the Toes"). Installers for R 3.6.1 can be found here <https://cloud.r-project.org/>
  
  Download the most recent 
version of RStudio here <https://www.rstudio.com/products/rstudio/download/>.

### Library installation

In this tutorial, we will work with some libraries: 
  
  * `sp`: for working with vector data,
* `rgdal`: for importing and exporting vector data from other programs, 
* `raster`: for working with raster data,
* `tidyverse`: for data wrangling (optional).
* `EBImage`: for image processing - Bioconductor project 
</div>
  
  ## Library installation
  <div style="font-size:75%;line-height:1.0;">
  __On Windows:__

Just run: `install.packages(c("sp", "raster", "rgdal", "markovchain"))`

__On Mac:__

First download and install [GDAL complete](http://www.kyngchaos.com/files/software/frameworks/GDAL_Complete-1.11.dmg)

### Library installation

If everything went well, the following commands should run without problem!
  
  library(sp)
library(rgdal)
library(raster)
library(markovchain)
</div>
  
  
  
  
  
  ## Packages for `R`
  
  * There are a large number of specific packages developed to enhance the data analysis and
numerical capabilities of `R`; see http://cran.R-project.org . 

**Example:** 
  
  * To install the `tidyverse` package:
  `install.packages("tidyverse")`
* To update all installed packages:
  ` update.packages()`
Note: the computer must be connected to the Internet.
* To see the list of installed packages:
  `installed.packages()`
* Other resources:  
  
  `CRAN` task Views http://cran.at.r-project.org/web/views

## 
**Seeking help and more informations**
  
  * There are various ways to seek help on `R`. If you are searching for help on a specific function that is in a package loaded into your namespace: `?function_name`. If you're not sure what package it belongs to: `??function_name`

<center>
<img src="img/Fig4.png" width=65%;height="15%">
</center>


## Reproducible Research Basics

Presentation of research should include:

* Data
* Source code
* Analysis

Benefits:

* Research is reproducible by independent parties.
* Changes are made easily.
* Work habits are improved.

**Dynamic Documents in Elementary Statistics**

Use the `knitr` package that supports `LateX`, `HTML`, Markdown.



## {.off}
### `R` Markdown 
- `R` Markdown allows the user to integrate `R` code into a report (slides)
- When data changes or code changes, so does the report
- No more need to copy-and-paste graphics, tables, or numbers
- Creates __reproducible__ reports
- Anyone who has your `R` Markdown (.Rmd) file and input data can re-run your analysis and get the exact same results (tables, figures, summaries)
- Can output report in HTML (default), Microsoft Word, or PDF
- These course slides are also created in `RStudio` (`R` Presentation)





# Let's rock. `R` Basics:  the class in a nutshell

A function is a machine learning which turns input objects (**arguments**) into an output object (**return value**), possibly with **side effects**, according to a definite rule

- Everything we'll do comes down to applying **functions** to **data**
- **Data**:  things like 5, "five", $5.000$, the matrix $\left[ \begin{array}{ccc} 5 & 5 & 5 \\ 5 & 5 & 5\end{array}\right]$
- **Functions**: things like $\log{}$,  etc ( one arguments), $+$, $<$ , `mod` (two), `mean` (several)



## { .off}
### `R` data types
You'll encounter different kinds of data types

- **Booleans** Direct binary values: `TRUE` or `FALSE` in R
- **Integers**: whole numbers (positive, negative or zero)
- **Characters** fixed-length blocks of bits, with special coding;
**strings** = sequences of characters
- **Floating point numbers**: a fraction (with a finite number of bits) times an exponent, like $1.87 \times {10}^{6}$
  - **Missing or ill-defined values**: `NA`, `NaN`, etc.

## {.small-table}
### `R` as a calculator 
Command | Description
--------|-------------
  `+,-,*,\` | add, subtract, multiply, divide
`^` | raise to the power of
`%%` | remainder after division (ex: `8 %% 3 = 2`)
`log(), exp()` | logarithms and exponents (ex: `log(2) =  0.6931472`)
`sqrt()` | square root
`round()` | round to the nearest whole number (ex: `round(2.3) = 2`)
`floor(), ceiling()` | round down or round up 
`abs()` | absolute value
<!-- `( )` | change the order of operations -->
  
  ## 
  <div style='margin-left:30px;float:left;width:45%;margin-top:-35px;'>
  **Arithmetic operators** 
  ```{r echo=TRUE}
5 + 2 # Addition
5 - 2 # Subtraction
5 * 2 # Multiplication
5 / 2 # Division
5 %% 2 # Modulus
5 %/% 2 # Integer division 
```
</div>
  
  <div style='float:right;width:45%;margin-top:-35px;'>
  **Binary operators** (Boolean)
```{r echo=TRUE}
2 >  5; 2 <  5  
2 >= 5; 2 <= 5
2 == 5; 2 != 5 
(2 > 5) & (6*7 == 42)
(2 > 5) | (6*7 == 42)
```
</div>
  
  
  ##  
  ### More types
  
  - `typeof()` function returns the type
- `is.`_foo_`()` functions return Booleans for whether the argument is of type _foo_
- `as.`_foo_`()` (tries to) "cast" its argument to type _foo_ --- to translate it sensibly into a _foo_-type value
- **Special case**: `as.factor()` Numbers are actually encodings and not numeric values. (E.g., 1 = High school grad; 2 = College grad; 3 = Postgrad) 

<div style='margin-left:30px;float:left;width:85%'>
  ```{r echo=TRUE, eval=TRUE}
typeof(7);  is.numeric(7); is.na(7); is.character(7)
```
</div>
  
  
  ## 
  <div style='margin-left:15px;float:left;width:48%;margin-top:-35px;'>
  **Variables** 
  
  We can give names to data objects; these give us **variables**
  ```{r echo=TRUE}
pi
```
<br>
  Variables can be arguments to functions or operators, just like constants:
  ```{r echo=TRUE}
pi*10;  cos(pi)
```
</div>
  
  <div style='float:right;width:45%;margin-top:-35px;'>
  **Assignment operator** 
  
  You can use `<-` or `=` for designation.
```{r, echo=TRUE}
time.factor <- 7; time.factor
time.in.years = 3; 
time.in.years * time.factor
```
<br>
  _Using names and variables makes code: easier to design, easier to debug, less prone to bugs, easier to improve,  and easier for others to read_
</div>
  
  
  
  ## Data structure: vectors
  
  - Group related data values into one object, a **data structure**
  - A **vector** is a sequence of values, all of the same type
- `c()` function returns a vector containing all its arguments in order
- **Indexing**  `vec[j]` is the `j`-element of `vec`
- Operators apply to vectors "pairwise" or "elementwise"

<div style='margin-left:20px;float:left;width:95%'>
  ```{r, echo=TRUE, eval=TRUE}
students <- c("Alumni-01", "Alumni-02", "Alumni-03", "Alumni-04")
midterm <- c(80, 90, 93, 82) # midterm escores
final <- c(78, 84, 95, 82) # Final exam scores
course.grades <- 0.4*midterm + 0.6*final # Final course grade
course.grades[1]; course.grades[4]
```
</div>
  
  ## Functions on vectors {.small-table}
  Command | Description
--------|------------
  `sum(vec)` | sums up all the elements of `vec`
`mean(vec)`,  `median(vec)` | mean (median) of `vec`
`min(vec), max(vec)` | the largest or smallest element of `vec`
`sd(vec), var(vec)` | the standard deviation and variance of `vec`
`length(vec)` | the number of elements in `vec`
`sort(vec)` | returns the `vec` in sorted order
`order(vec)` | returns the index that sorts the vector `vec`
`unique(vec)` | lists the unique elements of `vec`
`summary(vec)` | gives a five-number summary  
`any(vec), all(vec)` | useful on Boolean vectors
`which()` | returns the `TRUE` indexes of a Boolean vector

## 
Operators apply to vectors "pairwise" or "elementwise":
  <div style='margin-left:20px;width:95%'>
  ```{r echo=TRUE, eval=TRUE}
a.threshold <- 90 # A grade = 90% or higher
course.grades >= a.threshold # vector of booleans
a.students <- which(course.grades >= a.threshold)
students[a.students] # Names of A students
```
</div>
  <br>
  You can give **names** to components of vectors
<div style='margin-left:30px;width:95%'>
  ```{r, echo=TRUE, eval=TRUE}
names(course.grades) <- students # Assign names to the grades
names(course.grades)
course.grades[c("Alumni-01", "Alumni-03")] # Get final grades for 2 students
```
</div>
  
  ## More `R` variables 
  
  * The most comfortable and familiar class/data type for many of you will be `data.frame`
* You can think of these as essentially spreadsheets with rows and columns
* `data.frame`s are somewhat advanced objects in `R`; we will start with simpler objects;
<!-- * Here we introduce "1 dimensional" classes; often referred to as 'vectors' -->
  * Vectors can have multiple sets of observations, but each observation has to be the same class.

<div style='margin-left:20px;float:left;width:95%'>
  ```{r echo=TRUE}
x = c(students, final);  class(x);  y <-  "hello world!"
print(y);  class(y)
```
</div>
  
  ## Matrices {.off .slide}
  * Matrix: two-dimensional data, composed of rows and columns. Unlike data frames, the entire matrix is composed of one R class, e.g. all numeric or all characters.
* Matrices have two "slots" you can use to select data, which represent rows and columns, that are separated by a comma, so the syntax is `matrix[row,column]`. 

<div style='margin-left:20px;float:left;width:95%'>
  ```{r matrix, echo=TRUE}
n = 1:9;  mat = matrix(n, nrow = 3); mat; mat[1,1]
```
</div>
  
  ## Lists 
  <div style='margin-left:20px;float:left;width:95%;font-size:85%;line-height:1.1;'>
  * One other data type that is the most generic are `lists` and can be created using `list()` and hold vectors, strings, matrices, models, list of other list, lists upon lists!
  * Can reference data using `$` (if the elements are named), or using `[]`, or `[[]]`
```{r  echo=TRUE}
mlst <- list(letrs=c("A", "b"), matrix(1:4, ncol=2)); head(mlst)
```
</div>
  
  ## {.off}
  ### Useful RStudio tips
  Keystroke | Description
----------|-------------
  `<tab>` | autocompletes commands and filenames, and lists arguments for functions. Highly useful!
  `<up>` | cycle through previous commands in the console prompt
`<ctrl-up>` | lists history of previous commands matching an unfinished one
`<ctrl-enter>` | paste current line from source window to console. Good for trying things out ideas from a source file.
`<ESC>` | as mentioned, abort an unfinished command and get out of the + prompt



## {.fullpage .grad}

:::{.topic}
<span style='color:black'>ET624 --- </span></br> Estatística Descritiva
:::
  
  ## Estatística Descritiva
  <img src="images/TypesStats.svg" width='800px'></img>
  
  <!-- Número do Slide -->
  
  
  
  ## Medidas de Tendência Central
  
  :::{.text}
Represenrtão o conjunto de dados com um único valor. Nos dá a **localização** dos pontos centrais. Existem três medidas principais:  
  :::
  
  :::{escalas}
<img src="images/Location.svg" width='800px'></img>
  :::
  
  <!-- Número do Slide -->
  
  
  
  ## Medidas de Variabilidade
  
  :::{.text}
Medidas de variabiliddade são utilizadas para medir a **dispersão** (ou variabilidade, volatilidade, espalhamento) dos dados. Ou ainda como os dados estão distribuídos.  As medidas mais comuns são:
  :::
  
  
  :::{.escalas}
<img src="images/Dispersion.svg" width='800px'></img>
  :::
  
  <!-- Número do Slide -->
  
  
  
  
  
  ## Estatística Descritiva
  
  
  :::{.text .stat}

```{r}

#                               
initJS <- c("function(settings, json) {",
            "$(this.api().table().header()).css({'font-size':'16px'});",
            "$(this.api().table().body()).css({'font-size':'16px'});",
            "}")

tibble(
  Tipo = c("Medidas de Centralidades", "", "", "Medidas de Dispersão","", "", ""),
  `Estatística` = c("Média", "Médiana", "Moda", "Variância", "Desvio  padrão", "Amplitude", "Frequência"),
  `Descrição`  = c("Total dos valores dividido pelo número de valores", "Ordenar valores e determinar ponto central", "Examinar valores e determinar valor que aparece com mais frequência", "Total do quadrado dos desvios em relação à média e dividir pelo total de valores menos 1.", "Raiz quadrada da variância", "Diferença entre valor máximo e mínimo", "contar o número de ocorrências de cada valor"),
  `Objetivo`  = c("Descrever os valores usando o valor médio", "Determinar o valor central entre todos os valores. Importante na presença de <em>outliers</em>", "Descreve o valor mais comum", "Determinha a dispersão dos dados", "Fornece um indicador de quanto os valores diferem do valor médio", "Indicador geral de dispersão", "Número de vezes que cada valor ocorre")) %>% 
  DT::datatable(rownames = FALSE,
                escape = FALSE,
                callback = JS("
                              $('table.dataTable.no-footer').css({
                                  'border-bottom':'solid 2px black',
                                  'border-top':'solid 2px black'
                                });
                                $('tr td').css({
                                      'border':'none',
                                      'vertical-align':'middle'
                                    });
                              "),
                options = list(autoWidth = FALSE,
                               fixedColumns = TRUE,
                               dom = 't',
                               initComplete = JS(initJS),
                               columnDefs = list(list(className = 'dt-left', targets = 0:3),
                                                 list(bSortable = FALSE, targets = 0:3)),
                               #                 rowCallback=JS(
                               #                 'function(row,data) {
                               #                    if($(row)["0"]["_DT_RowIndex"] % 2 <1) 
                               #                    $(row).css("background","#edf4f5") //#f2f9ec
                               #                 }')
                               rowCallback=DT::JS("
                                function(row,data) {
                                  $('tr').css({
                                      'border':'0px solid white',
                                      'vertical-align':'middle'
                                    });
                                }")
                               
                ),    
                class = "compact", style="default" ) %>% 
  formatStyle(columns = 1:4, border = "none", 'vertical-align' = 'middle' ) #%>% 
#formatStyle(c(3), 'border-left' = "solid 1px") 
```

:::
  <!-- Número do Slide -->
  
  
  
  
  ## Leitura dos dados
  
  :::{.text}
```{r, echo=TRUE}
library(tidyverse) # Obs: necessita instalação primeiro
dados = read_csv("data/Pre, Post, Experimental, Control.csv") # Leitura
dados %>% glimpse() # Verificação rápida dos dados
```
:::
  
  
  <!-- Número do Slide -->
  
  
  
  ## Variáveis Nominais
  
  :::{.text}
Na base de dados temos as seguintes variáveis nominais: `group`, `timing` e `CG or TG`.

::::{class="row _milk"}


:::{class="column_milk3"}

```{r, echo=TRUE}
dados %>% 
  count(group)
```
:::
  
  :::{class="column_milk3"}

```{r, echo=TRUE}
dados %>% 
  count(timing)
```
:::
  
  :::{class="column_milk3"}

```{r, echo=TRUE}
dados %>% 
  count(`CG or TG`)
```
:::
  
  :::{class="column_milk"}

```{r, echo=TRUE}
dados %>% count(group, `CG or TG`)
```
:::
  
  :::{class="column_milk"}

```{r, echo=TRUE}
table(dados$group, dados$`CG or TG`)
```

:::
  
  ::::
  
  :::
  
  
  <!-- Número do Slide -->
  
  
  
  ## Variáveis Nominais
  
  
  ::::{class="row _milk" .text}


:::{class="column_milk"}

```{r, echo=TRUE, fig.width= 4}
ggplot(dados) + 
  geom_bar(aes(x = group))
```

:::
  
  :::{class="column_milk"}

```{r, echo=TRUE, fig.width= 4}
ggplot(dados) + 
  geom_bar(aes(x = timing))
```
:::
  
  ::::
  
  <!-- Número do Slide -->
  
  
  ## Variáveis Nominais
  
  ```{r, echo=TRUE, fig.width= 8}
ggplot(dados) + 
  geom_bar(aes(x = timing, fill = timing)) + 
  facet_wrap(~group)
```

<!-- Número do Slide -->
  
  
  
  ## Moda
  
  :::{.mlkinfobox .textbox}
Em um conjunto de dados, a **moda** é aquele resultado mais recorrente no conjunto, ou seja, com maior frequência absoluta.
:::
  
  :::{.text}
Qual é a `r stcol("moda")` das vaiáveis `timing` e `group`?
  :::
  
  
  ::::{class="row _milk" .text}


:::{class="column_milk"}

```{r, echo=TRUE}
dados %>% count(group, sort = TRUE)
```

:::
  
  :::{class="column_milk"}

```{r, echo=TRUE, fig.width= 4}
dados %>% count(timing, sort = TRUE)
```
:::
  
  ::::
  
  
  <!-- Número do Slide -->
  
  
  ## Variáveis Ordinas
  
  :::{.text}
Nas variaveis `Q1` a `Q18` foi usada uma **escala de Likert** e portanto são variáveis `r stcol("ordinais")`.
:::
  
  ::::{class="row _milk" .text}


:::{class="column_milk"}

```{r, echo=TRUE}
dados %>% count(Q1, sort = TRUE)
```

:::
  
  :::{class="column_milk"}

```{r, echo=TRUE, fig.width= 4}
ggplot(dados) + geom_bar(aes(x = Q1))
```
:::
  ::::
  
  
  
  <!-- Número do Slide -->
  
  
  ## Variáveis Ordinas
  
  ```{r, echo=TRUE, fig.width= 9}
dados %>% 
  mutate(timing = factor(timing, levels = c("pre", "post"))) %>%
  ggplot() + 
  geom_bar(aes(x = Q1, fill = timing)) + 
  facet_wrap(~group+timing)
```

<!-- Número do Slide -->
  
  
  
  