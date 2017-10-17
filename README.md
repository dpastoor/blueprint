[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/astrazeneca/blueprint?branch=master&svg=true)](https://ci.appveyor.com/project/astrazeneca/blueprint)

[![Travis build
status](https://travis-ci.org/astrazeneca/blueprint.svg?branch=master)](https://travis-ci.org/astrazeneca/blueprint)

[![Coverage
status](https://codecov.io/gh/astrazeneca/blueprint/branch/master/graph/badge.svg)](https://codecov.io/github/astrazeneca/blueprint?branch=master)

BLUEPRINT
=========

Create a model blueprint, using a consistent API across modeling
platforms.

    library(blueprint)
    blueprint <- Blueprint$new("nonmem")

    ## initializing new blueprint of type: nonmem

Blueprints are combined with templates (see below for specifics) to
scaffold out model control streams.

There are default templates for `nonmem` and `mrgsolve`, however
arbitrary user defined templates can be easily used as well. The first
template is the compartmental template, which supports the various
compartment models, eg 1/2 cmt iv/oral etc.

    templates <- load_templates("nonmem")
    models <- available_models("nonmem")

The specific template and model will need to be bound to the blueprint
model to render, however this is dynamic, as to generate different model
types easily.

Core Concepts
-------------

Blueprint provides a pipeline-based API for declaring the model
components. In general, the nomenclature draws from a general
terminology to NLMEM. Specifically, there are constants, parameters
(thetas) and hierarchies (omega/sigma). These constants, parameters, and
hierarchies are combined together to inform the model structure.

The required components for each piece are as minimal as possible,
generally a value and a name, though this also is extended to
incorporate other settings common, such as lower and upper bound, and
whether the element should be fixed, comments (such as units).

### parameters

#### basic declaration

    cl <- parameter(1.5)
    cl

    ## $name
    ## NULL
    ## 
    ## $value
    ## [1] 1.5
    ## 
    ## $comment
    ## NULL
    ## 
    ## $lower_bound
    ## [1] -Inf
    ## 
    ## $upper_bound
    ## [1] Inf
    ## 
    ## $fixed
    ## [1] FALSE
    ## 
    ## $covariate_relationships
    ## NULL
    ## 
    ## $link
    ## NULL
    ## 
    ## attr(,"class")
    ## [1] "parameter"

#### full declaration

    cl <- parameter(1.5, 
      "CL",
      lower_bound = 0, 
      upper_bound = 100,
      fixed = FALSE,
      link = "CL"
    )
    cl

    ## $name
    ## [1] "CL"
    ## 
    ## $value
    ## [1] 1.5
    ## 
    ## $comment
    ## NULL
    ## 
    ## $lower_bound
    ## [1] 0
    ## 
    ## $upper_bound
    ## [1] 100
    ## 
    ## $fixed
    ## [1] FALSE
    ## 
    ## $covariate_relationships
    ## NULL
    ## 
    ## $link
    ## [1] "CL"
    ## 
    ## attr(,"class")
    ## [1] "parameter"

### hierarchies

hierarchies can either be diagonal or block elements. Diagonal elements
are specified via `omega_param` and block elements via `block`. The
hierarchical parameter is **link**ed to a parameter via a name.

    b1 <- block(0.04, 0.01, 0.09, param_names = c("CL", "V"))
    b1

    ## $block
    ## [1] TRUE
    ## 
    ## $params
    ## [1] "CL" "V" 
    ## 
    ## $fix
    ## [1] FALSE
    ## 
    ## $correlation
    ## [1] FALSE
    ## 
    ## $matrix
    ##      [,1] [,2]
    ## [1,] 0.04 0.01
    ## [2,] 0.01 0.09
    ## 
    ## $value
    ## [1] "\n4.000000e-02 \n1.000000e-02 9.000000e-02"
    ## 
    ## $num_params
    ## [1] 2
    ## 
    ## $comment
    ## NULL
    ## 
    ## attr(,"class")
    ## [1] "block"

Information about the raw matrix, as well as the resultant structure is
stored.

    b1$matrix

    ##      [,1] [,2]
    ## [1,] 0.04 0.01
    ## [2,] 0.01 0.09

    cat(b1$value)

    ## 
    ## 4.000000e-02 
    ## 1.000000e-02 9.000000e-02

### Helpers

In addition to the model structure, a number of helpers to track and
inject other information required by the softwares are available.

-   `with_data` - dataset to be modeled, can extract column names,
    calculations, etc
-   `from_path` - path to data to inject into the input of a control
    stream.

Basic example
-------------

    one_cmpt_iv <- blueprint %>% 
      # use the one cmt iv template
      use_template(templates$compartmental) %>%
      model_type(models$one_cmt_iv) %>%
      # add clearance and volume parameters
      parameters(CL = parameter(5,  
                        lower_bound = 0.01, 
                        upper_bound = 10),
             V = parameter(34, lower_bound = 0.1)) %>%
      with_data(head(Theoph)) %>%
      from_path("path/to/my_data.csv") 

    one_cmpt_iv %>% render() %>% cat()

    ## $PROBLEM   
    ## $SUB ADVAN1 TRANS2
    ## $INPUT Subject Wt Dose Time conc
    ## $DATA path/to/my_data.csv
    ## IGNORE=@
    ## $PK
    ## 
    ## ; constants
    ## 
    ## 
    ##  CL = THETA(1)
    ##  V = THETA(2)
    ## $THETA
    ## (0.01 , 5, 10)
    ## (0.1 , 34, Inf)
    ## 
    ## 
    ## 
    ## 
    ## $SIGMA
    ## 
    ## 
    ## 
    ## $ERROR
    ## IPRED=F
    ## Y = IPRED

    one_cmt_block <- one_cmpt_iv %>%
      hierarchies(b1 = block(0.04, 0.01, 0.09, param_names = c("CL", "V")))

    one_cmt_block %>% render() %>% cat()

    ## $PROBLEM   
    ## $SUB ADVAN1 TRANS2
    ## $INPUT Subject Wt Dose Time conc
    ## $DATA path/to/my_data.csv
    ## IGNORE=@
    ## $PK
    ## 
    ## ; constants
    ## 
    ## 
    ##  TVCL = THETA(1)
    ##  TVV = THETA(2)
    ##  CL = TVCL*EXP(ETA(1))
    ##  V = TVV*EXP(ETA(2))
    ## $THETA
    ## (0.01 , 5, 10)
    ## (0.1 , 34, Inf)
    ## 
    ## 
    ## $OMEGA BLOCK(2)
    ## 4.000000e-02 
    ## 1.000000e-02 9.000000e-02
    ## 
    ## 
    ## $SIGMA
    ## 
    ## 
    ## 
    ## $ERROR
    ## IPRED=F
    ## Y = IPRED

    one_cmt_diag <- one_cmpt_iv %>%
      hierarchies(CL = 0.04, V = 0.09)

    one_cmt_diag %>% render() %>% cat()

    ## $PROBLEM   
    ## $SUB ADVAN1 TRANS2
    ## $INPUT Subject Wt Dose Time conc
    ## $DATA path/to/my_data.csv
    ## IGNORE=@
    ## $PK
    ## 
    ## ; constants
    ## 
    ## 
    ##  TVCL = THETA(1)
    ##  TVV = THETA(2)
    ##  CL = TVCL*EXP(ETA(1))
    ##  V = TVV*EXP(ETA(2))
    ## $THETA
    ## (0.01 , 5, 10)
    ## (0.1 , 34, Inf)
    ## 
    ## 
    ## $OMEGA
    ## 0.04
    ## $OMEGA
    ## 0.09
    ## 
    ## 
    ## $SIGMA
    ## 
    ## 
    ## 
    ## $ERROR
    ## IPRED=F
    ## Y = IPRED

    one_cmt_block %>%
      residual_error(ADD = 1) %>%
      render() %>% cat()

    ## $PROBLEM   
    ## $SUB ADVAN1 TRANS2
    ## $INPUT Subject Wt Dose Time conc
    ## $DATA path/to/my_data.csv
    ## IGNORE=@
    ## $PK
    ## 
    ## ; constants
    ## 
    ## 
    ##  TVCL = THETA(1)
    ##  TVV = THETA(2)
    ##  CL = TVCL*EXP(ETA(1))
    ##  V = TVV*EXP(ETA(2))
    ## $THETA
    ## (0.01 , 5, 10)
    ## (0.1 , 34, Inf)
    ## 
    ## 
    ## $OMEGA BLOCK(2)
    ## 4.000000e-02 
    ## 1.000000e-02 9.000000e-02
    ## 
    ## 
    ## $SIGMA
    ## 1  ; ADD
    ## 
    ## 
    ## 
    ## $ERROR
    ## IPRED=F
    ## Y = IPRED + EPS(1)

    one_cmt_block %>%
      residual_error(ADD = 1, PROP = 0.1) %>%
      render() %>% cat()

    ## $PROBLEM   
    ## $SUB ADVAN1 TRANS2
    ## $INPUT Subject Wt Dose Time conc
    ## $DATA path/to/my_data.csv
    ## IGNORE=@
    ## $PK
    ## 
    ## ; constants
    ## 
    ## 
    ##  TVCL = THETA(1)
    ##  TVV = THETA(2)
    ##  CL = TVCL*EXP(ETA(1))
    ##  V = TVV*EXP(ETA(2))
    ## $THETA
    ## (0.01 , 5, 10)
    ## (0.1 , 34, Inf)
    ## 
    ## 
    ## $OMEGA BLOCK(2)
    ## 4.000000e-02 
    ## 1.000000e-02 9.000000e-02
    ## 
    ## 
    ## $SIGMA
    ## 1  ; ADD
    ## 0.1  ; PROP
    ## 
    ## 
    ## 
    ## $ERROR
    ## IPRED=F
    ## Y = IPRED*(1+IPRED*EPS(2)) + EPS(1)

Extending templates
-------------------

One of the most powerful components of blueprint, is the fact that the
built-in templates are just starting points, and can easily be extended.

    bp <- Blueprint$new("nonmem")

    ## initializing new blueprint of type: nonmem

In this case, a model has already been established, but would like to
use blueprint to inject the parameter estimates

In this case, blueprint understands how to inject the theta and omega
values, in the portion of the template below

    {{#parameters}}
    {{> theta}}
    {{/parameters}}

    {{#omegas}}
    {{> omega}}
    {{/omegas}}

    bp$template <- "
    $PROBLEM
    $INPUT ... omitted for simplicity
    $DATA ...omitted for simplicity
    $SUBROUTINE ADVAN2 TRANS2

    $PK
    TVF1 = THETA(1)
    F1 = TVF1*EXP(ETA(1))
    TVALAG1 = THETA(2)
    ALAG1 = TVALAG1*EXP(ETA(2))
    TVKA = THETA(3)
    KA = TVKA*EXP(ETA(3))
    TVCL = THETA(4)
    CL = TVCL*EXP(ETA(4))
    TVV = THETA(5)
    V = TVV*EXP(ETA(5))
    S2 = V/1000

    $ERROR
    IPRED=FW=IPRED
    ; Specifications for M3 method
    LLOQ = 1.0 ;LLOQ = 1 ng/mL
    SD = SQRT(SIGMA(1,1))
    DUM=( LLOQ-IPRED ) / SD ;Dummy variable defining distribution
    CUMD=PHI(DUM);Cumulative distribution of DUM

    IF(BLQ.EQ.0.OR.NPDE_MODE.EQ.1) THEN ; 0 for measured sample
    F_FLAG=0
    Y = IPRED * (1+EPS(1))
    ENDIF

    IF(BLQ.EQ.1.AND.NPDE_MODE.EQ.0) THEN
    F_FLAG=1
    Y=CUMD
    MDVRES=1
    ENDIF

    IF(BLQ.EQ.1) DV_LOQ=LLOQ

    $THETA
    {{#parameters}}
    {{> theta}}
    {{/parameters}}

    {{#omegas}}
    {{> omega}}
    {{/omegas}}

    $EST ...omitted for simplicity
    "

    bp %>% parameters(
      F1 = parameter(0.8, upper_bound = 1),
      ALAG1 = parameter(0.5, lower_bound = 0, comment = "lag time; hours"),
      KA = parameter(1, comment = "1/hr"),
      CL = parameter(1.4, lower_bound = 0.1, comment = "L/hr"),
      V = parameter(32.5, lower_bound = 0.1, comment = "L")
    ) %>%
      hierarchies(F1 = 0.1,
                  ALAG1 = 0.1,
                  KA = 0.1,
                  CL_V = block(0.1, 0.01, 0.1,
                               param_names = c("CL", "V"))) %>%
      render() %>% cat

    ## 
    ## $PROBLEM
    ## $INPUT ... omitted for simplicity
    ## $DATA ...omitted for simplicity
    ## $SUBROUTINE ADVAN2 TRANS2
    ## 
    ## $PK
    ## TVF1 = THETA(1)
    ## F1 = TVF1*EXP(ETA(1))
    ## TVALAG1 = THETA(2)
    ## ALAG1 = TVALAG1*EXP(ETA(2))
    ## TVKA = THETA(3)
    ## KA = TVKA*EXP(ETA(3))
    ## TVCL = THETA(4)
    ## CL = TVCL*EXP(ETA(4))
    ## TVV = THETA(5)
    ## V = TVV*EXP(ETA(5))
    ## S2 = V/1000
    ## 
    ## $ERROR
    ## IPRED=FW=IPRED
    ## ; Specifications for M3 method
    ## LLOQ = 1.0 ;LLOQ = 1 ng/mL
    ## SD = SQRT(SIGMA(1,1))
    ## DUM=( LLOQ-IPRED ) / SD ;Dummy variable defining distribution
    ## CUMD=PHI(DUM);Cumulative distribution of DUM
    ## 
    ## IF(BLQ.EQ.0.OR.NPDE_MODE.EQ.1) THEN ; 0 for measured sample
    ## F_FLAG=0
    ## Y = IPRED * (1+EPS(1))
    ## ENDIF
    ## 
    ## IF(BLQ.EQ.1.AND.NPDE_MODE.EQ.0) THEN
    ## F_FLAG=1
    ## Y=CUMD
    ## MDVRES=1
    ## ENDIF
    ## 
    ## IF(BLQ.EQ.1) DV_LOQ=LLOQ
    ## 
    ## $THETA
    ## (-Inf , 0.8, 1)
    ## (0 , 0.5, Inf)  ; lag time; hours
    ## (-Inf , 1, Inf)  ; 1/hr
    ## (0.1 , 1.4, Inf)  ; L/hr
    ## (0.1 , 32.5, Inf)  ; L
    ## 
    ## 
    ## $OMEGA
    ## 0.1
    ## $OMEGA
    ## 0.1
    ## $OMEGA
    ## 0.1
    ## $OMEGA BLOCK(2)
    ## 1.000000e-01 
    ## 1.000000e-02 1.000000e-01
    ## 
    ## 
    ## $EST ...omitted for simplicity

This templating leverages the
[whisker](https://github.com/edwindj/whisker) library to do the
templating injection.

Eventual extensions
-------------------

even more standard templates

query language sugar for templates

    blueprint %>%
      ode_model(cmt = 1, depot = 1) %>%
      parameterize("CL") 

-   shiny app to dynamically create control streams via GUI
-   expanding support for a generic ODE language interface
-   supporting more nonmem features
