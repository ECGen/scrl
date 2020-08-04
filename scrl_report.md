Analysis Summary
================

-   Dead trees were removed from lichen community analyses.
-   Rock epiphyte communities were adequately sampled, based on species
    accumulation curves, with moth resistant trees accumulating slightly
    more lichen species.
-   Communities (abundance, richness, diversity, composition) were
    significantly, generally negatively, affected by moth
    susceptibility.
-   Several tree variables, including light availability, leaf litter
    abundance and rock abundance, were impacted by moth susceptibility.
-   Analysis of causal pathways supported an indirect link between moth
    susceptibility and impacts on lichen communities via decreasing rock
    (i.e. habitat) availability through increased leaf abscission and
    accumulation on rocks under trees.
-   These results support a genetically based link between intraspecific
    variation in susceptibility to an insect herbivore and community
    dynamics in an arid ecosystem.
-   Given the possible impacts of cliimate change on this system, this
    study supports the conclusion that community and ecosystem impacts
    need to be considered in an evolutionary context.

<!-- -->

    # 0. Supporting functions and libraries
    ## Support functions

    dif <- function(x){
        out=x[1]
        for (i in 2:length(x)){
            out=out-x[i]
        }
        return(out)
    }

    se <- function(x){sd(x) / sqrt(length(x))}


    ## Libraries
    my.libs <- c("vegan", "ecodist", "knitr", "reshape2")
    if (any(!(my.libs %in% installed.packages()[, 1]))){
        sapply(my.libs[!(my.libs %in% installed.packages()[, 1])], 
               install.packages)
    }else{}
    sapply(my.libs, require, character.only = TRUE)

    # if (!(any(grepl("ComGenR", installed.packages()[, 1])))){
    #    devtools::install_github("ecgen/comgenr")
    # }

Load Data
=========

The following are variable descriptions (Variable, Type, Range,
Definition):

-   Moth,categorical,0 or 1,Was the tree susceptible (0) or
    resistant (1) to moth attack?
-   Live/Dead,categorical,0 or 1,Was the tree dead (0) or alive (1)?
-   Litter %,continuous,0 to 100,Percent cover inside quadrat
-   Rocks &gt; 3cm? %,continuous,0 to 100,Percent cover of rocks &gt;
    3cm? inside quadrat
-   Rocks &lt; 3cm? %,continuous,0 to 100,Percent cover of rocks &lt;
    3cm? inside quadrat
-   Shrubs %,continuous,0 to 100,Percent cover of shrubs inside quadrat
-   Grass %,continuous,0 to 100,Percent cover of grass inside quadrat
-   Branches %,continuous,0 to 100,Percent cover of branches on ground
    inside quadrat
-   Distance,continuous,0 to 100,“Distance from main trunk, converted to
    percent of crown radius at that azimuth”
-   Azimuth,continuous,0 to 360,Compass direction from main trunk
-   Slope,continuous,0 to 90,Topographical steepness
-   Aspect,continuous,0 to 360,Compass direction of slope
-   Light,continuous,,Amount of light available to epiliths

<!-- -->

    ## Data are in ../data/scrl
    l.dat <- read.csv("./data/spp_env_combined.csv")

    ## Fix species names
    colnames(l.dat)[colnames(l.dat) == "Acasup"] <- "Acaame"

    ## Summary of data
    summary(l.dat)

    ## remove dead trees
    l.dat <- l.dat[l.dat[, "Live.Dead"] != 0, ]

    ## Lichen species list
    spp.l <- c("Acacon", "Acaame", "Acaobp", "Sterile.sp", "Brown.cr",
    "Lobalp", "Canros", "Calare", "Phydub", "Rhichr", "Xanlin", "Xanpli",
    "Xanele", "GrBr.cr", "Gray.cr")
    spp.moss <- c("Synrur", "Cerpur.Bryarg")

    ## Create a community matrix
    com <- l.dat[, colnames(l.dat) %in% c(spp.l, spp.moss)]
    com.moss <- l.dat[, colnames(l.dat) %in% spp.moss]

    ## Add the tree labels to the rownames
    rownames(com) <- paste(l.dat[, "Moth"], l.dat[, "Tree.pairs"], sep = "_")
    rownames(com.moss) <- paste(l.dat[, "Moth"], l.dat[, "Tree.pairs"], sep = "_")
    rownames(l.dat) <- paste(l.dat[, "Moth"], l.dat[, "Tree.pairs"], sep = "_")

    ## Paired environmental differences
    total.rocks  <- apply(l.dat[, c("Big.rocks..", "Small.rocks..")], 1, sum)
    env <- l.dat[, c("Litter..", "Big.rocks..", "Small.rocks..", 
                               "Shrubs..", "Grass..", "Branches..", 
                               "Light...N", "Light...S", "Light...average")]
    env <- cbind(env, total.rocks)
    env.dif <- apply(env, 2, function(x, p) tapply(x, p, diff), p = l.dat[, "Tree.pairs"])

    heatmap(abs(round(cor(env.dif), 3)))

![](scrl_report_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Species accumulation
====================

Are the communities on each tree type adequately sampled?

    spa.all <- specaccum(com, method = "exact")
    spa.res <- specaccum(com[l.dat[, "Moth"] == 1, ], method = "exact")
    spa.sus <- specaccum(com[l.dat[, "Moth"] == 0, ], method = "exact")

    plot(spa.all,
         ylim = c(0, 20),
         xlab = "Cumulative Trees Sampled",
         ylab = "Species Observed", 
         col = "grey", ci.col = 'lightgrey', ci.type = "poly", ci.lty = 0)
    plot(spa.res, ci.col = "black", ci.type = "bar", lty = 1, add = TRUE, ci.lty = 1)
    plot(spa.sus, ci.col = "black", ci.type = "bar", lty = 3, add = TRUE, ci.lty = 3)
    legend("bottomright", 
           legend = c("All", "Resistant", "Susceptible"), 
           lty = c(1, 1, 3), lwd = c(5, 2, 2), col = c("lightgrey", "black", "black"))

![](scrl_report_files/figure-markdown_strict/specacum-1.png)

    pdf("./results/scrl_spp-accum.pdf", width = 5, height = 5)
    plot(spa.all,
         ylim = c(0, 20),
         xlab = "Cumulative Trees Sampled",
         ylab = "Species Observed", 
         col = "grey", ci.col = 'lightgrey', ci.type = "poly", ci.lty = 0)
    plot(spa.res, ci.col = "black", ci.type = "bar", lty = 1, add = TRUE, ci.lty = 1)
    plot(spa.sus, ci.col = "black", ci.type = "bar", lty = 3, add = TRUE, ci.lty = 3)
    legend("bottomright", 
           legend = c("All", "Resistant", "Susceptible"), 
           lty = c(1, 1, 3), lwd = c(5, 2, 2), col = c("lightgrey", "black", "black"))
    dev.off()

    ## png 
    ##   2

Moth trees have different microenvironments
===========================================

-   paired t-tests

Moth trees have different lichen communities (FIGURE ch.plot A, R, H)
=====================================================================

less abundant and diverse (paired t-tests, in text)

    abun <- apply(com, 1, sum)
    rich <- apply(com, 1, function(x) sum(sign(x)))
    shan <- apply(com, 1, diversity, index = "shannon")
    tt.a <- t.test(tapply(abun, l.dat[, "Tree.pairs"], diff))
    tt.r <- t.test(tapply(rich, l.dat[, "Tree.pairs"], diff))
    tt.h <- t.test(tapply(shan, l.dat[, "Tree.pairs"], diff))
    tt.arh <- do.call(rbind, 
                      list(a = unlist(tt.a), r = unlist(tt.r), h = unlist(tt.h)))
    data.frame(tt.arh)

    ##         statistic.t parameter.df             p.value          conf.int1
    ## a -2.24872719194069           29  0.0322967805096532  -2.94827641857598
    ## r -2.95490149904486           29 0.00615219062629224   -4.2867753443144
    ## h -2.44676815758056           29  0.0207112921139992 -0.802255887812151
    ##             conf.int2 estimate.mean.of.x null.value.mean            stderr
    ## a  -0.139723581424019             -1.544               0 0.686610632687508
    ## r  -0.779891322352267  -2.53333333333333               0 0.857332582541993
    ## h -0.0717134452340905 -0.436984666523121               0  0.17859667871239
    ##   alternative            method                                 data.name
    ## a   two.sided One Sample t-test tapply(abun, l.dat[, "Tree.pairs"], diff)
    ## r   two.sided One Sample t-test tapply(rich, l.dat[, "Tree.pairs"], diff)
    ## h   two.sided One Sample t-test tapply(shan, l.dat[, "Tree.pairs"], diff)

composition is different (PERMANOVA, in text and supplement)

    com.ds <- cbind(com, ds = rep(0.0001, nrow(com)))
    com.ds.rel <- apply(com, 2, function(x) x/max(x))
    com.ds.rel <- cbind(com.ds.rel, ds = rep(0.0001, nrow(com)))
    com.ds.rel[is.na(com.ds.rel)] <- 0

    set.seed(123)
    ptab.moth <- adonis2(com.ds~ Moth, data = l.dat, 
                        strata = l.dat[, "Tree.pairs"], 
                        by = "margin", nperm = 100000)
    set.seed(123)
    ptab.moth.rel <- adonis2(com.ds.rel ~ Moth, data = l.dat, 
                             strata = l.dat[, "Tree.pairs"], 
                             by = "margin", nperm = 100000)

    kable(ptab.moth)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Moth</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.8329281</td>
<td style="text-align: right;">0.0389768</td>
<td style="text-align: right;">2.352343</td>
<td style="text-align: right;">0.023</td>
</tr>
<tr class="even">
<td>Residual</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">20.5368939</td>
<td style="text-align: right;">0.9610232</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.3698219</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

    kable(ptab.moth.rel)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Moth</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.8791695</td>
<td style="text-align: right;">0.0405034</td>
<td style="text-align: right;">2.448363</td>
<td style="text-align: right;">0.021</td>
</tr>
<tr class="even">
<td>Residual</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">20.8269063</td>
<td style="text-align: right;">0.9594966</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.7060758</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

three main species were reduced by moths (FDR paired t-tests, in text +
supplement)

    ind.spp <- lapply(com, function(x, p) t.test(tapply(x, p, diff)), p = l.dat[, "Tree.pairs"])
    isp <- apply(do.call(rbind, lapply(ind.spp, unlist)), 2, as.numeric)

    ## Warning in apply(do.call(rbind, lapply(ind.spp, unlist)), 2, as.numeric): NAs
    ## introduced by coercion

    ## Warning in apply(do.call(rbind, lapply(ind.spp, unlist)), 2, as.numeric): NAs
    ## introduced by coercion

    ## Warning in apply(do.call(rbind, lapply(ind.spp, unlist)), 2, as.numeric): NAs
    ## introduced by coercion

    rownames(isp) <- names(ind.spp)
    isp[, "p.value"] <- p.adjust(isp[, "p.value"], method = "fdr")
    isp <- isp[order(isp[, "p.value"]), ]

Calculate the average abundances of the indicators

    isp.names <- as.character(na.omit(rownames(isp[isp[, "p.value"] < 0.05, ])))
    isp.com <- com[,colnames(com) %in% isp.names]
    isp.dif <- apply(isp.com, 2, function(x,y) tapply(x, y, diff), y = l.dat[ ,"Tree.pairs"])

Create a multi-bar plot figure for the community.

    isp.dat <- melt(isp.dif)
    colnames(isp.dat) <- c("Tree.pairs", "Species", "diff")
    isp.mu <- tapply(isp.dat[, "diff"], isp.dat[, "Species"], mean)
    isp.se <- tapply(isp.dat[, "diff"], isp.dat[, "Species"], se)
    ard.dif <- cbind(tapply(abun, l.dat[, "Tree.pairs"], diff), 
                     tapply(rich, l.dat[, "Tree.pairs"], diff), 
                     tapply(shan, l.dat[, "Tree.pairs"], diff))
    colnames(ard.dif) <- c("Abundance", "Richness", "Diversity")
    ard.dat <- melt(ard.dif)
    colnames(ard.dat) <- c("Tree.pairs", "Stat", "diff")
    ard.mu <- tapply(ard.dat[, "diff"], ard.dat[, "Stat"], mean)
    ard.se <- tapply(ard.dat[, "diff"], ard.dat[, "Stat"], se)

    pdf(file = "./results/scrl_isp_ard.pdf", width = 9, height = 5)

    par(mfrow = c(1,2))
    bp.out <- barplot(ard.mu, col = "darkgrey", ylim = c(-5, 0), 
                      ylab = "Difference (S - R)", border = "NA")
    segments(bp.out[, 1], ard.mu + ard.se,
             bp.out[, 1], ard.mu - ard.se, 
             lwd = 1.5)
    bp.out <- barplot(isp.mu, col = "darkgrey", ylim = c(-0.5, 0), 
                      ylab = "Difference (S - R)", border = "NA",
                axisnames = TRUE, 
                names.arg = sapply(names(isp.mu), 
                    function(x) 
                                        paste(c(substr(x, 1, 1), 
                                                substr(x, 4, 4)), collapse = "")))
    segments(bp.out[, 1], isp.mu + isp.se,
             bp.out[, 1], isp.mu - isp.se, 
             lwd = 1.5)
    dev.off()

    ## png 
    ##   2

Create a plot of the two most indicative species

    pdf(file = "./results/scrl_complot.pdf", width = 7, height = 7)
    plot(com[, c("Acaame", "Canros")], pch = l.dat[, "Moth"] + 1, cex = 3, col = l.dat[, "Moth"] + 1)
    legend("topleft", title = "Tree Type", legend = c("Resistant", "Susceptible"), pch = c(2, 1), col = c(2, 1))
    dev.off()

    ## png 
    ##   2

Create plot with indicator taxa

    pdf(file = "./results/scrl_pdif.pdf", width = 7, height = 7)
    plot(melt(isp.dif)[-1], xlab = "Species", ylab = "Abundance Reduction")
    dev.off()

    ## png 
    ##   2

Litter covering rocks was the main driver (FIGURE = ORDINATION)
===============================================================

Although light did significantly explain variation in the lichen
community, this was not significant once the variation in litter was
controlled for.

    set.seed(123)
    ptab.env <- adonis2(com.ds ~ Litter.. +  Light...average, data = l.dat, 
                       strata = l.dat[, "Tree.pairs"], 
                       by = "margin", nperm = 100000)
    kable(ptab.env)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Litter..</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">1.0035484</td>
<td style="text-align: right;">0.0469610</td>
<td style="text-align: right;">2.972456</td>
<td style="text-align: right;">0.007</td>
</tr>
<tr class="even">
<td>Light…average</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.4114619</td>
<td style="text-align: right;">0.0192543</td>
<td style="text-align: right;">1.218728</td>
<td style="text-align: right;">0.243</td>
</tr>
<tr class="odd">
<td>Residual</td>
<td style="text-align: right;">57</td>
<td style="text-align: right;">19.2441042</td>
<td style="text-align: right;">0.9005271</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.3698219</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

    set.seed(123)
    ptab.env <- adonis2(com.ds ~ Light...average + Litter.. , data = l.dat, 
                       strata = l.dat[, "Tree.pairs"], 
                       by = "margin", nperm = 100000)
    kable(ptab.env)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Light…average</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.4114619</td>
<td style="text-align: right;">0.0192543</td>
<td style="text-align: right;">1.218728</td>
<td style="text-align: right;">0.243</td>
</tr>
<tr class="even">
<td>Litter..</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">1.0035484</td>
<td style="text-align: right;">0.0469610</td>
<td style="text-align: right;">2.972456</td>
<td style="text-align: right;">0.007</td>
</tr>
<tr class="odd">
<td>Residual</td>
<td style="text-align: right;">57</td>
<td style="text-align: right;">19.2441042</td>
<td style="text-align: right;">0.9005271</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.3698219</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

    set.seed(123)
    ptab.env <- adonis2(com.ds ~ total.rocks ,
                       strata = l.dat[, "Tree.pairs"], 
                       by = "term", nperm = 100000)

    kable(ptab.env)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>total.rocks</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">1.664876</td>
<td style="text-align: right;">0.0779078</td>
<td style="text-align: right;">4.900435</td>
<td style="text-align: right;">0.002</td>
</tr>
<tr class="even">
<td>Residual</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">19.704946</td>
<td style="text-align: right;">0.9220922</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.369822</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

    set.seed(123)
    ptab.env <- adonis2(com.ds ~ Big.rocks.. , data = l.dat, 
                       strata = l.dat[, "Tree.pairs"], 
                       by = "term", nperm = 100000)

    kable(ptab.env)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Big.rocks..</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">2.428473</td>
<td style="text-align: right;">0.1136403</td>
<td style="text-align: right;">7.436188</td>
<td style="text-align: right;">0.001</td>
</tr>
<tr class="even">
<td>Residual</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">18.941349</td>
<td style="text-align: right;">0.8863597</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.369822</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

    set.seed(123)
    ptab.env <- adonis2(com.ds ~ Small.rocks.. , data = l.dat, 
                       strata = l.dat[, "Tree.pairs"], 
                       by = "term", nperm = 100000)
    kable(ptab.env)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Small.rocks..</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.2204425</td>
<td style="text-align: right;">0.0103156</td>
<td style="text-align: right;">0.604541</td>
<td style="text-align: right;">0.782</td>
</tr>
<tr class="even">
<td>Residual</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">21.1493794</td>
<td style="text-align: right;">0.9896844</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.3698219</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

    set.seed(123)
    ptab.env <- adonis2(com.ds ~ Litter.. , data = l.dat, 
                       strata = l.dat[, "Tree.pairs"], 
                       by = "term", nperm = 100000)
    kable(ptab.env)

<table>
<thead>
<tr class="header">
<th></th>
<th style="text-align: right;">Df</th>
<th style="text-align: right;">SumOfSqs</th>
<th style="text-align: right;">R2</th>
<th style="text-align: right;">F</th>
<th style="text-align: right;">Pr(&gt;F)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Litter..</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">1.714256</td>
<td style="text-align: right;">0.0802185</td>
<td style="text-align: right;">5.058457</td>
<td style="text-align: right;">0.002</td>
</tr>
<tr class="even">
<td>Residual</td>
<td style="text-align: right;">58</td>
<td style="text-align: right;">19.655566</td>
<td style="text-align: right;">0.9197815</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td>Total</td>
<td style="text-align: right;">59</td>
<td style="text-align: right;">21.369822</td>
<td style="text-align: right;">1.0000000</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

Because light was significantly, negatively correlated with litter and
large rocks.

    cor.test(env.dif[, "Big.rocks.."], env.dif[, "Litter.."])

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  env.dif[, "Big.rocks.."] and env.dif[, "Litter.."]
    ## t = -11.106, df = 28, p-value = 9.054e-12
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9530598 -0.8039735
    ## sample estimates:
    ##        cor 
    ## -0.9027609

    pdf("./results/scrl_litterVbigrocks.pdf", width = 5, height = 5)
    dev.off()

    ## png 
    ##   2

    pdf("./results/scrl_litter_effects.pdf", width = 15, height = 7)
    par(mfrow = c(1,3))
    plot(density(tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff)), 
        main = "", xlab = "Litter Difference (S - R)")
    abline(v = mean(tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff)),
        lwd = 0.5)
    plot(env.dif[, "Big.rocks.."] ~ env.dif[, "Litter.."], 
         xlab = "Litter Difference (S - R)", ylab = "Rock Cover (size >3 cm) Difference (S - R)",
         pch = 19, cex = 1.5)
    abline(lm(env.dif[, "Big.rocks.."] ~ env.dif[, "Litter.."]))
    plot(tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff), 
        tapply(l.dat[, "Light...average"], l.dat[, "Tree.pairs"], diff), 
        xlab = "Litter Difference (S - R)", ylab = "Light Difference (S - R)",
        pch = 19, cex = 1.5)
    dev.off()

    ## png 
    ##   2

    nmds.out <- nmds(vegdist(com.ds), 2, 2)
    ord <- nmds.min(nmds.out, dims = 2)

    ## Minimum stress for given dimensionality:  0.2169355 
    ## r^2 for minimum stress configuration:  0.6416469

    ord.pch <- c("R", "S")[(l.dat[, "Moth"] + 1)]
    plot(X2~ X1, data = ord, pch = ord.pch)

![](scrl_report_files/figure-markdown_strict/ord-com-plot-1.png)

Litter not light was correlated with large rocks (dist cor, in text).
Thus, higher amounts of litter under trees was not related to the
penetration of light under the tree canopy.

    cor.test(tapply(l.dat[, "Big.rocks.."], l.dat[, "Tree.pairs"], diff),
             tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff))

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tapply(l.dat[, "Big.rocks.."], l.dat[, "Tree.pairs"], diff) and tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff)
    ## t = -11.106, df = 28, p-value = 9.054e-12
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9530598 -0.8039735
    ## sample estimates:
    ##        cor 
    ## -0.9027609

    cor.test(tapply(l.dat[, "Big.rocks.."], l.dat[, "Tree.pairs"], diff),
             tapply(l.dat[, "Light...average"], l.dat[, "Tree.pairs"], diff))

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tapply(l.dat[, "Big.rocks.."], l.dat[, "Tree.pairs"], diff) and tapply(l.dat[, "Light...average"], l.dat[, "Tree.pairs"], diff)
    ## t = 0.71624, df = 28, p-value = 0.4798
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2376184  0.4716125
    ## sample estimates:
    ##       cor 
    ## 0.1341335

    cor.test(tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff),
             tapply(l.dat[, "Light...average"], l.dat[, "Tree.pairs"], diff))

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff) and tapply(l.dat[, "Light...average"], l.dat[, "Tree.pairs"], diff)
    ## t = -0.92053, df = 28, p-value = 0.3652
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.5007401  0.2013096
    ## sample estimates:
    ##        cor 
    ## -0.1713898

    cor.test(tapply(l.dat[, "Small.rocks.."], l.dat[, "Tree.pairs"], diff),
             tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff))

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tapply(l.dat[, "Small.rocks.."], l.dat[, "Tree.pairs"], diff) and tapply(l.dat[, "Litter.."], l.dat[, "Tree.pairs"], diff)
    ## t = -4.994, df = 28, p-value = 2.819e-05
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8391386 -0.4332285
    ## sample estimates:
    ##        cor 
    ## -0.6863699
