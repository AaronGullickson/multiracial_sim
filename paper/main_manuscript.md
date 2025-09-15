# Abstract

# Introduction

 <*Something catchy perhaps contrasting popular understandings of U.S. population projections: the optimistic "browning of America" version popular in the 1990s and the more pessimistic "coming majority-minority society" version more common today...both of which hinge on assumptions about future growth of the multiracial population*.>
 


The nature of the population dynamics involved in this process are complex and depend on several inputs, such as interracial marriage rates and identification choices, not easily incorporated into standard demographic projection techniques. In this article, we use a microsimulation approach to compare a series of ideal type projection scenarios that develop over the course of XXX years. The microsimulation approach tracks the family tree of every single member of the population allowing us to separate mixed ancestry and multiracial identification. 

By running these simulations for a long period of time, we are able to reveal population dynamics that are likely a better reflection of long term trends than current understandings. Our results show that even under high rates of interracial marriage, continued growth of the multiracial population is highly dependent not only on the relative group size of the starting populations but also the prevailing "regime" of self-identification. This suggests <*something important about how the census does projections? or how people understand the evolution of multiraciality?*>
 

# Background

The enumeration and growth of the multiracial population represents one of the greatest potential catalysts for change in US racial dynamics over the last half century. Although multiraciality is not a new phenomenon, a "biracial baby boom" of individuals whose parents cross racial lines from the late 20th century and early 21st century has led to changes in official race reporting in the US and greater questioning of racial categories at large <!-- CITE -->.

However, this biracial baby boom may mislead regarding the long term impact of multiraciality on the US's racial future. Like the original baby boom, the biracial baby boom is driven by rapid historic change that generates cohort-specific distortions of stable population dynamics. Specifically, the biracial baby boom moved the "generational loci" of multiraciality to be dominated by individuals who were "first generation" offspring of parents that identified with different racial groups. The experience of multiraciality will be most salient for this generation. Much of the academic literature on multiraciality has focused on the experiences of these individuals.

However, this biracial baby boom does not adequately reflect the long term processes that will determine the future of multiraciality in the US. As the children of that boom grow into adulthood, they have begun to form their own families, most commonly with individuals of a single racial identification shared with the biracial person. These families will produce a new generation of children for home the generational loci of multiraciality will be more distant and thus likely less salient. Over time, this multigenerational process will likely result in a more fractured and diverse mosaic for our understanding of contemporary multiraciality than "absorbing state" of biracial identification often conjured up in the discourse on the biracial baby boom and the increasing racial diversity of the US.

# Data and Methods

To answer our research questions, we turn to a microsimulation approach. Specifically, we use the SOCSIM program to simulate individual life course events over a 500 year simulation period from an initial population of 50,000 individuals. SOCSIM was developed in the 1970s at the University of California, Berkeley to allow for the microsimulation of populations based on a set of demographic rates. Importantly, microsimulation preserves the kinship relationship between every individual in the microsimulation, which is critical for our approach. We use SOCSIM to build a simulation using two primary groups, A and B, and a third "mixed" group, AB,  that allows for intermarriage between the groups and with different scenarios for how the group membership of mixed-group births is determined. In all simulations, the first 300 years of the simulation are a "run in" to establish stable population characteristics, in which we do not allow intergroup partnership between group A and B. In the final 200 years of the simulation, we allow for intergroup partnering using several different scenarios. 

SOCSIM includes the ability to simulate multigroup populations. However, the existing algorithms to determine both intermarriage rates and the group membership of mixed-group births are both too limited for our current purposes. To overcome this challenge, we combined SOCSIM with our own algorithms (written in R) for determining marriages and group membership. We allow SOCSIM to run in single year increments, following its usual procedures to determine deaths and births within the population. Throughout the simulation, we use mortality rates based on US life tables from the year 2000. We similarly use baseline age specific fertility rates from the US in 2000, but apply a simple multiplier to produce a roughly stationary population throughout the simulation. 

At the end of each year, we hold a "marriage jubilee" in which all currently single individuals are potentially partnered and married. For every single woman, we choose up to fifty potential single male partners who do not share any of the same grandparents. We then calculate the log-odds, $O_{hw}$, of a marriage between potential husband $h$ and potential wife $w$ according to the following formula:

$$log(O_{hw}) = 0.072 (\textit{age}_h-\textit{age}_w) - 0.014 (\textit{age}_h-\textit{age}_w)^2 + \alpha_{hw}$$
 The age difference parameters are derived from @gullickson_counterfactual_2021 and produce a parabolic function of spousal age difference that is maximized when the husband is 2.76 years older than the wife. The $\alpha$ parameter is determined by group exogamy. When both potential partners are from the same group, this parameter is zero. For all other cases, $\alpha$ will be negative to indicate a lower likelihood of group exogamy than endogamy. We have three cases of intermarriage, an A-B intermarriage between the two primary groups, and two cases of intermarriage between either primary group and the mixed group, A-AB and B-AB. We develop different values for $\alpha$ across all of these cases in the scenarios discussed below. 

To determine the actual marriage chosen, we sample from the available possibilities with weight equal to $O_{hw}$. This approach is equivalent to a conditional logit model approach in which the probability for a specific match $i$ would be given by:

$$\frac{e^{O_i}}{\sum_{k=1}^{J} e^{O_j}}$$
Using this routine, it is possible for the same husband to be chosen by two different women. In such cases, we remove duplicate selections randomly (better luck next time, ladies).  We also found in practice that it was necessary to set a benchmark for situations in which none of the possible husbands crossed a reasonable threshold for acceptability. In cases where women were left with four or fewer options or the highest odds ratio was below 0.12, we terminated the search for a husband and these women remain single for the next year. <!-- TODO: discuss how this algorithm produces reasonable marriage statistics across all types of marriages -->

Because we want to track mixed ancestry, we technically only allow fertility within marriage, although SOCSIM does allow for fertility outside of marriage as well. For our purposes, these "marriages" are in practice just an instance of coupling to allow for easier accounting of parentage within the simulation framework.  <!- TODO: some discussion of the lack of divorce within our approach, and consequences of this -->

Between each year of the simulation, we also use our own algorithm to determine the group membership of any children born in the previous year. When these children are born to parents of the same group, the child is assigned to that group (e.g. the child of two A parents is assigned to A). In cases where the parents are assigned to different groups, the child could plausibly be assigned to any of the three groups, A, B, or AB. In such cases, we use a multinomial model to assign group membership based on the proportion of the child's ancestors that are members of group A. The model is defined as:

$$ \log(O_{ij}) = \beta_{0j} + \beta_{1j}x_i + \beta_{2j}x_i^2 + \beta_{3j}x_i^3$$
where $O_{ij}$ is the odds that individual $i$ is assigned to group $j$ and $x_i$ is the proportion of ancestors of individual $i$ that belong to group A (e.g. a "first generation" child of A and B parents would have 0.5 group A ancestors). We allow for a third degree polynomial function to allow for considerably non-linearity in this function. To determine the ultimate probability of each group assignment, we use:

$$P_{ij} = \frac{e^{O_{ij}}}{\sum_{k=1}^{J} e^{O_{ij}}}$$
where $P_{ij}$ is the probability of group assignment to group $j$. We then sample an assignment for each individual based on these probabilities. This model requires the estimation of nine distinct $\beta$ parameters (three for each group $j$). We discuss below how we develop these $\beta$ parameters for several different scenarios. 

This approach is intended to allow for a great deal of flexibility, but it does have some limitations. First, probabilistic group assignment is only triggered when a person's parents are self-identified as belonging to different groups. This eliminates the possibility of later generation switches in response a more generationally distant intergroup partnering. We do have examples of some such cases historically. The "ethnic renewal" of American Indian populations in the late 20th century involved ethnic switching of individuals from one racial identity to that of an American Indian identity based on some knowledge of often distant family history <!-- TODO: citations -->. In the opposite direction, the "passing" of light skinned black individuals into a whiteness was most often done by individuals whose multiracial ancestry was distant.

We also ignore the potential for *intragenerational* group switching. Research on contemporary multiracial identity has emphasized that switching racial self-identification over time is common <!-- TODO: citations -->. DISCUSS WHY NOT A BIG PROBLEM

## Scenarios

We develop a total of twenty simulations along three different dimensions: the initial relative size of groups, the degree of intergroup marriage over time, and the pattern of group assignment.

### Initial group size

Since most empirical cases of multiethnic populations will involve majority-minority populations, our primary simulations begin with a population that is 85% members of Group A and 15% members of group B. These numbers may shift slightly as a result of random chance during the "run in" period of the simulation but should remain relatively stable until we introduce intergroup dynamics. 

To understand better how relative group size might affect the intergroup dynamics we are studying, we also run a second set of scenarios in which Groups A and B are of equal size.

### Intermarriage rates

We create two different scenarios for intermarriage rates. In both cases, the initial period is loosely modeled on intermarriage patterns in the US from the 1960s, when intermarriage rates began to increase exponentially from an extremely rare baseline. We also found that the $\alpha$ parameters below are equal to half the cross-product odds ratios that are often used to measure intermarriage. We set the initial baseline for A-B intermarriage at $\alpha = -5$. We then allow for a "progress" trend in which this the $\alpha$ parameter is reduced linearly to zero over the next 100 years at which point it remains constant. Because this measure is on the log-scale, this approach indicates exponential growth in intermarriage until exogamy is as likely as endogamy (accounting for group size).

The second scenario is a "plateau" trend in which $\alpha$ is cut in half after 25 years and then remains constant. In this scenario, the barriers to intermarriage are substantially reduced but exogamy remains rarer than endogamy. 

In both scenarios, we set the A-AB and B-AB $\alpha$ values to be equal to half the A-B $\alpha$ values, with the assumption that these types of intergroup unions will be easier to cross, since they involve both some degree of endogamy and exogamy. <!-- We can cite Gullickson and Bratter soon hopefully on this one -->

### Group assignment

We then consider five different scenarios for group assignment. The first scenario which we refer to as the "absorbing state" is the one most often envisioned in popular discourse on multiraciality. In this scenario, the probability of AB identification of a mixed group child is 100%. Once a person becomes mixed, all of their descendants will remain mixed. In such a scenario, the mixed group becomes an absorbing state and the entire population must at some point become mixed. We do not believe that this is a realistic scenario but it does give us an important baseline from which to compare more conservative scenarios.

The remaining scenarios are divided along two dimensions. We assume that the likelihood of AB identification will be maximized when an individual shares ancestry equally from both groups A and B, and that as the proportion of ancestry moves in either direction from this point, the likelihood of AB identification will decrease and the likelihood will increase of identifying as a single race member of the group with which the person shares more ancestry.

Our scenarios are thus defined by the probability of AB identification at the peak and the degree to which this kind of identification declines as ancestry moves away from the peak. 

---

By design, the simulations we use here are simplified representations that allow us to understanding population dynamics and may not reflect real populations. However, the simulation approach we use here is developed to be intentionally extensible to more complex scenarios.

<*something else about how the ideal types are designed around the U.S. experience but the approach can be applied more broadly to other countries interested in incorporating self-identification dynamics into their projections (maybe include cites about existing work on Australian, New Zealand and UK projections here)*>

# Results

# Conclusions
