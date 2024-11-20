This shiny app allows you to visualize how group assignment will change as a function of a person's proportion of Group 1 ancestry. The probability of belonging to each group is given by a multinomial model governed by four parameters.

* Intercept for Group 1 assignment
* Slope for proportion of Group 1 ancestry for Group 1 assignment
* Intercept for Group 2 assignment
* Slope for proportion of Group 1 ancestry for Group 2 assignment

The mixed group is the reference for all assignment. These four parameters can be adjusted to create a large number of scenarios. The table below shows some examples that can be tried.

| Scenario                  | G1 Intercept | G1 Slope | G2 Intercept | G2 Slope |
|---------------------------|--------------|----------|--------------|----------|
| Strict Hypo/Hyperdescent  |  0 |  0 |  10 |   0 |
| Loose Hypo/Hyperdescent   |  3 |  3 |  10 |   3 |
| Low Multiracial ID        | -6 | 12 |   6 | -12 |
| High Multiracial ID       | -8 | 12 |   4 | -12 |
| Hypodescent to Multiracial| -8 |  6 |   4 | -12 |
| Symmetric Single Race ID  |  0 | 10 |  10 | -10 | 