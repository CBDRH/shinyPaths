### Confounding

*** 

In this summery example, we are asking the question _does eating ice-cream causes sunburn_? This potential relationship is represented on the DAG by the directed arrow from X to Y (X&rarr;Y).

Sunshine is a classic confounder here as it has an effect on both the exposure and outcome: ice-cream sales go up on warm days (Z&rarr;X) as does incidence of sunburn (Z&rarr;Y).

The presence of this confounder results in an open backdoor path (X&larr;Z&rarr;Y). 
