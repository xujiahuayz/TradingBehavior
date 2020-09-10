# Trading Behavior

This study series involve heavy computing for simulations and big data regressions that require High Performance Computing (HPC).


## Complete the code locally and push commits

```sh
git add .
git commit -m "commit message"
git push
```


## Use [EPFL fidis](https://www.epfl.ch/research/facilities/scitas/hardware/fidis/) for HPC

Locate the current repository and pull commits

```sh
cd /home/jxu/TradingBehavior
git pull
```

Run jobs following the [instructions](https://scitas-data.epfl.ch/confluence/display/DOC/Using+the+clusters)

```sh
sbatch RegressionsR/HPCjobs/MinimumExample.run
```

Push results (specified to be in the output folder)

```sh
git add .
git commit -m "commit message"
git push
```
