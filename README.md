# Trading Behavior

This study series involves heavy computing for simulations and big data regressions that require High Performance Computing (HPC).


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

Move results from `scratch` to `output` folder and push

```sh
mv /scratch/jxu/*.Rout output/
git add .
git commit -m "commit message"
git push
```
