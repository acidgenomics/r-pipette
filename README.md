# brio

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/brio.svg?branch=master)](https://travis-ci.com/acidgenomics/brio)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/cjde5mhr8226ctl8/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/brio/branch/master)
[![Anaconda version](https://anaconda.org/bioconda/r-brio/badges/version.svg) ![Anaconda latest release date](https://anaconda.org/bioconda/r-brio/badges/latest_release_date.svg) ![Anaconda downloads](https://anaconda.org/bioconda/r-brio/badges/downloads.svg)](https://anaconda.org/bioconda/r-brio)

Biological R input/output.

This package is part of the [basejump][] toolkit.

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
# Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/brio")
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```bash
conda install -c bioconda r-brio
```

[R]: https://www.r-project.org/
[basejump]: https://basejump.acidgenomics.com/
[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
