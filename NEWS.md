## brio 0.3.8 (2019-09-11)

### Minor changes

- `export`: Improved default extension documentation and internal argument
  matching via `match.arg`. For `matrix` files exported simply with `dir`
  argument, this will default to CSV format. For sparse `Matrix` files exported
  simply with `dir` argument, this will default to MTX format.

## brio 0.3.7 (2019-09-03)

### Minor changes

- `import`: Improved internal arguments passed to `data.table::fread`.

## brio 0.3.6 (2019-08-27)

- Updated R dependency to 3.6.

### Major changes

- `export`: Reworked internal code to call `data.table::fwrite` directly, rather
  than having to pass to `brio::export`. Added support for `bz2` output for
  `matrix` and `sparseMatrix` classes.

## brio 0.3.5 (2019-08-22)

### Minor changes

- No longer warn on MTX import without sidecar rownames and colnames files.
- `import`: Don't attempt to slot attributes for atomic vector return. This
  applies to source code lines and helps avoid valid name issues when assigning
  these values to colnames or rownames. I came across this issue while updating
  the Chromium package to assign names from 10X Genomics sidecar files.

## brio 0.3.4 (2019-08-16)

### Minor changes

- Reworked organization and naming of internal importer functions. Now including
  these in the `import` documentation, for clarity.
- `import`: Added `format` and `setclass` arguments.
- Switched to `data.table::fread` for import of bcbio count matrix. Previously,
  was using `readr::read_tsv`.
- Import of lines no longer stores brio attributes.
- Reduced number of package dependencies, no longer requiring readr.

## brio 0.3.3 (2019-08-13)

### Minor changes

- Added support for `url` calls to `import`, `localOrRemoteFile`, and
  `loadRemoteData`.
- Improved message consistency.
- Updated basejump dependency versions.

## brio 0.3.2 (2019-08-06)

### Minor changes

- Now using acidroxygen package to manage shared roxygen documentation params.

## brio 0.3.1 (2019-07-30)

### Minor changes

- `import`: Bug fix for invalid objects (e.g. S4 objects that inherit
  `SummarizedExperiment`) not returning silently. Had to convert the `try`
  call to a `tryCatch` call to avoid errors popping up during name checks.
- `import`: Now suppressing `partial match of 'OS' to 'OS.type'` warning for
  import of XLS files, which is a bug in gdata package.

## brio 0.3.0 (2019-07-22)

Bumped version number to reflect changes in basejump dependencies.

### Minor changes

- Improved naming consistency of internal functions.
- Updated package dependency versions.

## brio 0.2.4 (2019-07-18)

### Minor changes

- Improvements to Travis Docker and AppVeyor CI checks.

## brio 0.2.3 (2019-07-16)

### Minor changes

- `factorize`: Tightened up method support. Now exporting `DataFrame` only.
- Bug fix for `acid.data.frame` global option support.
- `sanitizeNA`: Improved factor return.
- Improved code coverage.

## brio 0.2.2 (2019-06-08)

### Major changes

- `loadData`, `loadDataAsName`, and `loadRemoteData` now support overwrite
  argument. The default behavior of these functions has changed to allow
  overwriting into the environment by default, matching the base `load` function
  conventions. If this behavior is undesired, set
  `options(acid.overwrite = FALSE)` and this will be inherited in all calls.
- Renamed `acid.export.overwrite` and `acid.save.overwrite` to simply use
  `acid.overwrite` global for IO functions. This was modified now that
  `loadData` also supports the `overwrite` argument.

## brio 0.2.1 (2019-05-08)

### Major changes

- `import`: Removed Google Sheets support. The `googlesheets` R package is
  currently too buggy, and the replacement `googlesheets4` package isn't stable.
  This functionality may be added back in a future update.

## brio 0.2.0 (2019-05-06)

Introducing breaking changes to `export` method. Now using `object` instead of
`x` as the primary argument, and defaulting to the use of `ext` and `dir`
instead of recommending the use of `file`, as is the convention in the brio
package. This makes interactive file export quicker and more intuitive,
involving less repetitive variable declarations.

## brio 0.1.20 (2019-04-25)

### Minor changes

- S4 generic reexport documentation fixes.

## brio 0.1.19 (2019-04-22)

### Minor changes

- Backward compatibility fixes/updates to support R 3.4.

## brio 0.1.18 (2019-04-01)

### Minor changes

- Bug fix release. Re-importing rio package to ensure `export()` always works
  on `data.frame` method.

## brio 0.1.17 (2019-03-31)

### Minor changes

- `import`: Added GRP file support for GSEA.
- `import`: Improved Google Sheet import support, using new googlesheets4
  package from tidyverse.
- Improved code coverage, getting closer to 100%.

## brio 0.1.16 (2019-03-28)

### New functions

- Migrated `sanitizePercent` here from [basejump][] package.

### Major changes

- Switched to using "acid" prefix instead of "basejump" for global `options`.
  This applies in particular to the `loadData` and `saveData` functions, where
  the `dir` argument can be set globally for an interactive session using this
  parameter.

## brio 0.1.15 (2019-03-27)

### New functions

- Migrated `removeNA` and `sanitizeNA` from [basejump][] here, so these
  functions can be imported in [freerange][] package.

## brio 0.1.14 (2019-03-22)

### Minor changes

- Migrated code to [Acid Genomics][].

## brio 0.1.13 (2019-03-20)

### Minor changes

- `localOrRemoteFile`: Improved error message for Windows users when tempfile
  can't be removed successfully. This can happen on systems when the user is
  not running as Administrator, but doesn't happen on macOS or Linux.

## brio 0.1.12 (2019-03-17)

### Minor changes

- `import`: Added initial `rownames` and `colnames` parameter support for
  `data.frame` import. I strongly recommend leaving these enabled by default.
  However, these are useful in some edge cases when loading data from remote
  servers (e.g. WormBase).
- `export`: Removed `...` passthrough for `data.frame` method.

## brio 0.1.11 (2019-03-13)

### Minor changes

- `import`: Switched to using `gdata::read.xls` to import legacy XLS binary
  files. `readxl::read_excel` doesn't work consistently for some files and
  returns `libxls` error. In the meantime, use gdata package, which is slow
  but works.
- Removed `validObject` check in `import` call.

## brio 0.1.10 (2019-03-11)

### Minor changes

- `localOrRemoteFile`: Binary file extension pattern matching bug fix. Applies
  to files downloaded on Windows. If `download.file` is not called with mode
  `wb` (write binary) for files on Windows, decompression will fail.

## brio 0.1.9 (2019-03-08)

### Major changes

- `loadData` and `saveData` now support `list` argument, supporting a character
  vector of object names. This allows for programmatic use of the functions
  with standard evaluation. For reference, this approach is inspired by the
  method defined in `save`.

### Minor changes

- `import`: Data provenance metadata is now slotted into `attributes` for S3
  return (e.g. `data.frame`) and `metadata` for S4 return (e.g. `DataFrame`).

## brio 0.1.8 (2019-02-25)

### Minor changes

- `export`: Tightened up the method support, removing the `ANY` method. Now
  we're explicitly exporting `data.frame`, `DataFrame`, `matrix`, and `GRanges`
  methods. This also has the added benefit of making the documentation more
  readable.

## brio 0.1.7 (2019-02-15)

### New functions

- `fileExt`: An improved variation on `tools::file_ext`.

### Minor changes

- `basenameSansExt`: Tightened up this function to return `NA` on match failure.
  This behaves simiarly to the new `fileExt` function.
- Switching back from defunct to deprecated for `sanitizeColData`,
  `sanitizeRowData`, `sanitizeRowRanges`, since these functions are still in use
  by bcbioRNASeq v0.2.9.
- Improved NEWS file for previous releases.

## brio 0.1.6 (2019-02-12)

### Minor changes

- Documentation fixes and website improvements.

## brio 0.1.5 (2019-02-11)

This release helps ensure backward compatibility with R 3.4.

### Minor changes

- Bug fix for assert in `transmit`: Need to wrap `isMatchingRegex` in `all` for
  backward compatibility with R 3.4.
- Miscellaneous CI fixes to [Travis CI] and [AppVeyor CI].

## brio 0.1.4 (2019-01-21)

### New functions

These functions have been migrated from [basejump] here to brio, since they
deal specifically with file input/output:

- `atomize`.
- `decode`.
- `encode`.
- `factorize`.

### Removed functions

- Removed `sanitizeColData` and `sanitizeRowData`. May need to add back in a
  future release, but removed for time being.

### Minor changes

- `realpath`: Removed unnecessary assert check using `allHaveAccess`, since
  `normalizePath` already checks for this when `mustWork = TRUE`.
- Added initial code coverage support using [testthat][].

## brio 0.1.3 (2019-12-13)

### Minor changes

- Updated imports to reflect renaming of S4Transformer package to simply
  [transformer][].

## brio 0.1.2 (2019-12-12)

### Minor changes

- Improved `extPattern` to inherit `compressExtPattern`.
- Improved documentation in `import` regarding compressed file handling.
- Split out NAMESPACE imports into `imports.R` file.
- Updated `sanitizeColData` and `sanitizeRowData` to take advantage of exported
  `atomize` function.
- Improved [Travis CI] and [AppVeyor CI] configuration.

## brio 0.1.1 (2019-01-05)

### Minor changes

- Added [Travis CI][] and [AppVeyor CI][] support.
- Improved documention in `import` regarding supported file formats.
- Split out internal importers into separate R files.
- Fixed return value for `pasteURL`.
- Disabled working examples for transmit, since they are failing on [Travis CI].
- Documentation fixes and miscellaneous tweaks to pass build checks.

## brio 0.1.0 (2018-12-24)

Initial release. Migrated input-output (IO) functions from [basejump][].

[Acid Genomics]: https://acidgenomics.com/
[AppVeyor CI]: https://www.appveyor.com/
[basejump]: https://basejump.acidgenomics.com/
[freerange]: https://freerange.acidgenomics.com/
[testthat]: http://testthat.r-lib.org/
[transformer]: https://transformer.acidgenomics.com/
[Travis CI]: https://travis-ci.com/
