## pipette 0.6.2 (2021-06-10)

### Minor changes

- `localOrRemoteFile`: Improved handling of URLs without a file extension.
- `import`/`export`: Simplified the appearance of file variables, making them
  easier to copy directly from the console for debugging.

## pipette 0.6.1 (2021-06-09)

### Minor changes

- `import`: Ensure that `data.table` engine always interprets empty strings
  (`""`) as `NA`. The `fread` function is opinionated about this and doesn't
  currently respect `""` input as an `NA` string.
- `decode`/`encode`: Ensure we're not dropping metadata here.

## pipette 0.6.0 (2021-06-04)

### Major changes

- `import` now uses S4 methods based on the `file` argument, which can be
  manually overridden with the `format` argument (e.g. "csv" for a CSV file).
- `import` and `export` now support `engine` argument for methods dispatching
  on `character` and `data.frame`. Currently "base" (base R), "data.table",
  "readr", and "vroom" are supported.
- Source code lines now default to using base R for import/export by default.
  Previously, this defaulted to data.table, which is now only used as the
  default for import of delimited files.

### Minor changes

- `import`: Added support for `removeBlank` and `stripWhitespace` for import
  of source code lines (`LinesFile`).
- `import`: Improved import handling of source code lines using data.table.

## pipette 0.5.18 (2021-05-18)

### Minor changes

- `decode` / `encode`: Reworked internal code slightly to provide compatibility
  with S4Vectors update in Bioconductor 3.13.

## pipette 0.5.17 (2021-05-18)

### Major changes

- `as.DataFrame`: Updated `list` to `DataFrame` coercion support that is
  compatible with Bioconductor 3.13 release update.
- `cacheURL`: Now using `tools::R_user_dir` instead of
  `rappdirs::user_cache_dir` internally. This matches the conventions used in
  Bioconductor 3.13 (e.g. AnnotationHub and BiocFileCache).

## pipette 0.5.16 (2021-05-18)

### Minor changes

- Documentation updates, to pass build checks without warnings on R 4.1
  and Bioconductor 3.13.

## pipette 0.5.15 (2021-04-27)

### Minor changes

- `cacheURL` now internally calls BiocFileCache and rappdirs as suggested
  packages, rather than direct imports. This helps keep the package a bit
  lighter and improve loading times, as BiocFileCache currently calls a number
  of heavy dependencies, including dplyr.
- The checksum functions `md5` and `sha256` now call `digest` internally as
  a suggested package, rather than a direct import.

## pipette 0.5.14 (2021-03-17)

### Major changes

- Switched the default engine back to data.table package for `import` and
  `export` functions.

### Minor changes

- Improved consistency of arguments for internal engines (base, data.table,
  readr, and vroom) for `import` and `export` of delimited files.
- Base engine now uses `read.table` and `write.table` instead of `read.csv`
  and `write.csv` for CSV files.
- The optional readr engine now uses `read_delim` and `write_delim`, similar
  to the update for base engine.
- Improved code coverage of base engine for import/export.

## pipette 0.5.13 (2021-03-04)

### Minor changes

- `import`: Hardened importer against unexpected mismatch when user attempts
  to manually define column names via `colnames` argument. Some importers
  such as `vroom` are currently too liberal about mismatches.

## pipette 0.5.12 (2021-02-22)

### Minor changes

- Including `seqnames` as a reexport, which is defined in GenomeInfoDb via
  GenomicRanges.

## pipette 0.5.11 (2021-02-19)

### Minor changes

- `as.DataFrame`: Restrict list-based coercion to `list` and `SimpleList`
  (instead of `List` virtual class).

## pipette 0.5.10 (2021-02-17)

### Minor changes

- Include `CompressedGRangesList` from GenomicRanges as a reexport.

## pipette 0.5.9 (2021-02-12)

### Minor changes

- Migrated IRanges reexports to AcidGenerics.

## pipete 0.5.8 (2021-02-12)

### Minor changes

- Updated `naStrings` to include `"-"`, `"_"`, and `" "`.

## pipette 0.5.7 (2021-02-12)

### Minor changes

- Minor rework and simplification of NAMESPACE, inheriting from AcidGenerics
  and AcidBase when possible.

## pipette 0.5.6 (2021-02-11)

### Minor changes

- Reexporting additional useful functions and classes from IRanges, including
  `AtomicList` virtual class.
- Migrated S4Vectors reexports to AcidGenerics, including `Annotated`, `Factor`,
  `Factor`, `LLint`, and `RectangularData` classes.

## pipette 0.5.5 (2021-02-11)

### Minor changes

- Including more reexports of useful S4 classes and functions defined in IRanges
  that we will reexport in basejump: `CharacterList`, `FactorList`,
  `IntegerList`, `LogicalList`, `NumericList`, `RleList`.

## pipette 0.5.4 (2021-02-09)

### Minor changes

- Reexporting some additional functions and classes from GenomicRanges, IRanges,
  Matrix, and S4Vectors that we can inherit in basejump.

## pipette 0.5.3 (2021-02-03)

### New functions

- Added `md5` and `sha256` functions, that use the digest package internally.
  Previously these were defined in the AcidGenomes package, but have migrated
  here for file management consistency.

### Major changes

- `cacheURL`: Fix bug that resulted in different remote URLs with the same
  base name being cached as the same object internally by BiocFileCache. Now
  URLs should always be cached uniquely.

### Minor changes

- Migrated test data from "tests.acidgenomics.com" to
  "r.acidgenomics.com/testdata".
- Added `droplevels` method for `DataFrame`.

## pipette 0.5.2 (2021-01-15)

### Minor changes

- `cacheURL`: Improved message. Now only showing when file gets cached into
  package cache via BiocFileCache. Now defaults to caching into BiocFileCache
  directory instead of pipette.
- Now exporting useful `rbindlist` function from data.table.
- Renamed `matchRowNameColumn` to `matchRownameColumn` (note case).

## pipette 0.5.1 (2021-01-13)

### Minor changes

- `naStrings`: Now including lowercase `NA` variants, which are seen in some
  files on RefSeq FTP server.
- `sanitizeNA`: Updated to also match lowercase `NA` patterns.

## pipette 0.5.0 (2021-01-13)

### Major changes

- `import`: Improved internal engine support for plain text delimited (e.g.
  CSV, TSV) files and source code lines. The vroom engine remains enabled by
  default but data.table, readr, and base R are consistently supported for
  import of either delimited files or source code lines.
- `export`: Added supported for base R export of CSV and TSV files. Internal
  engine consistency has been improved for `character` and `matrix`/`data.frame`
  methods. Note that `character` method currently falls back to using
  readr's `write_lines` function instead of attempting to use the vroom
  package by default.

## pipette 0.4.25 (2021-01-08)

### Minor changes

- `getURLDirList` now returns sorted.
- Reexporting `url.exists` function from RCurl, for convenience.

## pipette 0.4.24 (2021-01-06)

### Minor changes

- Switched from using cli package internally to AcidCLI.

## pipette 0.4.23 (2020-12-24)

### Minor changes

- Made some previous imports conditional suggested packages:
  jsonlite, readr, rtracklayer, yaml.
- `export`: Improved internal file name handling for CLI messages.
- Reworked rtracklayer as a suggested package instead of an explicit import.
- `import`: Improved internal bcbio counts importer code to use default TSV
  method, rather than relying on data.table `fread` function manually.
- Removed dependency on readr. Import of lines now uses `vroom::vroom_lines`
  internally, and `export` character method will conditionally switch to using
  base `writeLines` if the readr package is not installed.
- Removed Matrix `readMM` and `writeMM` as imports.
- Removed data.table `fread` and `fwrite` as imports.
- Removed BiocGenerics dependency, in favor of rexports defined in AcidGenerics.
  This helps keep the number of dependencies declared in the package more
  compact and manageable.
- `export`: Bug fix for handling of GZ file name extension for `character`
  method.
- Bug fix for `vroom_lines` error import error of bcbio log:
  `Unnamed col_types must have the same length as col_names`.
- Now including additional reexports from data.table and tibble packages.
- Bug fixes for `import`/`export` of tx2gene file handling for pending
  AcidGenomes package update.
- `import`: Hardened against `makeNames` usage on objects that don't support
  names assignment.

## pipette 0.4.22 (2020-12-11)

### Minor changes

- `export`: Added option to intentionally not export column and/or rownames
  for `matrix`, `data.frame`, and `DataFrame` classes.

## pipette 0.4.21 (2020-12-10)

### New functions

- Added `download`, which acts as a hardened wrapper for `utils::download.file`.
  Annoying, `download.file` returns status codes but does not intentionally
  error on any unsuccessful downloads. Our wrapper ensures that R always errors
  on any file download issue. It also sets a longer timeout internally, to
  avoid any potential issues with the `timeout` option being defined in
  `Rprofile`.

## pipette 0.4.20 (2020-12-09)

### Minor changes

- Updated dependency versions.
- `export`: Added `append` option for `character` method. Also relaxed checks
  on `character` method, allowing for exporting of empty vectors.

## pipette 0.4.19 (2020-10-12)

### Minor changes

- `transmit`: Added `download` argument support, to optionally return matching
  URLs without downloading. This is useful for handing off to `cacheURL`
  function for caching files inside of packages with BiocFileCache.

## pipette 0.4.18 (2020-10-09)

### Minor changes

- Bug fix for breaking change in readr v1.4 release. In the `write_*` functions,
  including `write_lines` and `write_csv`, the `path` argument has been renamed
  to `file`. Now requiring readr v1.4+ in pipette.

## pipette 0.4.17 (2020-10-09)

### New functions

- Migrated `transmit` here from basejump.

## pipette 0.4.16 (2020-10-06)

### New functions

- `getURLDirList`: Migrated this function from previous definition in basejump,
  so we can inherit inside new AcidGenomes package.

### Minor changes

- `cacheURL`: Added `package` argument, so other packages that use this
  function will automatically inherit the current package, as expected.

## pipette 0.4.15 (2020-10-06)

### New functions

- `cacheURL`: Utility function for easy package file caching using BiocFileCache
  package internally.

### Minor changes

- Updated Acid Genomics package dependencies.

## pipette 0.4.14 (2020-09-14)

### Minor changes

- `sanitizeNA`: Added support for "N/A" string, which is present in some Excel
  spreadsheets.

## pipette 0.4.13 (2020-08-18)

### Minor changes

- `export`: Now dropping non-atomic columns (e.g. Entrez ID list column) from
  data frames automatically prior to export. Previously, the `allAreAtomic`
  assert check was called automatically and would error on non-atomics.

## pipette 0.4.12 (2020-08-13)

### Minor changes

- `import`: Improved messages to always resolve full path to import directory.
- Bug fix for AppVeyor CI config.

## pipette 0.4.11 (2020-08-11)

### Minor changes

- `export`: Ensuring that full directory path is always resolved in message.
- Miscellaneous message improvements, related to internal `toString` handling.

## pipette 0.4.10 (2020-08-03)

### Minor changes

- Relax name checks for `import`.

## pipette 0.4.9 (2020-07-27)

### Minor changes

- Decreased data.table dependency from 1.13.0 back to 1.12.8, so we can build
  successfully on bioconda.

## pipette 0.4.8 (2020-07-24)

### Minor changes

- `export`: Improved messaages to include full output path.
- Increased minimum R dependency to 4.0.

## pipette 0.4.7 (2020-07-07)

### Minor changes

- `import` Hardened Excel input to intentionally error on any warnings returned
  by internal `read_excel` call, which is too liberal in coercing data types,
  in my opinion.
- `import`: Switched XLS parser from gdata (no longer updated) back to readxl,
  which is more actively developed.
- `import`: Added `makeNames` argument, to override default internal handling.
  This allows the user to apply snake case and/or camel case formatting
  automatically with this argument.

## pipette 0.4.6 (2020-06-11)

### Minor changes

- `import`: Added support for `skip` argument, which allows the user to skip
  a certain number of lines in the input.
- `import` and `export` of source code lines now uses readr package internally
  (`read_lines` and `write_lines`) instead of base `readLines` and `writeLines`.

## pipette 0.4.5 (2020-05-18)

### Minor changes

- `import`: Now setting delim internally for `vroom` import call, to handle
  single column data frame import. Otherwise vroom will warn about failing
  to detect expected delimiter.

## pipette 0.4.4 (2020-05-12)

### Major changes

- `import` and `export` functions now default to using vroom engine instead
  of data.table. Internally, these now call `vroom` and `vroom_write`.
  We noticed that the data.table `fwrite` function in particular can have issues
  writing many files on AWS EC2 instances, resulting in a stack imbalance.
  The vroom package seems to be more stable currently.

### Minor changes

- `loadData`, `saveData`: Switched to using `cli_alert` instead of `cli_text`
  internally for status messages.

## pipette 0.4.3 (2020-04-15)

### Minor changes

- `import`: Bug fix for `format` argument erroring on some supported file types.

## pipette 0.4.2 (2020-04-10)

### Minor changes

- `droplevels`: Ensuring S4 generic variant defined in S4Vectors package gets
  reexported and masks base S3 generic. This helps avoid a `C stack usage`
  issue that has popped up in the latest version of R.
- `import`: Fix for importing JSON files without extension. Can now declare
  using the `format` argument. This fix was needed to import GitHub JSON URLs
  inside new `installGitHub` function defined in [bb8][] package.

## pipette 0.4.1 (2020-01-28)

### Minor changes

- Switched license from MIT to GPL-3.
- Ensure that `coerce` method is reexported -- thanks @dpryan79 for catching
  this issue in basejump.

## pipette 0.4.0 (2020-01-19)

### Major changes

- Renamed package from brio to pipette, in preparation of CRAN submission.
- `export`: Reworked internal methods to use new `compress` and `decompress`
  functions defined in acidbase package.
- `localOrRemoteFile`: Reworked to use new `decompress` defined in acidbase
  internally, with improved `tempfile` handling.
- Migrated coercion methods and other utitilies from the now archived
  transformer package: `atomize`, `coerceToList`, `droplevels`, `decode`,
  `encode`, `factorize`, `matchRowNameColumn`, and `metadata2`.

### Minor changes

- Switched to using cli package for improved messages.

## brio 0.3.17 (2020-01-10)

### Minor changes

- `import`: Added `metadata` parameter option and improved internal `tryCatch`
  handling if call capture fails. This can be the case when nesting the function
  inside another function, which can cause `standardizeCall` to fail. Note that
  `match.call` doesn't have this problem but doesn't consistently expand the
  call with default formals as well.

## brio 0.3.16 (2019-12-09)

### Minor changes

- `export`: Removed internal dependencies on `as_tibble` and `as.data.table`
  calls. The tibble package recently changed the default row name handling
  behavior in `as_tibble`, which broke the code here. I reworked the internal
  code to only use base R approaches, so changes in the tidyverse no longer
  affect the package.

## brio 0.3.15 (2019-11-07)

### Minor changes

- Updated package dependencies to require Bioconductor 3.10 release.

## brio 0.3.14 (2019-10-24)

### Minor changes

- Improved internal metadata handling using new `metadata2` function.

## brio 0.3.13 (2019-10-22)

### Minor changes

- NAMESPACE updates to support migration of some low-level functions to the new
  [acidbase][] package.

## brio 0.3.12 (2019-10-18)

### New functions

- `getURLDirList`: Return a simple character vector of files and subdirectories
  in a remote directory. Intended for use with FTP servers.
- Also now reexporting the `getURL` function from RCurl.

### Minor changes

- `import`: Default `format` argument has been renamed from "none" to "auto".
- `localOrRemoteFile`: Improved handling for remote URLs without a file
  extension.
- `pasteURL`: Now smartly strips trailing slashes prior to internal paste call.

## brio 0.3.11 (2019-10-13)

- `naStrings`: Reverted back to including only "NA" and "NULL", instead of
  including empty space strings. This results in unwanted messages regarding
  strip whitespace from data.table `fread` function.

## brio 0.3.10 (2019-10-12)

### Major changes

- Added back internal support for readr package instead of data.table for
  `import` and `export` functions. We have observed stack imbalance and segfault
  memory dump issues with the latest data.table release (v1.12.4) on multi-core
  Azure VMs. The engine can now be changed using global options:
  
  - import: `acid.import.engine` ("data.table" or "readr").
  - export: `acid.export.engine` ("data.table" or "readr").
  
  This new addition is experimental and may be dropped in a future release. We
  find that readr currently works more reliably for export in some cases for
  large CSV files, but data.table is generally faster and more robust for data
  import of CSV and TSV files. We're intentionally keeping this functions simple
  and not providing a user-facing argument for selecting the internal engine.

### Minor changes

- Converted unnecessary exported global variables into internal globals:
  `compressExtPattern`, `extPattern`, `rdataExtPattern`, `rdataLoadError`.
- Updated `naStrings` to include empty whitespace.

## brio 0.3.9 (2019-10-04)

### Minor changes

- Updated data.table and rio dependencies, based on recent data.table 1.12.4
  update, which has a lot of changes.
- Fixed internal code to no longer show rownames message, even when
  `rownames = FALSE`.

## brio 0.3.8 (2019-09-11)

### Major changes

- `import`: Improved metadata stash approach inside S4 (`metadata`) and S3
  (`attributes`) return objects. Previously, import metadata was stashed inside
  "brio", but this has been renamed to "import". Metadata is no longer stashed
  inside R data objects loaded via `import`. Simplified the internal code inside
  `import` to only stash the `call`, whereas importer metadata is now handled
  by an internal `.defineImportMetadata` function.

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

[acidbase]: https://acidbase.acidgenomics.com/
[acid genomics]: https://acidgenomics.com/
[appveyor ci]: https://www.appveyor.com/
[basejump]: https://basejump.acidgenomics.com/
[bb8]: https://bb8.acidgenomics.com/
[freerange]: https://freerange.acidgenomics.com/
[testthat]: http://testthat.r-lib.org/
[transformer]: https://transformer.acidgenomics.com/
[travis ci]: https://travis-ci.com/
