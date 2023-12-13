#' File extension classes
#'
#' Currently intended for use with `import` function.
#'
#' @export
#' @note Updated 2023-12-13.
#'
#' @section Primary classes:
#'
#' - `PipetteBamFile`: Binary sequence alignment map (BAM).
#' - `PipetteBcfFile`: Binary variant call format (BCF).
#' - `PipetteBcbioCountsFile`: bcbio-nextgen counts file.
#' - `PipetteCramFile`: Compressed reference-oriented alignment map (CRAM).
#' - `PipetteCsvFile`: Comma-separated values file (CSV).
#' - `PipetteFastaFile`: FASTA file.
#' - `PipetteFastqFile`: FASTQ file.
#' - `PipetteGafFile`: Gene Ontology (GO) annotation file (GAF).
#' - `PipetteGctFile`: Gene Cluster Text file (GCT).
#' - `PipetteGmtFile`: Gene matrix transposed file (GMT).
#' - `PipetteGmxFile`: Gene matrix file (GMX).
#' - `PipetteGrpFile`: Gene set file (GRP).
#' - `PipetteJsonFile`: JSON file.
#' - `PipetteMafFile`: Mutation annotation format file (MAF).
#' - `PipetteMtxFile`: MatrixMarket exchange file (MTX).
#' - `PipetteOboFile`: Open Biomedical Ontologies file (OBO).
#' - `PipettePzfxFile`: GraphPad Prism file (PZFX).
#' - `PipetteRdsFile`: R data file containing a single, serialized object (RDS).
#' - `PipetteRDataFile`: R Data file containing multiple objects (RData/RDA).
#' - `PipetteSamFile`: Sequence alignment map (SAM).
#' - `PipetteTsvFile`: Tab-separated values file (TSV).
#' - `PipetteTableFile`: Base R table file (TXT).
#' - `PipetteVcfFile`: Variant call format (VCF).
#' - `PipetteYamlFile`: YAML file.
#'
#' @section `PipetteDelimFile-class`:
#'
#' Delimited file.
#'
#' File extension group supporting:
#'
#' - CSV: Comma-separated values
#' - TSV: Tab-separated values
#' - Base R table (TXT)
#'
#' @section `PipetteExcelFile-class`:
#'
#' Microsoft Excel file.
#'
#' File extension group supporting:
#'
#' - XLS
#' - XLSB
#' - XLSX
#'
#' @section `PipetteLinesFile-class`:
#'
#' Source code lines file.
#'
#' File extension group supporting:
#'
#' - BASH: Bash shell script
#' - LOG: Log file
#' - MD: Markdown file
#' - PY: Python script
#' - R: R script
#' - RMD: R Markdown file
#' - SH: Shell script (e.g. Bash, POSIX)
#' - ZSH: Zsh shell script
#'
#' @section `PipetteRioFile-class`:
#'
#' File extension supported by `rio::import`.
#'
#' File extension group supporting:
#'
#' - ARFF: Weka Attribute-Relation File Format
#' - DBF: dBase Database File
#' - DIF: Data Interchange Format
#' - DTA: Stata
#' - FWF: Fixed-width formatting
#' - MAT: Matlab
#' - MTP: Minitab
#' - ODS: OpenDocument (LibreOffice)
#' - POR: SPSS
#' - PSV: Pipe-separated values
#' - SAS7BDAT: SASS
#' - SAV: SPSS
#' - SYD: Systat
#' - REC: Epi Info
#' - XPT: SASS
#'
#' @section `PipetteRtracklayerFile-class`:
#'
#' File extension supported by `rtracklayer::import`.
#'
#' File extension group supporting:
#'
#' - BED, BED15, BEDGRAPH, BEDPE
#' - BIGWIG, BW, WIG
#' - GFF, GFF1, GFF2, GFF3, GTF
#' - BROADPEAK, NARROWPEAK
setClass(
    Class = "PipetteFile",
    slots = c("resource", "origResource")
)
setValidity(
    Class = "PipetteFile",
    method = function(object) {
        validate(
            isString(slot(object, "resource")),
            isString(slot(object, "origResource"), nullOk = TRUE)
        )
    }
)



## File extension groups =======================================================

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteDelimFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteExcelFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteLinesFile",
    contains = "PipetteFile"
)



## Handoff classes =============================================================

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteRioFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteRtracklayerFile",
    contains = "PipetteFile"
)



## File extensions =============================================================

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteBamFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteBcfFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteBcbioCountsFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteCramFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteCsvFile",
    contains = "PipetteDelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteFastaFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteFastqFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGafFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGctFile",
    contains = "PipetteDelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGmtFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGmxFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGrpFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteJsonFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteMafFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteMtxFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteOboFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipettePzfxFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteRDataFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteSamFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteRdsFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteTableFile",
    contains = "PipetteDelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteTsvFile",
    contains = "PipetteDelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteVcfFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteYamlFile",
    contains = "PipetteFile"
)
