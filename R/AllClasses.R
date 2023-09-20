#' File extension classes
#'
#' Currently intended for use with `import` function.
#'
#' @export
#' @note Updated 2023-09-20.
#'
#' @section Primary classes:
#'
#' - `PipetteBAMFile`: Binary sequence alignment map (BAM).
#' - `PipetteBCFFile`: Binary variant call format (BCF).
#' - `PipetteBcbioCountsFile`: bcbio-nextgen counts file.
#' - `PipetteCRAMFile`: Compressed reference-oriented alignment map (CRAM).
#' - `PipetteCSVFile`: Comma-separated values file (CSV).
#' - `PipetteFASTAFile`: FASTA file.
#' - `PipetteFASTQFile`: FASTQ file.
#' - `PipetteGCTFile`: Gene Cluster Text file (GCT).
#' - `PipetteGMTFile`: Gene matrix transposed file (GMT).
#' - `PipetteGMXFile`: Gene matrix file (GMX).
#' - `PipetteGRPFile`: Gene set file (GRP).
#' - `PipetteJSONFile`: JSON file.
#' - `PipetteMAFFile`: Mutation annotation format file (MAF).
#' - `PipetteMTXFile`: MatrixMarket exchange file (MTX).
#' - `PipetteOBOFile`: Open Biomedical Ontologies file (OBO).
#' - `PipettePZFXFile`: GraphPad Prism file (PZFX).
#' - `PipetteRDSFile`: R data file containing a single, serialized object (RDS).
#' - `PipetteRDataFile`: R Data file containing multiple objects (RData/RDA).
#' - `PipetteSAMFile`: Sequence alignment map (SAM).
#' - `PipetteTSVFile`: Tab-separated values file (TSV).
#' - `PipetteTableFile`: Base R table file (TXT).
#' - `PipetteVCFFile`: Variant call format (VCF).
#' - `PipetteYAMLFile`: YAML file.
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
    contains = "SimpleList"
)
setValidity(
    Class = "PipetteFile",
    method = function(object) {
        validate(
            isString(object[["resource"]]),
            isString(object[["origResource"]], nullOK = TRUE)
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
    Class = "PipetteBAMFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteBCFFile",
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
    Class = "PipetteCRAMFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteCSVFile",
    contains = "PipetteDelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteFASTAFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteFASTQFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGCTFile",
    contains = "PipetteDelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGMTFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGMXFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteGRPFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteJSONFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteMAFFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteMTXFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteOBOFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipettePZFXFile",
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
    Class = "PipetteSAMFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteRDSFile",
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
    Class = "PipetteTSVFile",
    contains = "PipetteDelimFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteVCFFile",
    contains = "PipetteFile"
)

#' @rdname PipetteFile-class
#' @export
setClass(
    Class = "PipetteYAMLFile",
    contains = "PipetteFile"
)
