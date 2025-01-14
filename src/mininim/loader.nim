{. warning[UnusedImport]:off .}

import
    std/os,
    std/macros,
    std/strutils,
    mininim

export
    mininim

#
# Config
#

macro scan*(folder: static[string], ext: static[string] = ".nim") =
    result = newStmtList()

    for file in os.walkDirRec(folder, checkDir=true):
        if file.endsWith(ext):
            result.add(nnkImportStmt.newTree(newIdentNode(file[0 ..< ^ext.len])))

    result.add(quote do:
        when defined(debug):
            echo "Total facets loaded: ", config.len
    )

