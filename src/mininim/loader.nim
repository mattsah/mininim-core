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

macro scan(folder: static[string], ext: static[string]) =
    result = newStmtList()

    for file in os.walkDirRec(folder, checkDir=true):

        if file.endsWith(ext):
            result.add(nnkImportStmt.newTree(newIdentNode(file[0 ..< ^ext.len])))

scan("./local", ".nim")

when defined(debug):
    echo "Total shapes loaded: ", config.len