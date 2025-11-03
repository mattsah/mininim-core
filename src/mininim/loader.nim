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

    result.add(
        quote do:
            {. warning[UnusedImport]:off .}
    )

    for file in os.walkDirRec(folder, checkDir=true):
        if file.endsWith(ext):
            result.add(nnkImportStmt.newTree(newIdentNode(file[0 ..< ^ext.len])))

    result.add(quote do:
        when defined(debug):
            echo fmt "message[{align($config.len, 3, '0')}]: total facets loaded"
    )

