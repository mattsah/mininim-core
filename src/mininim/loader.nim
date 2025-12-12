{. warning[UnusedImport]:off .}

import
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
            result.add(nnkImportStmt.newTree(newIdentNode(file[folder.high ..< ^ext.len])))

    result.add(quote do:
        when defined(debug):
            echo fmt "message[{align($Config.len, 3, '0')}]: total facets loaded"
    )

