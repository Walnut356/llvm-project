add_lldb_library(lldbPluginTypeSystemRust PLUGIN
  TypeSystemRust.cpp

  LINK_LIBS
    lldbCore
    lldbSymbol
    lldbTarget
    lldbUtility
    lldbPluginExpressionParserRust
    lldbPluginSymbolFileDWARF
    lldbPluginSymbolFilePDB
  LINK_COMPONENTS
    DebugInfoPDB
    Support
)
