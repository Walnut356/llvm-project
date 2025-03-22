add_lldb_library(lldbPluginTypeSystemRust PLUGIN
  TypeSystemRust.cpp

  LINK_LIBS
    lldbCore
    lldbSymbol
    lldbTarget
    lldbUtility
    lldbPluginSymbolFileDWARF
    lldbPluginSymbolFilePDB
  LINK_COMPONENTS
    Support
)
