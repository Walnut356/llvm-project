add_lldb_library(lldbPluginRustLanguage PLUGIN
  RustLanguage.cpp
  Slice.cpp
  Str.cpp
  Vec.cpp
  String.cpp
  SumType.cpp

  LINK_LIBS
    lldbCore
    lldbDataFormatters
    lldbSymbol
    lldbTarget
    lldbPluginTypeSystemRust
  LINK_COMPONENTS
    Support
)
