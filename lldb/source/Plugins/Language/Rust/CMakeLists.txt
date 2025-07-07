add_lldb_library(lldbPluginRustLanguage PLUGIN
  Utils.cpp
  RustLanguage.cpp
  Slice.cpp
  Str.cpp
  Vec.cpp
  String.cpp
  SumType.cpp
  RustStdLib.cpp
  HashMap.cpp

  LINK_LIBS
    lldbCore
    lldbDataFormatters
    lldbSymbol
    lldbTarget
    lldbPluginTypeSystemRust
  LINK_COMPONENTS
    Support
)
