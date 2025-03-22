add_lldb_library(lldbPluginRustLanguageRuntime PLUGIN
  RustLanguageRuntime.cpp

  LINK_LIBS
    lldbCore
    lldbSymbol
    lldbTarget
  LINK_COMPONENTS
    Support
  )
