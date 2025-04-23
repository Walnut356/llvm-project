if(NOT LLDB_BUILT_STANDALONE)
  set(tablegen_deps intrinsics_gen)
endif()

add_lldb_library(lldbPluginExpressionParserRust
  RustUserExpression.cpp
  RustLex.cpp
  RustParse.cpp

  LINK_LIBS
    lldbCore
    lldbExpression
    lldbSymbol
    lldbTarget

  DEPENDS
  ${tablegen_deps}

  LINK_COMPONENTS
    Support
  )
