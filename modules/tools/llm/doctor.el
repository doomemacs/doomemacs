;;; tools/llm/doctor.el -*- lexical-binding: t; -*-

(when (and (modulep! +mcp)
           (< emacs-major-version 30))
  (error! "MCP requires Emacs version >= 30. MCP will not work."))
