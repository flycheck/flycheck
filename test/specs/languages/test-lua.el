;;; test-lua.el --- Flycheck Specs: Lua -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Lua"
  (flycheck-buttercup-def-checker-test lua-luacheck lua syntax-error
    (flycheck-buttercup-should-syntax-check
     "language/lua/syntax-error.lua" 'lua-mode
     '(5 7 error "unfinished string" :id "E011" :checker lua-luacheck)))

  (flycheck-buttercup-def-checker-test lua-luacheck lua warnings
    (flycheck-buttercup-should-syntax-check
     "language/lua/warnings.lua" 'lua-mode
     '(1 1 warning "setting non-standard global variable 'global_var'"
         :id "W111" :checker lua-luacheck)
     '(3 16 warning "unused function 'test'"
         :id "W211" :checker lua-luacheck)
     '(3 21 warning "unused argument 'arg'"
         :id "W212" :checker lua-luacheck)
     '(4 11 warning "variable 'var2' is never set"
         :id "W221" :checker lua-luacheck)))

  (flycheck-buttercup-def-checker-test lua-luacheck lua custom-luacheckrc
    (let ((flycheck-luacheckrc "custom.luacheckrc"))
      (flycheck-buttercup-should-syntax-check
       "language/lua/warnings.lua" 'lua-mode
       '(1 1 warning "setting non-standard global variable 'global_var'"
           :id "W111" :checker lua-luacheck)
       '(3 16 warning "unused function 'test'"
           :id "W211" :checker lua-luacheck)
       '(4 11 warning "variable 'var2' is never set"
           :id "W221" :checker lua-luacheck))))

  (flycheck-buttercup-def-checker-test lua-luacheck lua custom-standards
    (let ((flycheck-luacheck-standards '("ngx_lua")))
      (flycheck-buttercup-should-syntax-check
       "language/lua/ngx_lua.warnings.lua" 'lua-mode
       '(3 16 warning "unused function 'test'"
           :id "W211" :checker lua-luacheck)
       '(3 21 warning "unused argument 'arg'"
           :id "W212" :checker lua-luacheck)
       '(4 11 warning "variable 'var2' is never set"
           :id "W221" :checker lua-luacheck))))

  (flycheck-buttercup-def-checker-test lua lua nil
    (let ((flycheck-disabled-checkers '(lua-luacheck)))
      (flycheck-buttercup-should-syntax-check
       "language/lua/syntax-error.lua" 'lua-mode
       '(5 nil error "unfinished string near '\"oh no'"
           :checker lua)))))

;;; test-lua.el ends here
