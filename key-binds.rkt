#lang s-exp framework/keybinding-lang
(keybinding "c:l" (λ (editor evt) (send editor insert "λ")))
