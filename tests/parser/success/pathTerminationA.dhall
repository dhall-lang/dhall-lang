-- Verify that certain punctuation marks terminate paths correctly
  λ(x : ./example)
→ [./example, {bar = <baz = ./example>, qux = ./example}, ./example]
