-- Verify that certain punctuation marks terminate paths correctly
  λ(x : ./example)
→ ./example[./example, ./example{bar = ./example<baz = ./example>, qux = ./example}, ./example]
