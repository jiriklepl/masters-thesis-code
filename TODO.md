# TO-DO list

- [ ] translate c-- to llvm <-
- [ ] change "Language" to "CMM"
- [x] rename Language.Parser.Utils to something more reasonable (renamed to Language.Parser.HasPos)
- LRAnalysis
  - [ ] make the errors/warnings found during flow analysis cleaner
  - [ ] refactor LRAnalysis
  - [ ] add "useless-write" warnings
  - [x] procedure arguments shouldn't be reported as uninitialized
  - [ ] split LRAnalysis into blockifying and flow analysis
- Flattener
  - [ ] fallthrough should be made explicit
- [ ] add haddock comments

  - [ ] Language

    - [ ] AST

      - [ ] Utils
      - [ ] Flattener
      - [ ] LRAnalysis

    - [ ] Utils
    - [ ] Lexer
    - [ ] Parser
    - [ ] Pretty
    - [ ] Translator
    - [x] Warnings

  - [ ] ParserTest
  - [ ] QuasiQuotes

- [ ] write unit tests

  - [ ] for binOp

- [ ] write QuickCheck tests
  - [ ] tests for re-parsing
- [ ] change `undefined` to `error s`
- [ ] draw a diagram for the architecture
  - [~] draw a diagram for the overall pipeline
  - [ ] draw a diagram for:
    - [ ] Lexer + Parser
    - [x] Flattener
    - [ ] LRAnalysis
    - [ ] Translator
