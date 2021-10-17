# TO-DO list

- [ ] translate c-- to llvm <-
- [ ] split LRAnalysis into blockifying and flow analysis
- [ ] change "Language" to "CMM"
- [ ] rename Language.Parser.Utils to something more reasonable
- [ ] procedure arguments shouldn't be reported as uninitialized
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

- [ ] add "useless-write" warnings
- [ ] make the errors/warnings found during flow analysis cleaner
- [ ] change `undefined` to `error s`
- [ ] refactor LRAnalysis
- [ ] draw a diagram for the architecture
  - [~] draw a diagram for the overall pipeline
  - [ ] draw a diagram for:
    - [ ] Lexer + Parser
    - [x] Flattener
    - [ ] LRAnalysis
    - [ ] Translator
