# TO-DO list

- [ ] translate c-- to llvm <-
- [x] change "Language" to "CMM"
- [x] rename Language.Parser.Utils to something more reasonable (renamed to Language.Parser.HasPos)
- LRAnalysis
  - [x] abstract out details about registering stuff
  - [x] procedure arguments shouldn't be reported as uninitialized
  - [x] split LRAnalysis into blockifying and flow analysis
- Blockifier
  - [~] refactor Blockifier
- FlowAnalysis
  - [ ] make the errors/warnings cleaner
  - [ ] add "useless-write" warnings
- Flattener
  - [x] fallthrough should be made explicit
- [ ] add haddock comments

  - [ ] Language

    - [ ] AST

      - [ ] Utils
      - [ ] Flattener
      - [ ] Blockifier
      - [ ] BlockifierState

    - [ ] FlowAnalysis
    - [ ] Utils
    - [ ] Lexer
    - [ ] Parser
      - [ ] HasPos
    - [ ] Pretty
    - [ ] Translator
    - [ ] TranslState
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
    - [ ] FlowAnalysis
    - [ ] Blockifier
    - [ ] Translator
