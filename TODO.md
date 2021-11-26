# TO-DO list

- [ ] translate c-- to llvm <-

- [x] change "Language" to "CMM"

- [x] rename Language.Parser.Utils to something more reasonable (renamed to Language.Parser.HasPos)

- LRAnalysis
  - [x] abstract out details about registering stuff
  - [x] procedure arguments shouldn't be reported as uninitialized
  - [x] split LRAnalysis into blockifying and flow analysis

- [x] Blockifier
  - [~] refactor Blockifier

- [x] FlowAnalysis
  - [ ] make the errors/warnings cleaner
  - [ ] add "useless-write" warnings
  - [ ] errors/warnings should output to stderr
  - [ ] refactor FlowAnalysis

- [x] Flattener
  - [x] fallthrough should be made explicit

- [ ] add haddock comments
  - [ ] CMM
    - [ ] AST
      - [ ] Utils
      - [ ] HasPos
      - [ ] Annot
      - [ ] Flattener
      - [ ] Blockifier
        - [ ] State
      - [x] BlockAnnot
    - [ ] FlowAnalysis
    - [ ] Utils
    - [ ] Lexer
    - [ ] Parser
      - [ ] HasPos
    - [ ] Pretty
    - [ ] Translator
      - [ ] State
    - [ ] Inference
      - [ ] BuiltIn
        - [ ] Operator
      - [ ] Type
      - [ ] Preprocess
        - [ ] State
    - [x] Warnings

  - [ ] ParserTest
  - [ ] QuasiQuotes

- [ ] Inference preprocessing
- [ ] Inference
  - [ ] Type classes
    - [ ] MPTC

- [ ] write unit tests
  - [ ] for binOp

- [ ] write QuickCheck tests
  - [ ] tests for re-parsing

- [x] change `undefined` to `error s`

- [~] draw a diagram for the architecture
  - [x] draw a diagram for the overall pipeline
  - [ ] draw a diagram for:
    - [ ] Lexer + Parser
    - [x] Flattener
    - [ ] FlowAnalysis
    - [ ] Blockifier
    - [ ] Translator
