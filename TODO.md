# TO-DO list

- [x] add monotype checker (or whatever)

- [x] translate c-- to llvm <-

- [x] change "Language" to "CMM"

- [x] rename Language.Parser.Utils to something more reasonable (renamed to Language.Parser.GetPos)

- LRAnalysis
  - [x] abstract out details about registering stuff
  - [x] procedure arguments shouldn't be reported as uninitialized
  - [x] split LRAnalysis into blockifying and flow analysis

- [x] Blockifier
  - [x] refactor Blockifier

- [x] FlowAnalysis
  - [x] make the errors/warnings cleaner
  - [ ] add "useless-write" warnings? (dead write)
  - [x] errors/warnings should output to stderr
  - [x] refactor FlowAnalysis

- [x] Move CMM.Pretty to CMM.AST.Pretty
- [x] Pretty types

- [x] Flattener
  - [x] fallthrough should be made explicit

- [ ] add haddock comments
  - [ ] CMM
    - [ ] AST
      - [ ] Annot
      - [x] BlockAnnot
        - [x] State
      - [x] Blockifier
      - [x] Flattener
      - [x] GetName
      - [~] Maps
      - [x] Utils
      - [x] Variables
    - [x] Control
      - [x] Applicative
    - [~] FlowAnalysis
    - [ ] Inference
      - [x] BuiltIn
      - [ ] Preprocess
        - [x] State
      - [ ] State
      - [ ] Type
    - [ ] Lens
    - [ ] Lexer
    - [ ] Parser
      - [ ] GetPos
    - [ ] Pretty
    - [ ] Translator
      - [ ] State
    - [ ] Utils

  - [ ] ParserTest
  - [ ] QuasiQuotes

- [x] Inference preprocessing
  - [x] check whether all `subType`s and `instType`s follow the correct order
  - [x] check correctness
- [x] Inference
  - [x] Type classes
    - [x] MPTC

- [x] Type
  - [x] check correctness of lambdize&forall -> removed

- [x] write unit tests
  - [x] for binOp: precedence, associativity etc

- [x] change `undefined` to `error s`

- [~] draw a diagram for the architecture
  - [x] draw a diagram for the overall pipeline
  - [ ] draw a diagram for:
    - [ ] Lexer + Parser
    - [x] Flattener
    - [ ] FlowAnalysis
    - [ ] Blockifier
    - [ ] Variables
    - [ ] Inference Preprocess
    - [ ] Inference
    - [ ] Translator
