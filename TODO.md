# TO-DO list

- [ ] add monotype checker (or whatever)

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

- [ ] Move CMM.Pretty to CMM.AST.Pretty
- [ ] Pretty types

- [x] Flattener
  - [x] fallthrough should be made explicit

- [ ] add haddock comments
  - [ ] CMM
    - [ ] AST
      - [ ] Annot
      - [x] BlockAnnot
        - [ ] State
      - [ ] Blockifier
      - [ ] Flattener
      - [ ] GetName
      - [ ] Maps
      - [ ] Utils
      - [ ] Variables
    - [-] Control
      - [ ] Applicative
    - [ ] FlowAnalysis
    - [ ] Inference
      - [ ] BuiltIn
        - [ ] Operator
      - [ ] Preprocess
        - [ ] State
      - [ ] State
      - [ ] Type
    - [ ] Lens
    - [ ] Lexer
    - [ ] Parser
      - [ ] HasPos
    - [ ] Pretty
    - [ ] Translator
      - [ ] State
    - [ ] Utils

  - [ ] ParserTest
  - [ ] QuasiQuotes

- [x] Inference preprocessing
  - [ ] check whether all `subType`s and `instType`s follow the correct order
  - [ ] check correctness
- [ ] Inference
  - [ ] Type classes
    - [ ] MPTC

- [ ] Type
  - [ ] check correctness of lambdize&forall

- [ ] write unit tests
  - [ ] for binOp: precedence, associativity etc

- [ ] write QuickCheck tests
  - [ ] tests for re-parsing
  - [ ] tests for variables (global, local)
  - [ ] tests for unification?

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
