# Well Typed Qubits

### TODO
- [x] Implement length-indexed lists
- [x] Modify previous qubit implementation to use these lists
- [x] Generalize Keys to access lists as labels
- [x] Implement a type-safe way to access sublists of a list
- [x] Implement type errors for invalid sublists
- [x] QAct ~ ReaderT(IO) monad to handle operations on qubits
- [x] Refactor length-indexed lists as first-class type families
- [ ] Refactor type-errors to be more informative
- [ ] Convince the type checker that if a list is a valid selector on a qubit, then all its sublists are also valid selectors
- [ ] Reimplement add function to receive keys and depend on the paramethers of the operation

