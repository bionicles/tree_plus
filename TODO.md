# Do First

# Backlog
- [ ] Fix ordering of leaves by line number consistently across languages
    - [ ] Fix Python ordering
    - [ ] Fix JavaScript / TypeScript ordering
    - [ ] Review other language unit tests for ordering
- [ ] expand test cases for the supported languages to verify ordering & completeness
- [ ] Upgrade CICD GitHub Action
    - [ ] Build and deploy to PyPi if tests pass
- [ ] test file input instead of directory
- [ ] measure test coverage
- [ ] test named lambdas in python
- [ ] autoinstall tree-sitter grammars to enable real parsing instead of regex if available
- [ ] modularize the tree_plus function to separate index creation from tree creation
- [ ] brainstorm and integrate more flags / options to control results
    - [ ] add a todo flag to only show todos and unchecked checkboxes
    - [ ] add a search / filter input to filter results
- [ ] make a tree_scan to apply AI LLMs to the tree
    - [ ] e.g. to suggest todos
    - [ ] to write docs
    - [ ] to write tests
    - [ ] to write code
    - [ ] to translate languages (e.g. 'rewrite it in rust' example)


# Nice to have
- [ ] Support more languages
    - [ ] SQL (create tables, procedures)
    - [ ] C#
    - [ ] C++
    - [ ] OpenAPI yaml
    - [ ] Json RPC 2.0 schemas
    - [ ] GRPC schemas
    - [ ] GraphQL schemas
    - [ ] Go
    - [ ] Dart
    - [ ] Swift

# Done
    - [x] Add Grid testing to CI (windows, mac, linux) x (Python 3.9 ... 3.12 alpha)