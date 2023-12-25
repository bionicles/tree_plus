-- haskell_test.hs
data Person = Person String

greet :: Person -> String
greet (Person name) = "Hello, " ++ name

-- a more realistic example of a multiline signature from https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/GraphQL/Execute/Resolve.hs
resolveVariables ::
  forall m fragments.
  (MonadError QErr m, Traversable fragments) =>
  Options.BackwardsCompatibleNullInNonNullableVariables ->
  [G.VariableDefinition] ->
  GH.VariableValues ->
  [G.Directive G.Name] ->
  G.SelectionSet fragments G.Name ->
  m
    ( [G.Directive Variable],
      G.SelectionSet fragments Variable
    )
resolveVariables nullInNonNullableVariables definitions jsonValues directives selSet = do
  variablesByName <- HashMap.groupOnNE getName <$> traverse buildVariable definitions
  uniqueVariables <- flip
    HashMap.traverseWithKey
    variablesByName
    \variableName variableDefinitions ->
      case variableDefinitions of
        a :| [] -> return a
        _ ->
          throw400 ParseFailed
            $ "multiple definitions for variable "
            <>> variableName
  ((directives', selSet'), usedVariables) <- flip runStateT mempty $ do
    d <- traverse (traverse (resolveVariable uniqueVariables)) directives
    s <- traverse (traverse (resolveVariable uniqueVariables)) selSet
    pure (d, s)
  let variablesByNameSet = HS.fromList . HashMap.keys $ variablesByName
      jsonVariableNames = HS.fromList $ HashMap.keys jsonValues
      -- At the time of writing, this check is disabled using
      -- a local binding because, the master branch doesn't implement this
      -- check.
      isVariableValidationEnabled = False
