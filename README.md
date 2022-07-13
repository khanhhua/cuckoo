# cuckoo
What is so special about cuckoo? It is the eggs.

## Examples

```hs
profile :: IO String
profile = do
  let 
    fullnameRandomizer = (,,,,,,,) 
      <$> fakeFirstName
      <*> fakeFamilyName
      <*> fakeNumber (10, 99)
      <*> fakeStreetName
      <*> fakeStateName
      <*> fakePhone "+49" 9
      <*> fakeJobTitle
      <*> fakeCompany
  g <- newStdGen
  ((fname, lname, number, streetName, stateName, phone, job, company), _nextG) <- runFake fullnameRandomizer g

  pure $ fname
    <> " " <> lname
    <> " living at " <> number
    <> " " <> streetName
    <> " " <> stateName
    <> " (" <> phone <> ") working as " <> job <> " for " <> company
```