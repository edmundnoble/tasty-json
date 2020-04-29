Write [tasty](https://hackage.haskell.org/packages/tasty) test results to
a file in JSON format.

Example:

```haskell
defaultMainWithIngredients (consoleAndJsonReporter : defaultIngredients) tests
```

