# Open questions

## Exercise 4

Depending on the order you have given in you grammar it can do left or right recursion. You can tell the grammar to add every new prop on the right or left of props.

```
-- Left-recursive
prods : prod                  { [$1] }
      | prods prod            { $2 : $1 }

-- Right-recursive
prods : prod                  { [$1] }
      | prod prods            { $1 : $2 }
```

If we want to do this in parser combinators we can use a combination of folds and flips to create right- or left-associativity.

```haskell
chainl :: Parser s a → Parser s (a → a → a) → Parser s a
chainr :: Parser s a → Parser s (a → a → a) → Parser s a
chainl p s =
  foldl (flip ($)) <$> p <∗> many (flip <$> s <∗> p)
chainr p s =
  flip (foldr ($)) <$> many (flip ($) <$> p <∗> s) <∗> p
```


## Exercise 10
Yes it does matter. Changing the moment when to add the will also affect the outcome of the space, because you change the order the tasks will be done.