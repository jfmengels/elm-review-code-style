# elm-review-code-style

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`NoUnnecessaryTrailingUnderscore`](https://package.elm-lang.org/packages/jfmengels/elm-review-code-style/1.0.0/NoUnnecessaryTrailingUnderscore) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnnecessaryTrailingUnderscore
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnnecessaryTrailingUnderscore.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example
```
