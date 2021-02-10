# elm-review-code-style

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to follow some of my personal code style preferences.

I tend to not have many rules thanks to using `elm-format` and I don't think that `elm-review` serves a better purposes than reporting code style infringements, but I do think that there are use-cases for it. 


## Provided rules

- [`NoSimpleLetBody`](https://package.elm-lang.org/packages/jfmengels/elm-review-code-style/1.0.0/NoSimpleLetBody) - Reports when a let expression's body is a simple reference to a value declared in the let expression.
- [`NoUnnecessaryTrailingUnderscore`](https://package.elm-lang.org/packages/jfmengels/elm-review-code-style/1.0.0/NoUnnecessaryTrailingUnderscore) - Reports unnecessary or suboptimal trailing underscores in variable names.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnnecessaryTrailingUnderscore.rule
    , NoSimpleLetBody.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example
```
