# elm-review-code-style

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to follow some of my personal code style preferences.

I tend to not have many rules related to code style thanks to using `elm-format` and Elm's simple language,
and I think that `elm-review` brings more values by reporting different kinds of issues than code style infringements,
but I do think that there are use-cases for it.

A few warnings before trying to add them to your review configuration.
1. These rules enforce opinions **I personally** have on "nicer" Elm code, and honestly they're mostly about resolving things I find relatively annoying. Do not enforce the ones your or your team disagrees with in your project.

2. These rules may be a source of more frustration (when the tests fails because of them) for your team and a source of work that will bring little value to your project. I try to provide fixes when I can to reduce that work though!

With that said, I recommend [trying them out](#try-it-out) to help you decide.

## Provided rules

- [🔧 `NoSimpleLetBody`](https://package.elm-lang.org/packages/jfmengels/elm-review-code-style/1.0.0/NoSimpleLetBody "Provides automatic fixes") - Reports when a let expression's body is a simple reference to a value declared in the let expression.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoSimpleLetBody
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoSimpleLetBody.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example
```
