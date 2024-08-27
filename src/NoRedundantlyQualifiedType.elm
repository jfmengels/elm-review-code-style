module NoRedundantlyQualifiedType exposing (rule)

{-|

@docs rule

-}

import Review.Rule as Rule exposing (Rule)


{-| Reports when a type is qualified by a module (alias) of the same name.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoRedundantlyQualifiedType.rule
        ]

There is a convention in Elm to center modules around a type, and to name the module after the type. Usually, such a module is imported like this:

    import Inventory.Fruit as Fruit exposing (Fruit)

That allows you to refer to values like `Fruit.apple` and to the type `Fruit`. When inferring types, you might end up with `Fruit.Fruit`. It can be considered a bit silly to say the same word twice like that. It's clear where the `Fruit` type comes from anyway. This rule simplifies to just `Fruit`.


## Fail

    import Set

    directions : Set.Set String
    directions =
        Set.fromList [ "north", "south", "east", "west" ]


## Success

    import Set exposing (Set)

    directions : Set String
    directions =
        Set.fromList [ "north", "south", "east", "west" ]


## When (not) to enable this rule

This rule resolves a minor style issue, and may not be worth enforcing depending on how strongly you feel about this issue.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoRedundantlyQualifiedType
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoRedundantlyQualifiedType" ()
        -- Add your visitors
        |> Rule.fromModuleRuleSchema