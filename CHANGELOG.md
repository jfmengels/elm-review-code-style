# Changelog

## [1.1.1] - 2022-10-10

- Added more automatic fixes for [`NoUnnecessaryTrailingUnderscore`]. Thanks [@lydell](https://github.com/lydell)! ([#10](https://github.com/jfmengels/elm-review-code-style/pull/10))

## [1.1.0] - 2022-09-13

- Add new rule [`NoUnnecessaryTrailingUnderscore`]
- [`NoSimpleLetBody`] now simplifies let expressions when the body is a tuple.
```elm
a =
    let
        ( b, c ) =
            xyz
    in
    ( b, c )
-->
a =
    xyz
```

- [`NoSimpleLetBody`] now simplifies let expressions when the body is a tuple or a named pattern (not always, to prevent problems with phantom types).

```elm
b =
    let
        (A b) =
            xyz
    in
    A b
-->
b =
    xyz
```

- [`NoSimpleLetBody`] now offers an automatic fix even when the value to move is not the last one.

```elm
a =
    let
        -- This was previously not automatically fixed
        someThing = data

        data = "data"
    in
    someThing
```

## Missing changelog

Help would be appreciated to fill the blanks!

[`NoSimpleLetBody`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-code-style/latest/NoSimpleLetBody)
[`NoUnnecessaryTrailingUnderscore`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-code-style/latest/NoUnnecessaryTrailingUnderscore)

[1.1.0]: https://github.com/jfmengels/elm-review-code-style/releases/tag/1.1.0
