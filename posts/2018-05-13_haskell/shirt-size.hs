data ShirtSize = Small | Medium | Large | Huge

johnsSize = Medium

main =
  putStrLn (case johnsSize of
      Small -> "Eat more spinach!"
      Medium -> "You're just average."
      Large -> "Is the air thinner up there?"
    )
