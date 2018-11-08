data ShirtSize = Small | Medium | Large | ExtraLarge

myShirtSize = Medium

main =
  putStrLn (case myShirtSize of
      Small -> "Eat more spinach!"
      Medium -> "You're just average."
      Large -> "Is the air thinner up there?"
      ExtraLarge -> "Wow, you're HUUUUGE!"
    )
