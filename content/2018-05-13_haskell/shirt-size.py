from enum import Enum

ShirtSize = Enum('ShirtSize', 'Small Medium Large Huge')

johnsSize = ShirtSize.Medium

if johnsSize == ShirtSize.Small:
    print("Eat more spinach!")

elif johnsSize == ShirtSize.Medium:
    print("You're just average.")

elif johnsSize == ShirtSize.Large:
    print("Is the air thinner up there?")
