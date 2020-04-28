public class ShirtSizeTest {
  enum ShirtSize { Small, Medium, Large, Huge }

  public static void main (String[] args) {
    ShirtSize johnsSize = ShirtSize.Huge;

    switch (johnsSize) {
      case Small: System.out.println("Small"); break;
      case Medium: System.out.println("Medium"); break;
      case Large: System.out.println("Large"); break;
    }
  }
}

