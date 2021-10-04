package recursions;

public class Recursions {

	public static void main(String[] args) {

		System.out.println(fibonacci(3));

		/**
		 * Aufgabe c) Hier wird 2 ausgegeben und die Funktion fibonacci_recursiv wird
		 * insgesamt 5 mal aufgerufen.
		 */
		long ergebnis = fibonacci_recursiv(3);
		System.out.println(ergebnis);

		System.out.println(palindrom("OTTO", 0));
	}

	/** Funktion zum berechnen der Ficonacci Folge (Iterativ) */
	private static long fibonacci(int n) {
		long result = 0;
		long x = 0;
		long y = 1;

		for (int i = 1; i < n; i++) {
			result = x + y;
			x = y;
			y = result;
		}
		return result;
	}

	/** Funktion zum berechnen der Ficonacci Folge (Rekursiv) */
	private static long fibonacci_recursiv(int n) {
		if (n == 0) {
			return 0;
		}
		if (n == 1) {
			return 1;
		}
		return fibonacci_recursiv(n - 1) + fibonacci_recursiv(n - 2);
	}

	/** Funktion zum checken von Palindromen (Rekursiv) */
	private static boolean palindrom(String word, int i1) {
		if (word.length() % 2 != 0) {
			return false;
		}
		char[] chars = word.toCharArray();
		int i2 = chars.length - 1 - i1;
		char x = chars[i1];
		char y = chars[i2];

		if (i1 < i2) {
			if (x != y) {
				return false;
			}
			if (x == y) {
				return palindrom(word, i1 + 1);
			}
		}
		return true;
	}
}
