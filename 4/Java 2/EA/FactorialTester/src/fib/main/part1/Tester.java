package fib.main.part1;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import enums.EnumDemo;

public class Tester {
	@Test
	public void test() {
		assertEquals(1, Factorial.factorial(1));
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
		1, 1, true, abc def, E2
		3, 6, true, abc def, E2
		6, 720, true, abc def, E2
	""")
	public void test(int arg, int expected, boolean fake1, String fake2, EnumDemo fake3) {
		assertEquals(expected, Factorial.factorial(arg));
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
		1, 1, true, abc def, E2
		3, 6, true, abc def, E2
		6, 720, true, abc def, E2
	""")
	public void test2(int arg, int expected, boolean fake1, String fake2, EnumDemo fake3) {
//		org.junit.jupiter.api.Assertions.assertEquals(expected, Factorial.factorial(arg));
//		Assertions.assertEquals(expected, Factorial.factorial(arg));

		assertAll(
			() -> assertEquals(expected, Factorial.factorial(arg)),
			() -> assertEquals(expected, Factorial.factorial(arg))
		);

	}

	@Test
	public void test3() {
		int[] result = { 1, 2, 6 };
		assertArrayEquals(new int[] { 1, 2, 6 }, result);
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
		-2, -2 is below zero!
		-12, -12 is below zero!
	""")
	public void test4(int arg, String msg) {
		var ex = assertThrows(IllegalArgumentException.class, () -> {
//			Factorial.factorial(5);
			Factorial.factorial(arg);
		});

		assertEquals(msg, ex.getMessage());

//		assertSame();

		
		var doubleResult = Math.sqrt(2);
		assertEquals(1.4142, doubleResult, 0.01);
		assertEquals(1.4142, doubleResult, "explanation");
		assertEquals(1.4142, doubleResult, 0.01, () -> "explanation");
	}



}
