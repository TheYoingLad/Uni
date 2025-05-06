package fib.main.part1;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

public class Tester2 {

	@Test
	public void test() {
		List<Integer> myContent = new ArrayList<>();
		assertEquals(0, myContent.size());
	}

	@Test
	public void test2() {
		List<Integer> myContent = new ArrayList<>();
		myContent.add(354);
		assertAll(
			() -> assertEquals(1, myContent.size()),
			() -> assertEquals(354, myContent.get(0))
		);
	}
}
