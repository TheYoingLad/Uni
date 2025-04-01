package fib.main.part1;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

public class Tester2Better {
	List<Integer> myContent;

	@BeforeEach
	public void beforeEach() {
		 myContent = new ArrayList<>();
	}

	@Test
	public void test() {
		assertEquals(0, myContent.size());
	}

	@Nested
	public class AnotherStep1 {	
		@Test
		public void test() {
			assertEquals(0, myContent.size());
		}
	}

	@Nested
	public class Step1 {
		@BeforeEach
		public void beforeEach() {
			myContent.add(354);
		}

		@Test
		public void test() {
			assertAll(
				() -> assertEquals(1, myContent.size()),
				() -> assertEquals(354, myContent.get(0))
			);
		}

		@Nested
		public class Step2 {
			@BeforeEach
			public void beforeEach() {
				myContent.remove(0);
			}

			@Test
			public void test() {
				assertAll(
					() -> assertEquals(0, myContent.size())
				);
			}
		}
}
}
