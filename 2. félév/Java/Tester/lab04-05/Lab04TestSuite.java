import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import famous.sequence.FibonacciTestSuite;

@Suite
@SelectClasses({
	// demo
	FibonacciTestSuite.class,

	// tasks
	TriangularNumbersTestSuite.class,
	IncrementTestSuite.class,
	MusicTestSuite.class,
	AdderTestSuite.class,

	// exercises
	CaesarCipherTestSuite.class,
	IncantationStructureTest.class,
	SoliloquyTestSuite.class
})
public class Lab04TestSuite {}
