
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import famous.sequence.TriangularNumbersTest;

@Suite
@SelectClasses({
	TriangularNumbersStructureTest.class,
	TriangularNumbersTest.class
})
public class TriangularNumbersTestSuite {

}
