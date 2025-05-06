import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import text.to.numbers.*;

@Suite
@SelectClasses({
	MultiLineFileStructureTest.class,
	MultiLineFileTest.class
})
public class MultiLineFileTestSuite {}
