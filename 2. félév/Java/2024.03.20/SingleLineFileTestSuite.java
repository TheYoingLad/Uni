import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import text.to.numbers.*;

@Suite
@SelectClasses({
	SingleLineFileStructureTest.class,
	SingleLineFileTest.class
})
public class SingleLineFileTestSuite {}
