

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses({
    IncrementStructureTest.class,
    IncrementTestSuite.class
})
public class IncrementTestSuite {}

