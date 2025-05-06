

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import math.operation.textual.AdderTest;

@Suite
@SelectClasses({
    AdderStructureTest.class,
    AdderTest.class
})
public class AdderTestSuite {}

