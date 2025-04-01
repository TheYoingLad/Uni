package math.operation.textual;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import math.operation.textual.AdderTest;

@Suite
@SelectClasses({
    AdderStructureTest.class,
    AdderTest.class,
    AdderInvalidTest.class
})
public class AdderTestSuite {}

