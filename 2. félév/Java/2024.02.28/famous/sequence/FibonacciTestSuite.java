package famous.sequence;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@SelectClasses({
    FibonacciStructureTest.class,
    FibonacciTest.class
})
@Suite public class FibonacciTestSuite {
    @SelectClasses({
        FibonacciStructureTest.class
    })
    @Suite public static class StructuralTests {}

    @SelectClasses({
        FibonacciTest.class
    })
    @Suite public static class FunctionalTests {}
}