import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import magic.SoliloquyTest;

@Suite
@SelectClasses({
    SoliloquyStructureTest.class,
    SoliloquyTest.class
})
public class SoliloquyTestSuite {}
