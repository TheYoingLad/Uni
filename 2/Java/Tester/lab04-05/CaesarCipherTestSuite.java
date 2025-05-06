import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import cipher.CaesarCipherTest;

@Suite
@SelectClasses({
    CaesarCipherStructureTest.class,
    CaesarCipherTest.class
})
public class CaesarCipherTestSuite {}
