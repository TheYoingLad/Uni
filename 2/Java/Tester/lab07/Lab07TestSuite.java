
import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses({
	DataProcessorStructureTest.class
	, SingleLineFileStructureTest.class
	, MultiLineFileStructureTest.class
	, SumCheckerStructureTest.class
    , StatisticsTest.class
    , FileContentTest.class

})
public class Lab07TestSuite {}
