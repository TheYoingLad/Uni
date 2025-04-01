package test;

import buggy.Buggy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

@Timeout(5)
class BuggyTest {

    @Test
    void timeOut() {
        Buggy.timeOut();
    }
}