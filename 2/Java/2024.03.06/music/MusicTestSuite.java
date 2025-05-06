package music;
import music.recording.*;
import music.fan.*;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

import music.fan.FanTest;
import music.recording.ArtistTest;
import music.recording.RecordLabelTest;

@Suite
@SelectClasses({
    RecordLabelStructureTest.class,
    ArtistStructureTest.class,
    FanStructureTest.class,

    RecordLabelTest.class,
    ArtistTest.class,
    FanTest.class
})
public class MusicTestSuite {}
