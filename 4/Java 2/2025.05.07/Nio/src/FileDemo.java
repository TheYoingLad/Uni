import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class FileDemo {
    public static void main(String[] args) throws Exception {
        try (var fc = FileChannel.open(Path.of("Nio/src", "FileDemo.java"), StandardOpenOption.READ);) {
            var buf = ByteBuffer.allocate(1);
//            fc.position(5); // set position
            int byteCount = fc.read(buf);

            System.out.println(buf.get(0));

            buf.flip(); // vissza az elejére
            System.out.println(buf.get());
        }
    }
}
