import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class ZipReader {
    public static void main(String[] args) throws IOException {
        try (var fc = FileChannel.open(Path.of("Nio", "input.zip"), StandardOpenOption.READ);) {
            var header = ByteBuffer.allocate(30).order(ByteOrder.LITTLE_ENDIAN);
            fc.read(header);

//            while (buf.hasRemaining()) {
            int fileSize = header.getInt(18);
            short fileNameSize = header.getShort(26);

            System.out.println(fileSize);
            System.out.println(fileNameSize);

            fc.position(30);
            var name = ByteBuffer.allocate(fileNameSize).order(ByteOrder.LITTLE_ENDIAN);
            fc.read(name);

            String fileName = new String(name.array());
            System.out.println(fileName);

            fc.position(30 + fileNameSize);
            var data = ByteBuffer.allocate(fileSize).order(ByteOrder.LITTLE_ENDIAN);
            fc.read(data);

            String content = new String(data.array());
            System.out.println(content);
        }
    }
}

