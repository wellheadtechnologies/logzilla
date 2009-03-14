package core;

import java.io.File;

public interface LasWriter {
    void writeLasFile(LasFile lf, File file);
    void writeLasFile(LasFile lf, String path);
}

