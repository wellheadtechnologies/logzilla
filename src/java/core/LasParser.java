package core;

import java.io.File;

public interface LasParser {
    LasFile parseLasFile(File file);
    LasFile parseLasFile(String path);
}