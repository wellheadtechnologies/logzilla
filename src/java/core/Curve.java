package core;
import java.util.List;

public interface Curve {
    Descriptor getDescriptor();
    List getData();
    Curve getIndex();
}