package core;
import java.util.List;

public interface Header {
    String getType();
    String getPrefix();
    List<Descriptor> getDescriptors();
    Descriptor getDescriptor(String name);
}