package xsbti;

import java.io.File;

/**
 * Represents a classfile on disk, which may be either loose in a directory, or inside a jar.
 */
public interface FileRef
{
  /**
   * @return The File containing this class, which will be either a .jar file or
   * a loose .class file.
   */
  File containingFile();
}
