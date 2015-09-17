package xsbti;

import java.io.File;

/**
 * Represents a file on disk, which may be either loose in a directory, or inside a jar.
 */
public interface FileRef
{
  /**
   * @return The File "containing" this file, which will be either a .jar file or
   * a loose File itself.
   */
  File containingFile();

  /**
   * @return True if the file is a class.
   */
  boolean isClass();
}
