/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbti;

import java.io.File;
import java.io.IOException;

/**
 * Interface differentiate Jar outputs from Directory outputs.
 */
public abstract class OutputLocation {
  protected final File file;
  protected OutputLocation(File f) {
    this.file = f;
  }

  /**
   * The directory which this output will be placed into.
   */
  protected abstract File directory();

  /**
   * @return The File for this OutputLocation, which may be either a jar or directory.
   */
  public final File file() {
    return this.file;
  }

  /**
   * Prepares this location to receive output by creating any necessary parent directories.
   */
  public final void prepare() throws IOException {
    this.directory().mkdirs();
  }

  /**
   * @return Creates an OutputLocation for the given file, which will represent either
   * a jar or directory.
   */
  public static OutputLocation create(File file) {
    if (endsWithJar(file)) {
      return new JarOutputLocation(file);
    }
    return new DirectoryOutputLocation(file);
  }

  protected static boolean endsWithJar(File file) {
    return file.getName().toLowerCase().endsWith(".jar");
  }
}
