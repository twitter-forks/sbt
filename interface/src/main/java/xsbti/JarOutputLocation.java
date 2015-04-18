/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbti;

import java.io.File;
import java.io.IOException;

public final class JarOutputLocation extends OutputLocation {
  public JarOutputLocation(File f) {
    super(f);
    assert OutputLocation.endsWithJar(f) : f + " is not a jarfile.";
  }

  protected final File directory() {
    return this.file.getParentFile();
  }
}
