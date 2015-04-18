/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbti;

import java.io.File;
import java.io.IOException;

public final class DirectoryOutputLocation extends OutputLocation {
  public DirectoryOutputLocation(File f) {
    super(f);
    assert !OutputLocation.endsWithJar(f) : f + " is (probably) not a directory.";
  }

  protected final File directory() {
    return this.file;
  }
}
