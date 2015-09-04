package xsbti;

import java.io.File;

public final class ClassRefJarred implements ClassRef
{
  public final File jarFile;
  public final String classFile;

  public ClassRefJarred(File jarFile, String classFile) {
    this.jarFile = jarFile;
    this.classFile = classFile;
  }

  public File containingFile() {
    return jarFile;
  }

  /** NB: Used in serialization of this class. */
  public String toString() {
    return ClassRefs.JARRED + "(" + jarFile + "!" + classFile + ")";
  }
}
