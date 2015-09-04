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
  @Override
  public String toString() {
    return ClassRefs.JARRED + "(" + jarFile + "!" + classFile + ")";
  }

  @Override
  public boolean equals(Object o) {
    if (!(o instanceof ClassRefJarred)) return false;
    ClassRefJarred that = (ClassRefJarred) o;
    if (!that.jarFile.equals(this.jarFile)) return false;
    if (!that.classFile.equals(this.classFile)) return false;
    return true;
  }

  @Override
  public int hashCode() {
    int hash = 31;
    hash += this.jarFile.hashCode();
    hash += this.classFile.hashCode();
    return hash;
  }
}
