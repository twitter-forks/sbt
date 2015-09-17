package xsbti;

import java.io.File;

public final class FileRefJarred implements FileRef
{
  public final File jarFile;
  public final String classFile;

  public FileRefJarred(File jarFile, String classFile) {
    this.jarFile = jarFile;
    this.classFile = classFile;
  }

  public File containingFile() {
    return jarFile;
  }

  /** NB: Used in serialization of this class. */
  @Override
  public String toString() {
    return FileRefs.JARRED + "(" + jarFile + "!" + classFile + ")";
  }

  @Override
  public boolean equals(Object o) {
    if (!(o instanceof FileRefJarred)) return false;
    FileRefJarred that = (FileRefJarred) o;
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
