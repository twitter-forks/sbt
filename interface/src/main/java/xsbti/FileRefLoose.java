package xsbti;

import java.io.File;

public final class FileRefLoose implements FileRef
{
  public final File classFile;

  public FileRefLoose(File classFile) {
    this.classFile = classFile;
  }

  public File containingFile() {
    return classFile;
  }

  /** NB: Used in serialization of this class. */
  @Override
  public String toString() {
    return FileRefs.LOOSE + "(" + classFile + ")";
  }

  @Override
  public boolean equals(Object o) {
    if (!(o instanceof FileRefLoose)) return false;
    FileRefLoose that = (FileRefLoose) o;
    if (!that.classFile.equals(this.classFile)) return false;
    return true;
  }

  @Override
  public int hashCode() {
    return classFile.hashCode();
  }
}
