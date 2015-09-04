package xsbti;

import java.io.File;

public final class ClassRefLoose implements ClassRef
{
  public final File classFile;

  public ClassRefLoose(File classFile) {
    this.classFile = classFile;
  }

  public File containingFile() {
    return classFile;
  }

  /** NB: Used in serialization of this class. */
  @Override
  public String toString() {
    return ClassRefs.LOOSE + "(" + classFile + ")";
  }

  @Override
  public boolean equals(Object o) {
    if (!(o instanceof ClassRefLoose)) return false;
    ClassRefLoose that = (ClassRefLoose) o;
    if (!that.classFile.equals(this.classFile)) return false;
    return true;
  }

  @Override
  public int hashCode() {
    return classFile.hashCode();
  }
}
