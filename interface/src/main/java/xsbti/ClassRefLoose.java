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
}
