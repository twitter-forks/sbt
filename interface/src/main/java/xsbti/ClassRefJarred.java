package xsbti;

import java.io.File;

public interface ClassRefJarred extends ClassRef
{
  File jarFile();
  String classFile();
}
