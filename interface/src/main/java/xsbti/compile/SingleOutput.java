package xsbti.compile;

import java.io.File;

public interface SingleOutput extends Output {

	/** The directory or jar where class files should be generated.
	* Incremental compilation will manage the class files in this directory/jar.
	* In particular, outdated class files will be deleted before compilation.
	* It is important that this output location is exclusively used for one set of sources. */
	File outputLocation();
}
