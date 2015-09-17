package xsbti.compile;

import xsbti.FileRef;
import xsbti.Maybe;

/**
* Determines if an entry on a classpath contains a class.
*/
public interface DefinesClass
{
 	/**
	* Returns a classpath entry for the requested class.
	*/
	Maybe<FileRef> apply(String className);
}
