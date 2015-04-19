package xsbti.compile;

import java.io.File;
import java.net.URL;

// only includes changes to dependencies outside of the project
public interface DependencyChanges
{
	boolean isEmpty();
	// class files, possibly in jars
	URL[] modifiedBinaries();
	// class names
	String[] modifiedClasses();
}
