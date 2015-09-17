package xsbti.compile;

import xsbti.FileRef;

// only includes changes to dependencies outside of the project
public interface DependencyChanges
{
	boolean isEmpty();
	// class files, possibly in jars
	FileRef[] modifiedBinaries();
	// class names
	String[] modifiedClasses();
}
