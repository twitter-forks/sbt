package xsbti.compile;

import xsbti.ClassRef;

// only includes changes to dependencies outside of the project
public interface DependencyChanges
{
	boolean isEmpty();
	// class files, possibly in jars
	ClassRef[] modifiedBinaries();
	// class names
	String[] modifiedClasses();
}
