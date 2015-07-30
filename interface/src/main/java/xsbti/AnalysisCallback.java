/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
package xsbti;

import java.io.File;
import java.net.URL;

public interface AnalysisCallback
{
	/** Called to indicate that the source file <code>source</code> depends on the source file
	* <code>dependsOn</code>.  Note that only source files included in the current compilation will
	* passed to this method.  Dependencies on classes generated by sources not in the current compilation will
	* be passed as class dependencies to the classDependency method.
	* If <code>publicInherited</code> is true, this dependency is a result of inheritance by a
	* template accessible outside of the source file.
	* @deprecated Use `sourceDependency(File dependsOn, File source, DependencyContext context)` instead. */
	@Deprecated
	void sourceDependency(File dependsOn, File source, boolean publicInherited);
	/** Called to indicate that the source file <code>source</code> depends on the source file
	* <code>dependsOn</code>.  Note that only source files included in the current compilation will
	* passed to this method.  Dependencies on classes generated by sources not in the current compilation will
	* be passed as class dependencies to the classDependency method.
	* <code>context</code> gives information about the context in which this dependency has been extracted.
	* See xsbti.DependencyContext for the list of existing dependency contexts. */
	void sourceDependency(File dependsOn, File source, DependencyContext context);
	/** Called to indicate that the source file <code>source</code> depends on the top-level
	* class named <code>name</code> from class or jar file <code>binary</code>.
	* If <code>publicInherited</code> is true, this dependency is a result of inheritance by a
	* template accessible outside of the source file.
	* @deprecated Use `binaryDependency(URL binary, String name, File source, DependencyContext context)` instead. */
	@Deprecated
	void binaryDependency(URL binary, String name, File source, boolean publicInherited);
	/** Called to indicate that the source file <code>source</code> depends on the top-level
	* class named <code>name</code> from class or jar file <code>binary</code>.
	* <code>context</code> gives information about the context in which this dependency has been extracted.
	* See xsbti.DependencyContext for the list of existing dependency contexts. */
	void binaryDependency(URL binary, String name, File source, DependencyContext context);
	/** Called to indicate that the source file <code>source</code> produces a class file at
	* <code>module</code> contain class <code>name</code>.*/
	void generatedClass(File source, URL module, String name);
	/** Called when the public API of a source file is extracted. */
	void api(File sourceFile, xsbti.api.SourceAPI source);
	void usedName(File sourceFile, String names);
	/** Provides problems discovered during compilation.  These may be reported (logged) or unreported.
	* Unreported problems are usually unreported because reporting was not enabled via a command line switch. */
	void problem(String what, Position pos, String msg, Severity severity, boolean reported);
	/**
	 * Determines whether method calls through this interface should be interpreted as serving
	 * name hashing algorithm needs in given compiler run.
	 *
	 * In particular, it indicates whether member reference and inheritance dependencies should be
	 * extracted.
	 *
	 * As the signature suggests, this method's implementation is meant to be side-effect free. It's added
	 * to AnalysisCallback because it indicates how other callback calls should be interpreted by both
	 * implementation of AnalysisCallback and it's clients.
	 *
	 * NOTE: This method is an implementation detail and can be removed at any point without deprecation.
	 *       Do not depend on it, please.
	 */
	boolean nameHashing();
}
