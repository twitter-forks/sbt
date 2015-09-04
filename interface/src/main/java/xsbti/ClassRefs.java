package xsbti;

import java.io.File;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class ClassRefs
{
  static final String JARRED = "crj";
  static final String LOOSE = "crl";

  private static final Pattern LOOSE_PATTERN = Pattern.compile(LOOSE + "\\(([^)]*)\\)");
  private static final Pattern JARRED_PATTERN = Pattern.compile(JARRED + "\\(([^!]*)!([^)]*)\\)");

  public static ClassRef fromString(String s) throws IOException {
    Matcher jMatcher = JARRED_PATTERN.matcher(s);
    if (jMatcher.matches()) {
      return new ClassRefJarred(new File(jMatcher.group(1)), jMatcher.group(2));
    }
    Matcher lMatcher = LOOSE_PATTERN.matcher(s);
    if (lMatcher.matches()) {
      return new ClassRefLoose(new File(lMatcher.group(1)));
    }
    throw new IOException("Unsupported ClassRef string: " + s);
  }
}
