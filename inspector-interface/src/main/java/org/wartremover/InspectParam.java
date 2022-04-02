package org.wartremover;

import java.net.URL;

public interface InspectParam {
  String[] tastyFiles();
  String[] dependenciesClasspath();
  URL[] wartClasspath();
  String[] errorWarts();
  String[] warningWarts();
  boolean failIfWartLoadError();
}
