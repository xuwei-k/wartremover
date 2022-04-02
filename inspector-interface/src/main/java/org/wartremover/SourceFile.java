package org.wartremover;

import java.nio.file.Path;
import java.util.Optional;

public interface SourceFile {
  Optional<Path> getJPath();
  String name();
  String path();
  Optional<String> content();
}
