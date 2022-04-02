package org.wartremover;

import java.util.Optional;

public interface Position {
  int start();
  int end();
  SourceFile sourceFile();
  int startLine();
  int endLine();
  int startColumn();
  int endColumn();
  Optional<String> sourceCode();
}
